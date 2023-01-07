package midas
package models

// NOTE: This LLC model is *very* crude model of a cache that simple forwards
// misses onto the DRAM model, while short-circuiting hits.
import junctions._

import midas.core._
import midas.widgets._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.{ParameterizedBundle, MaskGen, UIntToOH1}
import freechips.rocketchip.util.DecoupledHelper

import chisel3._
import chisel3.util._

import scala.math.min
import Console.{UNDERLINED, RESET}

import java.io.{File, FileWriter}

// State to track reads to DRAM, ~loosely an MSHR
class MSHR(llcKey: LLCParams)(implicit p: Parameters) extends NastiBundle()(p) {
  val set_addr = UInt(llcKey.sets.maxBits.W)
  val xaction = new TransactionMetaData
  val wb_in_flight =  Bool()
  val acq_in_flight = Bool()
  val enabled       = Bool() // Set by a runtime configuration register

  def valid(): Bool = (wb_in_flight || acq_in_flight) && enabled
  def available(): Bool = !valid && enabled
  def setCollision(set_addr: UInt): Bool = (set_addr === this.set_addr) && valid

  // Call on a MSHR register; sets all pertinent fields (leaving enabled untouched)
  def allocate(
      new_xaction: TransactionMetaData,
      new_set_addr: UInt,
      do_acq: Bool,
      do_wb: Bool = false.B)(implicit p: Parameters): Unit = {
    set_addr := new_set_addr
    wb_in_flight := do_wb
    acq_in_flight := do_acq
    xaction := new_xaction
  }
}

object MSHR {
  def apply(llcKey: LLCParams)(implicit p: Parameters): MSHR = {
    val w = Wire(new MSHR(llcKey))
    w.wb_in_flight := false.B
    w.acq_in_flight := false.B
    // Initialize to enabled to play nice with assertions
    w.enabled := true.B
    w.xaction := DontCare
    w.set_addr := DontCare
    w
  }
}

class BlockMetadata(tagBits: Int) extends Bundle {
  val tag = UInt(tagBits.W)
  val valid = Bool()
  val dirty = Bool()
}

class LLCProgrammableSettings(llcKey: LLCParams) extends Bundle
    with HasProgrammableRegisters with HasConsoleUtils {
  val wayBits     = Input(UInt(log2Ceil(llcKey.ways.maxBits).W))
  val setBits     = Input(UInt(log2Ceil(llcKey.sets.maxBits).W))
  val blockBits   = Input(UInt(log2Ceil(llcKey.blockBytes.maxBits).W))
  val activeMSHRs = Input(UInt(log2Ceil(llcKey.mshrs.max + 1).W))

  // Instrumentation
  val misses         = Output(UInt(32.W)) // Total accesses is provided by (totalReads + totalWrites)
  val writebacks     = Output(UInt(32.W)) // Number of dirty lines returned to DRAM
  val refills        = Output(UInt(32.W)) // Number of clean lines requested from DRAM
  val peakMSHRsUsed  = Output(UInt(log2Ceil(llcKey.mshrs.max+1).W)) // Peak number of MSHRs used
  // Note short-burst writes will produce a refill, whereas releases from caches will not

  val registers = Seq(
    wayBits     -> RuntimeSetting(llcKey.ways.maxBits, "Log2(ways per set)"),
    setBits     -> RuntimeSetting(llcKey.sets.maxBits, "Log2(sets per bank"),
    blockBits   -> RuntimeSetting(llcKey.blockBytes.maxBits, "Log2(cache-block bytes"),
    activeMSHRs -> RuntimeSetting(llcKey.mshrs.max, "Number of MSHRs", min = 1, max = Some(llcKey.mshrs.max))
  )

  def maskTag(addr: UInt): UInt = (addr >> (blockBits +& setBits))
  def maskSet(addr: UInt): UInt = ((addr >> blockBits) & ((1.U << setBits) - 1.U))(llcKey.sets.maxBits-1, 0)
  def regenPhysicalAddress(set_addr: UInt, tag_addr: UInt): UInt =
    (set_addr << (blockBits)) |
    (tag_addr << (blockBits +& setBits))

  def setLLCSettings(bytesPerBlock: Option[Int] = None): Unit = {
    Console.println(s"\n${UNDERLINED}Last-Level Cache Settings${RESET}")

    regMap(blockBits).set(log2Ceil(requestInput("Block size in bytes",
                                                default = llcKey.blockBytes.max,
                                                min     = Some(llcKey.blockBytes.min),
                                                max     = Some(llcKey.blockBytes.max))))
    regMap(setBits).set(log2Ceil(requestInput("Number of sets in LLC",
                                               default = llcKey.sets.max,
                                               min     = Some(llcKey.sets.min),
                                               max     = Some(llcKey.sets.max))))
    regMap(wayBits).set(log2Ceil(requestInput("Set associativity",
                                              default = llcKey.ways.max,
                                              min     = Some(llcKey.ways.min),
                                              max     = Some(llcKey.ways.max))))
  }
}

case class WRange(min: Int, max: Int) {
  def minBits: Int = log2Ceil(min)
  def maxBits: Int = log2Ceil(max)
}

case class LLCParams(
    ways: WRange       = WRange(1, 8),
    sets: WRange       = WRange(32, 4096),
    blockBytes: WRange = WRange(8, 128),
    mshrs: WRange      = WRange(1, 8)// TODO: check against AXI ID width
  ) {

  def maxTagBits(addrWidth: Int): Int = addrWidth - blockBytes.minBits - sets.minBits

  def print(): Unit = {
    println("  LLC Parameters:")
    println("    Sets:              " + sets)
    println("    Associativity:     " + ways)
    println("    Block Size (B):    " + blockBytes)
    println("    MSHRs:             " + mshrs)
    println("    Replacement Policy: Random\n")
  }
}

class LLCModelIO(val key: LLCParams)(implicit val p: Parameters) extends Bundle {
  val req = Flipped(new NastiReqChannels)
  val wResp = Decoupled(new WriteResponseMetaData) // to backend
  val rResp = Decoupled(new ReadResponseMetaData)
  val memReq = new NastiReqChannels                  // to backing DRAM model
  val memRResp = Flipped(Decoupled(new ReadResponseMetaData)) // from backing DRAM model
  val memWResp = Flipped(Decoupled(new WriteResponseMetaData))

  // NEW CONNECTIONS
  //val tNasti = Flipped(new NastiIO)
  val tNastiRChannelFire = Input(Bool())
  val tNastiBChannelFire = Input(Bool())
  val tNastiRChannel = Flipped(new NastiReadDataChannel)
  val tNastiBChannel = Flipped(new NastiWriteResponseChannel)
  val pendingReadsFull = Input(Bool())
  val pendingAWReqFull = Input(Bool())
  val pendingWReqFull = Input(Bool())
  // NEW CONNECTIONS

  // LLC runtime configuration
  val settings = new LLCProgrammableSettings(key)
}

class LLCModel(cfg: BaseConfig)(implicit p: Parameters) extends NastiModule()(p) {
  val llcKey = cfg.params.llcKey.get
  val io = IO(new LLCModelIO(llcKey))

  // ***** WRITE QUEUE V1 *****
  // Write queue recombines AW and W transactions before allowing LLC to issue a write response
  val awQueue = Module(new Queue(new WriteResponseMetaData, cfg.maxWrites, flow=true))
  val awQueueLen = SatUpDownCounter(cfg.maxWrites)
  awQueueLen.inc := awQueue.io.enq.fire
  awQueueLen.dec := awQueue.io.deq.fire
  val awQueueNotFull = ~awQueueLen.full
  val awEnqueueFire = DecoupledHelper( // TODO: is this necessary? same reason as below
    awQueue.io.enq.ready,
    io.req.aw.valid,
    awQueueNotFull
  )
  io.req.aw.ready := awEnqueueFire.fire(io.req.aw.valid)
  awQueue.io.enq.valid := awEnqueueFire.fire(awQueue.io.enq.ready)
  awQueue.io.enq.bits := WriteResponseMetaData(io.req.aw.bits)

  val memWReqs = SatUpDownCounter(cfg.maxWrites)
  io.req.w.ready := ~memWReqs.full // TODO: is this okay? -> not sure where valid/data is going, and whether to use DecoupledHelper
  val newWReq = ((memWReqs.value > awQueue.io.count) && io.req.aw.fire) ||
                ((memWReqs.value < awQueue.io.count) && memWReqs.inc) ||
                 (memWReqs.inc && io.req.aw.fire)
  memWReqs.inc := io.req.w.fire && io.req.w.bits.last
  memWReqs.dec := newWReq
  val numCompleteWriteReqs = SatUpDownCounter(cfg.maxWrites)
  numCompleteWriteReqs.inc := newWReq
  numCompleteWriteReqs.dec := io.wResp.fire
  val completeWriteReady = ~numCompleteWriteReqs.empty

  // must prevent duplicate ID's from being sent -> currently only services 1 write at a time
  val writeNotIssued = RegInit(true.B)
  when (io.wResp.fire) {
    assert(writeNotIssued === true.B, "ERROR: sending write request to backend when one has not been fully serviced.")
    writeNotIssued := false.B
  }
  when (io.tNastiBChannelFire) {
    assert(writeNotIssued === false.B, "ERROR: write response channels are firing but a read is not currently issued.")
    writeNotIssued := true.B
  }

  val awDequeueFire = DecoupledHelper(
    io.wResp.ready,
    awQueue.io.deq.valid,
    completeWriteReady,
    writeNotIssued
  )
  awQueue.io.deq.ready := awDequeueFire.fire(awQueue.io.deq.valid)
  io.wResp.valid := awDequeueFire.fire(io.wResp.ready)
  io.wResp.bits := awQueue.io.deq.bits

  assert(awQueue.io.enq.ready || !io.req.aw.fire,
    "AW queue in LLCModel would overflow")

  val currAWEnqueue = WriteResponseMetaData(io.req.aw.bits)
  when (awQueue.io.enq.fire) {
    printf("""AWQUEUE ENQUEUE FIRE BEGIN
id             : 0x%x
awQueueLength  : %d
maxWrites      : %d
AWQUEUE ENQUEUE FIRE END
""",
           currAWEnqueue.id,
           awQueueLen.value,
           cfg.maxWrites.U)
  }

  val currAWDequeue = awQueue.io.deq.bits
  when (awQueue.io.deq.fire) {
    printf("""AWQUEUE DEQUEUE FIRE BEGIN
id             : 0x%x
awQueueLength  : %d
maxWrites      : %d
AWQUEUE DEQUEUE FIRE END
""",
           currAWDequeue.id,
           awQueueLen.value,
           cfg.maxWrites.U)
  }
  // ***** WRITE QUEUE V1 *****

  // ***** READ QUEUE V2 *****
  val arQueue = Module(new Queue(new ReadResponseMetaData, cfg.maxReads, flow=true))
  val arQueueLen = SatUpDownCounter(cfg.maxReads)
  arQueueLen.inc := arQueue.io.enq.fire
  arQueueLen.dec := arQueue.io.deq.fire
  val arQueueNotFull = ~arQueueLen.full
  val arEnqueueFire = DecoupledHelper( // TODO: is this necessary? Not needed if arQueue.io.enq.ready is high only when queue is not full
    arQueue.io.enq.ready,
    io.req.ar.valid,
    arQueueNotFull
  ) 
  io.req.ar.ready := arEnqueueFire.fire(io.req.ar.valid)
  arQueue.io.enq.valid := arEnqueueFire.fire(arQueue.io.enq.ready)
  arQueue.io.enq.bits := ReadResponseMetaData(io.req.ar.bits)

  // must prevent duplicate ID's from being sent -> currently only services 1 read at a time
  val readNotIssued = RegInit(true.B)
  when (io.rResp.fire) {
    assert(readNotIssued === true.B, "ERROR: sending read request to backend when one has not been fully serviced.")
    readNotIssued := false.B
  }
  when (io.tNastiRChannelFire && io.tNastiRChannel.last) {
    assert(readNotIssued === false.B, "ERROR: read data channels are firing but a read is not currently issued.")
    readNotIssued := true.B
  }

  val arDequeueFire = DecoupledHelper(
    io.rResp.ready,
    arQueue.io.deq.valid,
    readNotIssued
  )
  arQueue.io.deq.ready := arDequeueFire.fire(arQueue.io.deq.valid)
  io.rResp.valid := arDequeueFire.fire(io.rResp.ready)
  io.rResp.bits := arQueue.io.deq.bits

  val currAREnqueue = ReadResponseMetaData(io.req.ar.bits)
  when (arQueue.io.enq.fire) {
    printf("""ARQUEUE ENQUEUE FIRE BEGIN
id             : 0x%x
len            : %d
arQueueLength  : %d
maxReads       : %d
ARQUEUE ENQUEUE FIRE END
""",
           currAREnqueue.id,
           currAREnqueue.len,
           arQueueLen.value,
           cfg.maxReads.U)
  }

  val currARDequeue = arQueue.io.deq.bits
  when (arQueue.io.deq.fire) {
    printf("""ARQUEUE DEQUEUE FIRE BEGIN
id             : 0x%x
len            : %d
arQueueLength  : %d
maxReads       : %d
ARQUEUE DEQUEUE FIRE END
""",
           currARDequeue.id,
           currARDequeue.len,
           arQueueLen.value,
           cfg.maxReads.U)
  }
  // ***** READ QUEUE V2 *****

  // READ QUEUE V1
  //val arQueue = Module(new Queue(new NastiReadAddressChannel, cfg.maxReads, flow = true))
  //val pendingReadsNotFull = ~io.pendingReadsFull

  // enqueue
  //val arReqFire = DecoupledHelper(
    //arQueue.io.enq.ready,
    //io.req.ar.valid,
    //pendingReadsNotFull
  //)
  //io.req.ar.ready := arReqFire.fire(io.req.ar.valid)
  //arQueue.io.enq.valid := arReqFire.fire(arQueue.io.enq.ready)
  //arQueue.io.enq.bits := io.req.ar.bits

  //when (io.req.ar.valid) {
    //printf("AR VALID: address 0x%x is valid but not necessarily firing\n", io.req.ar.bits.addr)
  //}

  //when (io.req.ar.fire) {
    //printf("AR FIRE: adding address 0x%x to arQueue\n", io.req.ar.bits.addr)
  //}

  // dequeue
  //arQueue.io.deq.ready := io.rResp.ready
  //io.rResp.valid := arQueue.io.deq.valid
  //io.rResp.bits := ReadResponseMetaData(arQueue.io.deq.bits)

  //when (io.rResp.fire) {
    //printf("releasing address 0x%x to backend\n", arQueue.io.deq.bits.addr)
  //}
  // READ QUEUE V1

  //val arQueue = Module(new Queue(new NastiReadAddressChannel, cfg.maxReads, flow = true))
  //arQueue.io.enq.bits := io.req.ar.bits
  //arQueue.io.enq.valid := io.req.ar.fire

  //arQueue.io.deq.ready := io.rResp.ready
  //io.rResp.valid := arQueue.io.deq.valid
  //io.rResp.bits := ReadResponseMetaData(arQueue.io.deq.bits)

  // Ready to accept new reads if number of pending reads has not exceeded maximum
  //val pendingReadsNotFull = ~io.pendingReadsFull
  //val rRespFire = DecoupledHelper(
    //io.rResp.ready,
    //reads.valid,
    //pendingReadsNotFull
  //)

  //val reads = Queue(io.req.ar)
  //reads.ready := rRespFire.fire(reads.valid)
  //io.rResp.valid := rRespFire.fire(io.rResp.ready)
  //io.rResp.bits := ReadResponseMetaData(reads.bits)

  //reads.ready := io.rResp.ready && (~io.pendingReadsFull)
  //io.rResp.valid := reads.valid
  // ***** READ HIT WRITE WAIT V1 *****
}





