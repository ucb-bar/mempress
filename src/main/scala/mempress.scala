//see LICENSE for license
//authors: Joonho Whangbo

package mempress

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}


case class MemPressParams(
  maxStreams: Int = 16,
  arbQueueDepth: Int = 32,
  maxOutstandingReqs: Int = 32,
  fiboLFSRBits: Int = 30,
  singleL2TL: Boolean = true
)

case object MemPressKey extends Field[Option[MemPressParams]](None)
case object MemPressTLB extends Field[Option[TLBConfig]](None)
case object MemPressPrintfEnable extends Field[Boolean](false)

trait HasMemPressParams {
  implicit val p: Parameters
  val memPressExternal = p(MemPressKey).get
  val maxStreams = memPressExternal.maxStreams
  val arbQueueDepth = memPressExternal.arbQueueDepth
  val maxOutstandingReqs = memPressExternal.maxOutstandingReqs
  val fiboLFSRBits = memPressExternal.fiboLFSRBits
  val singleL2TL = memPressExternal.singleL2TL
}

class MemPress(opcodes: OpcodeSet)(implicit p: Parameters) extends 
  LazyRoCC(
    opcodes=opcodes, 
    nPTWPorts = if (p(MemPressKey).isDefined) p(MemPressKey).get.maxStreams else 0)
  with HasMemPressParams
{

  override lazy val module = new MemPressImp(this)

  val l2helper_cnt = if (singleL2TL) 1 else maxStreams

  val l2helper = (0 until l2helper_cnt).map{ x =>
                  val y = LazyModule(new L2MemHelper(s"stream[${x}]", 
                                     numOutstandingReqs=maxOutstandingReqs, 
                                     queueResponses=true, 
                                     queueRequests=true)) 
                  tlNode := y.masterNode
                  y
                }
}

class MemPressImp(outer: MemPress)(implicit p: Parameters) 
extends LazyRoCCModuleImp(outer) with HasMemPressParams
{
  chisel3.dontTouch(io)

  val idxWidth = log2Ceil(maxStreams)
  val l2helper_cnt = if (singleL2TL) 1 else maxStreams

  // tie up some wires
  io.mem.req.valid := false.B
  io.mem.s1_kill := false.B
  io.mem.s2_kill := false.B
  io.mem.keep_clock_enabled := true.B
  io.interrupt := false.B

  val ctrl = Module(new CtrlModule(maxStreams, idxWidth, singleL2TL))
  ctrl.io.rocc_in <> io.cmd
  io.resp <> ctrl.io.rocc_out
  io.cmd.ready := ctrl.io.rocc_in.ready
  io.busy := ctrl.io.busy

  val reqgen = Module(new ReqGen(maxStreams, idxWidth, fiboLFSRBits))
  reqgen.io.send_reqs := ctrl.io.send_reqs
  ctrl.io.sent_done := reqgen.io.sent_done
  ctrl.io.req_fire := reqgen.io.req_fire
  reqgen.io.global_stream_info <> ctrl.io.global_stream_info
  reqgen.io.local_stream_info <> ctrl.io.local_stream_info

  val arb = Module(new MemArbiter(maxStreams, idxWidth, arbQueueDepth))
  arb.io.req_in <> reqgen.io.req

  val status = RegEnable(io.cmd.bits.status, io.cmd.fire)

  val chosen = arb.io.req_out.bits.idx
  arb.io.req_out.ready := false.B

  for (i <- 0 until l2helper_cnt) {
    if (singleL2TL) {
      outer.l2helper(i).module.io.userif.req.valid := arb.io.req_out.valid
      arb.io.req_out.ready := outer.l2helper(i).module.io.userif.req.ready
    } else {
      when (i.U === chosen) {
        outer.l2helper(i).module.io.userif.req.valid := arb.io.req_out.valid
        arb.io.req_out.ready := outer.l2helper(i).module.io.userif.req.ready
      }.otherwise {
        outer.l2helper(i).module.io.userif.req.valid := false.B
      }
    }
    ctrl.io.dmem_resp(i) <> outer.l2helper(i).module.io.userif.resp
    outer.l2helper(i).module.io.userif.req.bits := arb.io.req_out.bits.data
    outer.l2helper(i).module.io.sfence <> ctrl.io.sfence_out
    outer.l2helper(i).module.io.status.valid := ctrl.io.dmem_status_out.valid
    outer.l2helper(i).module.io.status.bits := ctrl.io.dmem_status_out.bits.status
    io.ptw(i) <> outer.l2helper(i).module.io.ptw
  }
}

class WithMemPress extends Config ((site, here, up) => {
  case MemPressTLB => Some(TLBConfig(nSets = 4, nWays = 4, nSectors = 1, nSuperpageEntries = 1))
  case MemPressKey => Some(MemPressParams(
    maxStreams = 16,
    arbQueueDepth = 32,
    maxOutstandingReqs = 32,
    fiboLFSRBits = 30,
    singleL2TL = true
  ))
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val mempress = LazyModule.apply(new MemPress(OpcodeSet.custom2)(p))
      mempress
    }
  )
})
