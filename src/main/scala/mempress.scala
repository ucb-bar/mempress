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

case object MemPressMaxStreams extends Field[Int]
case object MemPressArbQueDepth extends Field[Int]
case object MemPressTLDepth extends Field[Int]
case object MemPressMaxOutstandingReqs extends Field[Int]
case object MemPressPrintfEnable extends Field[Boolean](false)

class MemPress(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(
    opcodes = opcodes, nPTWPorts = if (p(MemPressTLB).isDefined) p(MemPressMaxStreams) else 0) {

  override lazy val module = new MemPressImp(this)

  val max_streams = p(MemPressMaxStreams)
  val max_outstand_reqs = p(MemPressMaxOutstandingReqs)
  val l2helper = (0 until max_streams).map{ x =>
    val y = LazyModule(new L2MemHelper(s"stream[${x}]", 
                                       numOutstandingReqs=max_outstand_reqs, 
                                       queueResponses=true, 
                                       queueRequests=true)) 
    tlNode := y.masterNode
    y
  }
}

class MemPressImp(outer: MemPress)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  chisel3.dontTouch(io)

  val max_streams = p(MemPressMaxStreams)

  // tie up some wires
  io.mem.req.valid := false.B
  io.mem.s1_kill := false.B
  io.mem.s2_kill := false.B
  io.mem.keep_clock_enabled := true.B
  io.interrupt := false.B

  val ctrl = Module(new CtrlModule())
  ctrl.io.rocc_in <> io.cmd
  io.resp <> ctrl.io.rocc_out
  io.cmd.ready := ctrl.io.rocc_in.ready
  io.busy := ctrl.io.busy

  val arb = Module(new MemArbiter)
  arb.io.req_in <> ctrl.io.dmem_req
  arb.io.idx := ctrl.io.dmem_req_idx

  val status = RegEnable(io.cmd.bits.status, io.cmd.fire)

  arb.io.req_out.ready := false.B
  for (i <- 0 until max_streams) {
    when (i.U === arb.io.chosen) {
      outer.l2helper(i).module.io.userif.req.valid := arb.io.req_out.valid
      arb.io.req_out.ready := outer.l2helper(i).module.io.userif.req.ready
    }.otherwise {
      outer.l2helper(i).module.io.userif.req.valid := false.B
    }
    ctrl.io.dmem_resp(i) <> outer.l2helper(i).module.io.userif.resp
    outer.l2helper(i).module.io.userif.req.bits := arb.io.req_out.bits
    outer.l2helper(i).module.io.sfence <> ctrl.io.sfence_out
    outer.l2helper(i).module.io.status.valid := ctrl.io.dmem_status_out.valid
    outer.l2helper(i).module.io.status.bits := ctrl.io.dmem_status_out.bits.status
    io.ptw(i) <> outer.l2helper(i).module.io.ptw
  }
}

class WithMemPress extends Config ((site, here, up) => {
  case MemPressTLB => Some(TLBConfig(nSets = 4, nWays = 4, nSectors = 1, nSuperpageEntries = 1))
  case MemPressMaxStreams => 32
  case MemPressArbQueDepth => 8
  case MemPressMaxOutstandingReqs => 8
  case MemPressPrintfEnable => false
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val mempress = LazyModule.apply(new MemPress(OpcodeSet.custom2)(p))
      mempress
    }
  )
})
