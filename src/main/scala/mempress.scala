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
case object MemPressReqQueDepth extends Field[Int]

class WrapBundle(nPTWPorts: Int)(implicit p: Parameters) extends Bundle {
  val io = new RoCCIO(nPTWPorts)
  val clock = Input(Clock())
  val reset = Input(UInt(1.W))
}

class MemPress(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(
    opcodes = opcodes, nPTWPorts = if (p(MemPressTLB).isDefined) 1 else 0) {

  override lazy val module = new MemPressImp(this)

  val dmemOpt = p(MemPressTLB).map { _ =>
    val dmem = LazyModule(new DmemModule)
    tlNode := dmem.node
    dmem
  }
}

class MemPressImp(outer: MemPress)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  chisel3.dontTouch(io)

  val ctrl = Module(new CtrlModule)
  ctrl.io.rocc_in <> io.cmd
  io.resp <> ctrl.io.rocc_out
  ctrl.io.dmem_resp <> io.mem.resp

  io.cmd.ready := ctrl.io.rocc_in.ready
  io.busy := ctrl.io.busy
  io.interrupt := false.B

  val arb = Module(new MemArbiter)

  arb.io.req_in <> ctrl.io.dmem_req
  arb.io.idx := 1.U

  val status = RegEnable(io.cmd.bits.status, io.cmd.fire)
  def dmem_ctrl(req: DecoupledIO[HellaCacheReq]) {
// req.valid := ctrl.io.dmem_req.valid
// ctrl.io.dmem_req.ready := req.ready
// req <> ctrl.io.dmem_req
// req.bits.addr := ctrl.io.dmem_req.bits.addr
// req.bits.tag := ctrl.io.dmem_req.bits.tag
// req.bits.cmd := ctrl.io.dmem_req.bits.cmd
// req.bits.size := ctrl.io.dmem_req.bits.size
// req.bits.data := ctrl.io.dmem_req.bits.data
    req <> arb.io.req_out
    req.bits.signed := false.B
    req.bits.dprv := status.dprv
    req.bits.dv := status.dv
    req.bits.phys := false.B
  }

  outer.dmemOpt match {
    case Some(m) => {
      val dmem = m.module
      dmem_ctrl(dmem.io.req)
      io.mem.req <> dmem.io.mem
      io.ptw.head <> dmem.io.ptw

      dmem.io.status := status
      dmem.io.sfence := false.B
    }
    case None => dmem_ctrl(io.mem.req)
  }

}

class WithMemPress extends Config ((site, here, up) => {
  case MemPressTLB => Some(TLBConfig(nSets = 1, nWays = 4, nSectors = 1, nSuperpageEntries = 1))
  case MemPressMaxStreams => 4
  case MemPressReqQueDepth => 8
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val mempress = LazyModule.apply(new MemPress(OpcodeSet.custom2)(p))
      mempress
    }
  )
})
