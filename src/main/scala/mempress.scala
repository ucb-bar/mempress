//see LICENSE for license
//authors: Joonho Whangbo

package mempress

import Chisel._
import chisel3.util.{HasBlackBoxResource}
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBConfig, HellaCacheReq}

// case object Sha3WidthP extends Field[Int]
// case object Sha3Stages extends Field[Int]
// case object Sha3FastMem extends Field[Boolean]
// case object Sha3BufferSram extends Field[Boolean]
// case object Sha3Keccak extends Field[Boolean]
// case object Sha3PrintfEnable extends Field[Boolean](false)

class WrapBundle(nPTWPorts: Int)(implicit p: Parameters) extends Bundle {
  val io = new RoCCIO(nPTWPorts)
  val clock = Clock(INPUT)
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

  io.resp.valid := Bool(false) 

  val ctrl = Module(new CtrlModule)
  ctrl.io.rocc_in <> io.cmd

// ctrl.io.rocc_req_val   <> io.cmd.valid
// io.cmd.ready := ctrl.io.rocc_req_rdy
// ctrl.io.rocc_funct     <> io.cmd.bits.inst.funct
// ctrl.io.rocc_rs1       <> io.cmd.bits.rs1
// ctrl.io.rocc_rs2       <> io.cmd.bits.rs2
// ctrl.io.rocc_rd        <> io.cmd.bits.inst.rd
// io.busy := ctrl.io.busy

  val status = RegEnable(io.cmd.bits.status, io.cmd.fire())
  val dmem_data = Wire(Bits())
  def dmem_ctrl(req: DecoupledIO[HellaCacheReq]) {
    req.valid := ctrl.io.dmem_req_val
    ctrl.io.dmem_req_rdy := req.ready
    req.bits.tag := ctrl.io.dmem_req_tag
    req.bits.addr := ctrl.io.dmem_req_addr
    req.bits.cmd := ctrl.io.dmem_req_cmd
    req.bits.size := ctrl.io.dmem_req_size
    req.bits.data := dmem_data
    req.bits.signed := Bool(false)
    req.bits.dprv := status.dprv
    req.bits.dv := status.dv
    req.bits.phys := Bool(false)
  }

  outer.dmemOpt match {
    case Some(m) => {
      val dmem = m.module
      dmem_ctrl(dmem.io.req)
      io.mem.req <> dmem.io.mem
      io.ptw.head <> dmem.io.ptw

      dmem.io.status := status
      dmem.io.sfence := ctrl.io.sfence
    }
    case None => dmem_ctrl(io.mem.req)
  }

// ctrl.io.dmem_resp_val  <> io.mem.resp.valid
// ctrl.io.dmem_resp_tag  <> io.mem.resp.bits.tag
// ctrl.io.dmem_resp_data := io.mem.resp.bits.data

// val dpath = Module(new DpathModule(W,S)(p))

// dpath.io.message_in <> ctrl.io.buffer_out
// dmem_data := dpath.io.hash_out(ctrl.io.windex)

  //ctrl.io <> dpath.io
// dpath.io.absorb := ctrl.io.absorb
// dpath.io.init := ctrl.io.init
// dpath.io.write := ctrl.io.write
// dpath.io.round := ctrl.io.round
// dpath.io.stage := ctrl.io.stage
// dpath.io.aindex := ctrl.io.aindex
}

class WithMemPress extends Config ((site, here, up) => {
// case Sha3WidthP => 64
// case Sha3Stages => 1
// case Sha3FastMem => true
// case Sha3BufferSram => false
// case Sha3Keccak => false
// case Sha3BlackBox => false
  case MemPressTLB => Some(TLBConfig(nSets = 1, nWays = 4, nSectors = 1, nSuperpageEntries = 1))
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val mempress = LazyModule.apply(new MemPress(OpcodeSet.custom2)(p))
      mempress
    }
  )
})

// class WithSha3Printf extends Config((site, here, up) => {
// case Sha3PrintfEnable => true
// })
