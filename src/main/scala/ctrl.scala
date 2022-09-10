//see LICENSE for license
//authors: Joonho Whangbo
package mempress

import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.config._

class CtrlModuleIO()(implicit val p: Parameters) extends Bundle
  with HasCoreParameters 
  with MemoryOpConstants {
  val rocc_in = Flipped(Decoupled(new RoCCCommand))
  val rocc_out = Decoupled(new RoCCResponse)
  val busy = Output(Bool())
// val sfence = Output(Bool())
    val dmem_req_val      = Bool(OUTPUT)
    val dmem_req_rdy      = Bool(INPUT)
    val dmem_req_tag      = Bits(OUTPUT, coreParams.dcacheReqTagBits)
    val dmem_req_addr     = Bits(OUTPUT, coreMaxAddrBits)
    val dmem_req_cmd      = Bits(OUTPUT, M_SZ)
    val dmem_req_size     = Bits(OUTPUT, log2Ceil(coreDataBytes + 1))

    val dmem_resp_val     = Bool(INPUT)
    val dmem_resp_tag     = Bits(INPUT, 7)
    val dmem_resp_data    = Bits(INPUT, 64)
}

class CtrlModule()(implicit val p: Parameters) extends Module
    with HasCoreParameters 
    with MemoryOpConstants {

  val io = IO(new CtrlModuleIO)

  val ctrl_idle :: ctrl_getinst :: ctrl_write :: ctrl_writepend :: Nil = Enum(UInt(), 4)
  val ctrl_state = RegInit(ctrl_idle)

  // we don't need responses
  io.rocc_out.valid := false.B
  io.rocc_out.bits.rd := 0.U
  io.rocc_out.bits.data := 0.U

  val busy = RegInit(false.B)
  io.busy := busy

  val rs1_val = RegInit(0.U(64.W))
  val rs2_val = RegInit(0.U(64.W))

  val dmem_resp_val_reg = RegNext(io.dmem_resp_val)
  val dmem_resp_tag_reg = RegNext(io.dmem_resp_tag)

  switch (ctrl_state) {
    is (ctrl_idle) {
      printf(s"ctrl_idle state")

      io.rocc_in.ready := !busy

      when (io.rocc_in.fire) {
        rs1_val := io.rocc_in.bits.rs1
        rs2_val := io.rocc_in.bits.rs2

        busy := true.B
        ctrl_state := ctrl_write

        when (io.rocc_in.bits.inst.funct === UInt(0)) {
          printf(s"rs1: $rs1_val rs2 $rs2_val\n")
        }
      }
    }
    is (ctrl_write) {
      printf(s"ctrl_write state")

      io.dmem_req_val := true.B
      io.dmem_req_tag := 0.U
      io.dmem_req_addr := rs1_val
      io.dmem_req_cmd := M_XWR
      io.dmem_req_size := log2Ceil(8).U

      when (io.dmem_req_rdy) {
        ctrl_state := ctrl_writepend
      }
    }
    is (ctrl_writepend) {
      printf(s"ctrl_writepend state")

      io.dmem_req_val := false.B

      when (dmem_resp_val_reg) {
        busy := false.B
        ctrl_state := ctrl_idle
      }
    }
  }

  printf(s"$busy")
}
