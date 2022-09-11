//see LICENSE for license
//authors: Joonho Whangbo

package mempress

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.config._

class MemReqInternal()(implicit val p: Parameters) extends Bundle 
    with HasCoreParameters 
    with MemoryOpConstants {
  val addr = UInt(coreMaxAddrBits.W)
  val tag  = UInt(coreParams.dcacheReqTagBits.W)
  val cmd  = UInt(M_SZ.W)
  val size = UInt(log2Ceil(coreDataBytes + 1).W)
  val data = UInt(64.W)
}

class CtrlModuleIO()(implicit val p: Parameters) extends Bundle
    with HasCoreParameters 
    with MemoryOpConstants {
  val rocc_in = Flipped(Decoupled(new RoCCCommand))
  val rocc_out = Decoupled(new RoCCResponse)
  val busy = Output(Bool())

  val dmem_req = Decoupled(new MemReqInternal)
  val dmem_resp = Flipped(Valid(new HellaCacheResp))

// val sfence = Output(Bool())
// val dmem_req_val      = Output(Bool())
// val dmem_req_rdy      = Input(Bool())
// val dmem_req_addr     = Output(Bits(coreMaxAddrBits.W))
// val dmem_req_tag      = Output(Bits(coreParams.dcacheReqTagBits.W))
// val dmem_req_cmd      = Output(Bits(M_SZ.W))
// val dmem_req_size     = Output(Bits(log2Ceil(coreDataBytes + 1).W))

// val dmem_resp_val     = Input(Bool())
// val dmem_resp_tag     = Input(Bits(7.W))
// val dmem_resp_data    = Input(Bits(64.W))
}

class CtrlModule()(implicit val p: Parameters) extends Module
    with HasCoreParameters 
    with MemoryOpConstants {
  val io = IO(new CtrlModuleIO)

  val max_streams = p(MemPressMaxStreams)

  val ctrl_idle :: ctrl_getinst :: ctrl_access :: ctrl_accesspend :: Nil = Enum(4)
  val ctrl_state = RegInit(ctrl_idle.asUInt)

  // we don't need responses
  io.rocc_out.valid := false.B
  io.rocc_out.bits.rd := 0.U
  io.rocc_out.bits.data := 0.U

  val busy = RegInit(false.B)
  io.busy := busy

  val stream_rd :: stream_wr :: Nil = Enum(2)

  val stream_cnt     = RegInit(0.U(log2Ceil(max_streams+1).W)) // number of streams
  val rec_stream_cnt = RegInit(0.U(log2Up(max_streams+1).W)) // streams received
  val stream_type    = RegInit(stream_rd.asUInt)                  // stream type
  val max_reqs       = RegInit(0.U(64.W))                  // max reqs sent per stream
  val stride         = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))
  val start_addr     = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))

  val dmem_resp_val_reg = RegNext(io.dmem_resp.valid)
  val dmem_resp_tag_reg = RegNext(io.dmem_resp.bits.tag)

  io.dmem_req.valid := false.B
  io.dmem_req.bits.addr := 0.U
  io.dmem_req.bits.tag := 0.U
  io.dmem_req.bits.cmd := M_XWR
  io.dmem_req.bits.size := 0.U
  io.dmem_req.bits.data := 0.U

  io.rocc_in.ready := !busy

  switch (ctrl_state) {
    is (ctrl_idle.asUInt) {
      printf(p"ctrl_idle state\n")

      when (io.rocc_in.fire) {
        when (io.rocc_in.bits.inst.funct === 0.U) {
          stream_cnt := io.rocc_in.bits.rs1

          when (io.rocc_in.bits.rs2 === 0.U) {
            stream_type := stream_rd.asUInt
          }.otherwise {
            stream_type := stream_wr.asUInt
          }

          assert(stream_cnt < max_streams.U) // can we use assertions like this?
          printf(p"stream_cnt: ${io.rocc_in.bits.rs1} stream_type ${io.rocc_in.bits.rs2}\n")
        }.elsewhen (io.rocc_in.bits.inst.funct === 1.U) {
          max_reqs := io.rocc_in.bits.rs1
          printf(p"max_reqs: ${io.rocc_in.bits.rs1}\n")
        }.elsewhen (io.rocc_in.bits.inst.funct === 2.U) {
          rec_stream_cnt := rec_stream_cnt + 1.U

          stride(rec_stream_cnt)     := io.rocc_in.bits.rs1
          start_addr(rec_stream_cnt) := io.rocc_in.bits.rs2

          when (rec_stream_cnt === stream_cnt - 1.U) {
            busy := true.B
            ctrl_state := ctrl_access.asUInt
          }
          printf(p"stride: ${io.rocc_in.bits.rs1} start_addr: ${io.rocc_in.bits.rs2}\n")
        }
      }
    }
    is (ctrl_access.asUInt) {
      printf(p"ctrl_access state\n")

      io.dmem_req.valid := true.B
      io.dmem_req.bits.addr := start_addr(0.U) + stride(0.U)
      io.dmem_req.bits.tag := 0.U
      io.dmem_req.bits.cmd := M_XWR
      io.dmem_req.bits.size := log2Ceil(8).U
      io.dmem_req.bits.data := stride(0.U)

      when (io.dmem_req.ready) {
        ctrl_state := ctrl_accesspend.asUInt
      }
    }
    is (ctrl_accesspend.asUInt) {
      printf(p"ctrl_accesspend state\n")

      io.dmem_req.valid := false.B

      when (dmem_resp_val_reg) {
        busy := false.B
        ctrl_state := ctrl_idle.asUInt
      }
    }
  }

  printf(p"$busy")
}
