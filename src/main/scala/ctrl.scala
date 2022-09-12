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
  val dmem_req_idx = Output(UInt(log2Ceil(p(MemPressMaxStreams)).W))
  val dmem_resp = Flipped(Valid(new HellaCacheResp))
}

class CtrlModule()(implicit val p: Parameters) extends Module
    with HasCoreParameters 
    with MemoryOpConstants {
  val io = IO(new CtrlModuleIO)

  printf(s"${coreParams.dcacheReqTagBits}")

  val max_streams = p(MemPressMaxStreams)
  println(s"p(MemPressMaxStreams) : ${max_streams}")

  val ctrl_idle :: ctrl_getinst :: ctrl_access :: ctrl_accesspend :: Nil = Enum(4)
  val ctrl_state = RegInit(ctrl_idle.asUInt)

  // we don't need responses
  io.rocc_out.valid := false.B
  io.rocc_out.bits.rd := 0.U
  io.rocc_out.bits.data := 0.U

  val busy = RegInit(false.B)
  io.busy := busy

  val stream_rd :: stream_wr :: Nil = Enum(2)

  val stream_cnt     = RegInit(0.U(log2Ceil(max_streams + 1).W))   // number of streams
  val rec_stream_cnt = RegInit(0.U(log2Ceil(max_streams + 1).W))   // streams received
  val stream_type    = RegInit(stream_rd.asUInt)                   // stream type
  val max_reqs       = RegInit(0.U(64.W))                          // max reqs sent per stream
  val stride         = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W)))) // bytes to skip
  val start_addr     = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))

  val dmem_resp_val_reg = RegNext(io.dmem_resp.valid)
  val dmem_resp_tag_reg = RegNext(io.dmem_resp.bits.tag)

  val s_idx       = RegInit(0.U(log2Ceil(max_streams + 1).W))
  val s_sent      = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))
  val s_sent_done = RegInit(VecInit(Seq.fill(max_streams)(false.B)))
  val s_rec       = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))
  val s_rec_done  = RegInit(VecInit(Seq.fill(max_streams)(false.B)))

  io.dmem_req_idx := s_idx
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

          assert(stream_cnt < max_streams.U)
          printf(p"stream_cnt: ${io.rocc_in.bits.rs1} stream_type ${io.rocc_in.bits.rs2}\n")
        }.elsewhen (io.rocc_in.bits.inst.funct === 1.U) {
          max_reqs := io.rocc_in.bits.rs1
          printf(p"max_reqs: ${io.rocc_in.bits.rs1}\n")
// assert(((io.rocc_in.bits.rs1 & (io.rocc_in.bits.rs1 - 1.U)) === 0.U))
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
      val addr = start_addr(s_idx) + stride(s_idx) * s_sent(s_idx)
      val data = stride(s_idx) - 1.U
      printf(p"ctrl_access state dmem_req.fire: ${io.dmem_req.fire} s_idx: ${s_idx} s_sent: ${s_sent(s_idx)} s_rec: ${s_rec(s_idx)} addr: ${addr} data: ${data}\n")

      io.dmem_req.valid     := !s_sent_done(s_idx)
      io.dmem_req.bits.addr := addr
      io.dmem_req.bits.tag  := s_idx
      io.dmem_req.bits.cmd  := Mux(stream_type === stream_rd.asUInt, M_XRD, M_XWR)
      io.dmem_req.bits.size := log2Ceil(1024).U // FIXME
      io.dmem_req.bits.data := data

      when (s_sent_done(s_idx)) {
        s_idx := Mux(s_idx === stream_cnt - 1.U, 0.U, s_idx + 1.U)
      }.elsewhen (io.dmem_req.fire) {
        s_idx := Mux(s_idx === stream_cnt - 1.U, 0.U, s_idx + 1.U)
        s_sent(s_idx) := s_sent(s_idx) + 1.U
        when (s_sent(s_idx) === max_reqs - 1.U) {
          s_sent_done(s_idx) := true.B
        }
      }

      when (dmem_resp_val_reg) {
        val s_rec_idx = dmem_resp_tag_reg
        s_rec(s_rec_idx) := s_rec(s_rec_idx) + 1.U
        printf(p"tag: ${dmem_resp_tag_reg} s_rec_idx: ${s_rec_idx} s_rec: ${s_rec}\n")

        when (s_rec(s_rec_idx) === max_reqs - 1.U) {
          s_rec_done(s_rec_idx) := true.B
        }
      }

      val sent_done = s_sent_done.zipWithIndex.map { case(s, idx) =>
        val x = WireInit(true.B)
        when (idx.U < stream_cnt) {
          x := s
        }
        x
      }

      when (sent_done.reduce(_ && _)) {
        ctrl_state := ctrl_accesspend.asUInt
      }
    }
    is (ctrl_accesspend.asUInt) {
      printf(p"ctrl_accesspend state\n")

      io.dmem_req.valid := false.B

      when (dmem_resp_val_reg) {
        val s_rec_idx = dmem_resp_tag_reg
        s_rec(s_rec_idx) := s_rec(s_rec_idx) + 1.U
        printf(p"tag: ${dmem_resp_tag_reg} s_rec_idx: ${s_rec_idx} s_rec: ${s_rec} OHToUInt(max_reqs): ${OHToUInt(max_reqs)}\n")

        when (s_rec(s_rec_idx) === max_reqs - 1.U) {
          s_rec_done(s_rec_idx) := true.B
        }
      }

      // FIXME
      val rec_done = s_rec_done.zipWithIndex.map { case(r, idx) =>
        val x = WireInit(true.B)
        when (idx.U < stream_cnt) {
          x := r
        }
        x
      }

      when (rec_done.reduce(_ && _)) {
        busy := false.B
        ctrl_state := ctrl_idle.asUInt

        s_idx := 0.U
        s_sent.foreach(_ := 0.U)
        s_sent_done.foreach(_ := false.B)
        s_rec.foreach(_ := 0.B)
        s_rec_done.foreach(_ := false.B)
      }
    }
  }

  printf(p"$busy")
}
