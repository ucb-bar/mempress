//see LICENSE for license
//authors: Joonho Whangbo

package mempress

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.config._
import freechips.rocketchip.util.DecoupledHelper

class CtrlModuleIO()(implicit val p: Parameters) extends Bundle {
  val max_streams = p(MemPressMaxStreams)

  val rocc_in = Flipped(Decoupled(new RoCCCommand))
  val rocc_out = Decoupled(new RoCCResponse)
  val busy = Output(Bool())

  val dmem_status_out = Valid(new RoCCCommand)
  val sfence_out = Output(Bool())

  val dmem_req = Decoupled(new L2ReqInternal)
  val dmem_req_idx = Output(UInt(log2Ceil(max_streams).W))
  val dmem_resp = Vec(max_streams, Flipped(Decoupled(new L2RespInternal)))
}

class CtrlModule()(implicit val p: Parameters) extends Module
    with HasCoreParameters 
    with MemoryOpConstants {

  val FUNCT_GET_STREAM_CNT_AND_TYPE = 0.U
  val FUNCT_GET_MAXREQS = 1.U
  val FUNCT_PARSE_STREAM_INFO = 2.U
  val FUNCT_SFENCE = 3.U

  val max_streams = p(MemPressMaxStreams)

  val io = IO(new CtrlModuleIO)

  val ctrl_idle :: ctrl_getinst :: ctrl_access :: ctrl_accesspend :: Nil = Enum(4)
  val ctrl_state = RegInit(ctrl_idle.asUInt)

  // we don't need responses
  io.rocc_out.valid := false.B
  io.rocc_out.bits.rd := 0.U
  io.rocc_out.bits.data := 0.U

  val busy = RegInit(false.B)
  io.busy := busy

  val sfence_fire = DecoupledHelper(io.rocc_in.valid,
                                    io.rocc_in.bits.inst.funct === FUNCT_SFENCE)
  io.sfence_out := sfence_fire.fire

  io.dmem_status_out.bits <> io.rocc_in.bits
  io.dmem_status_out.valid := io.rocc_in.fire

  val stream_rd :: stream_wr :: Nil = Enum(2)

  val stream_cnt     = RegInit(0.U(log2Ceil(max_streams + 1).W))   // number of streams
  val rec_stream_cnt = RegInit(0.U(log2Ceil(max_streams + 1).W))   // streams received
  val stream_type    = RegInit(stream_rd.asUInt)                   // stream type
  val max_reqs       = RegInit(0.U(64.W))                          // max reqs sent per stream
  val stride         = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W)))) // bytes to skip
  val start_addr     = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))

  val dmem_resp_val_reg = (0 until max_streams).map{ i => RegNext(io.dmem_resp(i).valid) }
  io.dmem_resp.foreach(_.ready := true.B)

  val s_idx       = RegInit(0.U(log2Ceil(max_streams + 1).W))
  val s_sent      = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))
  val s_sent_done = RegInit(VecInit(Seq.fill(max_streams)(false.B)))
  val s_rec       = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))
  val s_rec_done  = RegInit(VecInit(Seq.fill(max_streams)(false.B)))

  io.dmem_req_idx := s_idx
  io.dmem_req.valid := false.B
  io.dmem_req.bits.addr := 0.U
  io.dmem_req.bits.cmd := M_XWR
  io.dmem_req.bits.size := 0.U
  io.dmem_req.bits.data := 0.U

  io.rocc_in.ready := !busy

  switch (ctrl_state) {
    is (ctrl_idle.asUInt) {
      when (io.rocc_in.fire) {
        when (io.rocc_in.bits.inst.funct === FUNCT_GET_STREAM_CNT_AND_TYPE) {
          stream_cnt := io.rocc_in.bits.rs1

          when (io.rocc_in.bits.rs2 === 0.U) {
            stream_type := stream_rd.asUInt
          }.otherwise {
            stream_type := stream_wr.asUInt
          }

          assert(stream_cnt < max_streams.U)

          MemPressLogger.logInfo("ROCC_GET_STREAM_CNT_AND_TYPE: stream_cnt = %d, stream_type = %d\n",
            io.rocc_in.bits.rs1, io.rocc_in.bits.rs2)
        }.elsewhen (io.rocc_in.bits.inst.funct === FUNCT_GET_MAXREQS) {
          max_reqs := io.rocc_in.bits.rs1
          MemPressLogger.logInfo("ROCC_GET_MAXREQS: max_reqs = %d\n", io.rocc_in.bits.rs1)
        }.elsewhen (io.rocc_in.bits.inst.funct === FUNCT_PARSE_STREAM_INFO) {
          rec_stream_cnt := rec_stream_cnt + 1.U

          val start_addr_align = (io.rocc_in.bits.rs2 >> 4.U) << 4.U
          stride(rec_stream_cnt)     := io.rocc_in.bits.rs1
          start_addr(rec_stream_cnt) := start_addr_align

          when (rec_stream_cnt === stream_cnt - 1.U) {
            busy := true.B
            ctrl_state := ctrl_access.asUInt
          }
          MemPressLogger.logInfo("ROCC_PARSE_STREAM_INFO: stride = %d, start_addr = 0x%x, start_addr_align = 0x%x\n", 
            io.rocc_in.bits.rs1, io.rocc_in.bits.rs2, start_addr_align)
        }
      }
    }
    is (ctrl_access.asUInt) {
      val addr = start_addr(s_idx) + stride(s_idx) * s_sent(s_idx)
      val data = ((1.U << 128.U) - 1.U) ^ (1.U << s_idx)

      io.dmem_req.valid     := !s_sent_done(s_idx)
      io.dmem_req.bits.addr := addr
      io.dmem_req.bits.cmd  := Mux(stream_type === stream_rd.asUInt, M_XRD, M_XWR)
      io.dmem_req.bits.size := log2Ceil(16).U // FIXME
      io.dmem_req.bits.data := data

      MemPressLogger.logInfo("CTRL_ACCESS state, dmem_req.fire = %d, s_idx = %d, s_sent = %d, addr = 0x%x, data = 0x%x\n",
        io.dmem_req.fire, s_idx, s_sent(s_idx), addr, data)

      when (s_sent_done(s_idx)) {
        s_idx := Mux(s_idx === stream_cnt - 1.U, 0.U, s_idx + 1.U)
      }.elsewhen (io.dmem_req.fire) {
        s_idx := Mux(s_idx === stream_cnt - 1.U, 0.U, s_idx + 1.U)
        s_sent(s_idx) := s_sent(s_idx) + 1.U
        when (s_sent(s_idx) === max_reqs - 1.U) {
          s_sent_done(s_idx) := true.B
        }
      }

      check_resp()

      val sent_done = check_done(s_sent_done)
      when (sent_done.reduce(_ && _)) {
        ctrl_state := ctrl_accesspend.asUInt
      }
    }
    is (ctrl_accesspend.asUInt) {
      io.dmem_req.valid := false.B

      check_resp()

      val rec_done = check_done(s_rec_done)
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

  def check_done(regs : Vec[Bool]) = {
    val done = regs.zipWithIndex.map { case(r, idx) =>
      val x = WireInit(true.B)
      when (idx.U < stream_cnt) {
        x := r
      }
      x
    }
    done
  }

  def check_resp() {
    for (i <- 0 until max_streams) {
      when (dmem_resp_val_reg(i)) {
        val idx = i.U
        s_rec(idx) := s_rec(idx) + 1.U

        when (s_rec(idx) === max_reqs - 1.U) {
          s_rec_done(idx) := true.B
        }
      }
    }
  }
}
