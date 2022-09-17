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

class GlobalStreamInfo()(implicit val p: Parameters) extends Bundle {
  val max_streams = p(MemPressMaxStreams)

  val stream_cnt = UInt(log2Ceil(max_streams + 1).W)
  val addr_range = UInt(64.W)
  val max_reqs   = UInt(64.W)
}

class LocalStreamInfo extends Bundle {
  val start_addr  = UInt(64.W)
  val stride      = UInt(64.W)
  val stream_type = UInt(3.W)
}

class CtrlModuleIO()(implicit val p: Parameters) extends Bundle {
  val max_streams = p(MemPressMaxStreams)
  val idx_w = log2Ceil(max_streams)

  val rocc_in = Flipped(Decoupled(new RoCCCommand))
  val rocc_out = Decoupled(new RoCCResponse)
  val busy = Output(Bool())

  val dmem_status_out = Valid(new RoCCCommand)
  val sfence_out = Output(Bool())

  val send_reqs = Output(Bool())
  val sent_done = Input(Bool())
  val req_fire  = Input(Bool())
  val global_stream_info = Valid(new GlobalStreamInfo)
  val local_stream_info  = Valid(Indexed(new LocalStreamInfo, idx_w))

  val dmem_resp = Vec(max_streams, Flipped(Decoupled(new L2RespInternal)))
}

class CtrlModule()(implicit val p: Parameters) extends Module
    with HasCoreParameters 
    with MemoryOpConstants {

  val FUNCT_SFENCE = 0.U
  val FUNCT_PARSE_GLOBAL_STREAM_INFO = 1.U
  val FUNCT_PARSE_LOCAL_STREAM_INFO = 2.U
  val FUNCT_GET_CYCLE = 3.U
  val FUNCT_GET_REQCNT = 4.U

  val max_streams = p(MemPressMaxStreams)

  val io = IO(new CtrlModuleIO)

  val ctrl_idle :: ctrl_getinst :: ctrl_access :: ctrl_accesspend :: Nil = Enum(4)
  val ctrl_state = RegInit(ctrl_idle.asUInt)

  val cycle_counter = RegInit(0.U(64.W))
  val req_counter = RegInit(0.U(64.W))

  val rocc_out_val = RegInit(false.B)
  val rocc_out_rd  = RegInit(0.U(5.W))
  val rocc_out_data = RegInit(0.U(64.W))
  io.rocc_out.valid := rocc_out_val
  io.rocc_out.bits.rd := rocc_out_rd
  io.rocc_out.bits.data := rocc_out_data

  val busy = RegInit(false.B)
  io.rocc_in.ready := !busy
  io.busy := busy

  val sfence_fire = DecoupledHelper(io.rocc_in.valid,
                                    io.rocc_in.bits.inst.funct === FUNCT_SFENCE)
  io.sfence_out := sfence_fire.fire
  io.dmem_status_out.bits <> io.rocc_in.bits
  io.dmem_status_out.valid := io.rocc_in.fire

  val rec_stream_cnt = RegInit(0.U(log2Ceil(max_streams + 1).W))   // streams received

  val global_info_val = RegInit(false.B)
  val stream_cnt     = RegInit(0.U(log2Ceil(max_streams + 1).W))   // number of streams
  val addr_range     = RegInit(0.U(64.W))
  val max_reqs       = RegInit(0.U(64.W))                          // max reqs sent per stream

  global_info_val := false.B
  io.global_stream_info.valid := global_info_val
  io.global_stream_info.bits.stream_cnt := stream_cnt
  io.global_stream_info.bits.addr_range := addr_range
  io.global_stream_info.bits.max_reqs := max_reqs

  val local_info_val   = RegInit(false.B)
  val start_addr_align = RegInit(0.U(64.W))
  val stride           = RegInit(0.U(64.W))
  val stream_type      = RegInit(0.U(3.W))
  val s_idx_out        = RegInit(0.U(log2Ceil(max_streams + 1).W))

  local_info_val := false.B
  io.local_stream_info.valid := local_info_val
  io.local_stream_info.bits.idx := s_idx_out
  io.local_stream_info.bits.data.start_addr := start_addr_align
  io.local_stream_info.bits.data.stride := stride
  io.local_stream_info.bits.data.stream_type := stream_type

  val send_reqs = RegInit(false.B)
  io.send_reqs := send_reqs

  val dmem_resp_val_reg  = (0 until max_streams).map{ i => RegNext(io.dmem_resp(i).valid) }
  val dmem_resp_data_reg = (0 until max_streams).map{ i => RegNext(io.dmem_resp(i).bits.data) }
  io.dmem_resp.foreach(_.ready := true.B)

  val s_idx       = RegInit(0.U(log2Ceil(max_streams + 1).W))
  val s_sent      = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))
  val s_sent_done = RegInit(VecInit(Seq.fill(max_streams)(false.B)))
  val s_rec       = RegInit(VecInit(Seq.fill(max_streams)(0.U(64.W))))
  val s_rec_done  = RegInit(VecInit(Seq.fill(max_streams)(false.B)))

  switch (ctrl_state) {
    is (ctrl_idle.asUInt) {
      when (io.rocc_in.fire) {
        val funct = io.rocc_in.bits.inst.funct
        val rs1_val = io.rocc_in.bits.rs1
        val rs2_val = io.rocc_in.bits.rs2

        when (funct === FUNCT_PARSE_GLOBAL_STREAM_INFO) {
          global_info_val := true.B
          stream_cnt := rs1_val(4, 0)
          addr_range := rs1_val >> 5.U
          max_reqs := rs2_val

          assert(stream_cnt < max_streams.U)
        } .elsewhen (funct === FUNCT_PARSE_LOCAL_STREAM_INFO) {
          rec_stream_cnt := rec_stream_cnt + 1.U

          local_info_val := true.B
          start_addr_align := (rs2_val >> 4.U) << 4.U
          stride := (rs1_val >> 3.U)
          stream_type := rs1_val(2, 0)
          s_idx_out := rec_stream_cnt

          when (rec_stream_cnt === stream_cnt - 1.U) {
            busy := true.B
            ctrl_state := ctrl_access.asUInt
          }
        }.elsewhen (funct === FUNCT_GET_CYCLE && !rocc_out_val) {
          rocc_out_val := true.B
          rocc_out_rd := io.rocc_in.bits.inst.rd
          rocc_out_data := cycle_counter
          cycle_counter := 0.U
        }.elsewhen (funct === FUNCT_GET_REQCNT && !rocc_out_val) {
          rocc_out_val := true.B
          rocc_out_rd := io.rocc_in.bits.inst.rd
          rocc_out_data := req_counter
          req_counter := 0.U
        }
      }

      when (io.rocc_out.fire) {
        rocc_out_val := false.B
      }
    }
    is (ctrl_access.asUInt) {
      cycle_counter := cycle_counter + 1.U
      when (io.req_fire) {
        req_counter := req_counter + 1.U
      }

      send_reqs := true.B
      check_resp()

      when (io.sent_done) {
        ctrl_state := ctrl_accesspend.asUInt
        send_reqs := false.B
      }
    }
    is (ctrl_accesspend.asUInt) {
      cycle_counter := cycle_counter + 1.U

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

  def check_resp() {
    for (i <- 0 until max_streams) {
      when (dmem_resp_val_reg(i)) {
        val idx = i.U
        s_rec(idx) := s_rec(idx) + 1.U

        when (s_rec(idx) === max_reqs - 1.U) {
          s_rec_done(idx) := true.B
        }
        MemPressLogger.logInfo("MemResp -> data: 0x%x\n", dmem_resp_data_reg(i))
      }
    }
  }

  // FIXME : same code in reqgen.scala
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
}
