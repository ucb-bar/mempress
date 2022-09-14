//see LICENSE for license
//authors: Joonho Whangbo

package mempress

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.config._

class MemArbiterIO(val max_streams: Int)(implicit val p: Parameters) extends Bundle {
  val req_in = Flipped(Decoupled(new L2ReqInternal))
  val idx = Input(UInt(log2Ceil(max_streams).W))

  val req_out = Decoupled(new L2ReqInternal)
  val chosen = Output(UInt(log2Ceil(max_streams).W))
}

class MemArbiter()(implicit val p: Parameters) extends Module {
  val max_streams = p(MemPressMaxStreams)

  val io = IO(new MemArbiterIO(max_streams))

  val que_depth = p(MemPressArbQueDepth)
  val que = Seq.fill(max_streams)(Module(new Queue(new L2ReqInternal, que_depth)))

  io.req_in.ready := false.B
  que.zipWithIndex.foreach { case(q, idx) =>
    q.io.enq.bits := io.req_in.bits
    when (idx.U =/= io.idx) {
      q.io.enq.valid := false.B
    }.otherwise {
      q.io.enq.valid := io.req_in.valid
      io.req_in.ready := q.io.enq.ready
    }
  }

  val arbiter = Module(new RRArbiter(new L2ReqInternal, max_streams))
  for (i <- 0 until max_streams) {
    arbiter.io.in(i) <> que(i).io.deq
  }

  io.req_out <> arbiter.io.out
  io.chosen := arbiter.io.chosen
}
