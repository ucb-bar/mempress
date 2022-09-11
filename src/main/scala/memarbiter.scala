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
  val req_in = Flipped(Decoupled(new MemReqInternal))
  val idx = Input(UInt(log2Ceil(max_streams).W))

  val req_out = Decoupled(new MemReqInternal)
}

class MemArbiter()(implicit val p: Parameters) extends Module {
  val max_streams = p(MemPressMaxStreams)

  val io = IO(new MemArbiterIO(max_streams))

  val que_depth = p(MemPressReqQueDepth)
  val que = Seq.fill(max_streams)(Module(new Queue(new MemReqInternal, que_depth)))
// val que_out = Vec(que.map{case q => q.io.deq})

  que.zipWithIndex.foreach { case(q, idx) =>
    q.io.enq <> io.req_in
    when (idx.U =/= io.idx) {
      q.io.enq.valid := false.B
    }
  }

  val arbiter = Module(new RRArbiter(new MemReqInternal, max_streams))
  for (i <- 0 until max_streams) {
    arbiter.io.in(i) <> que(i).io.deq
  }

  io.req_out <> arbiter.io.out
}
