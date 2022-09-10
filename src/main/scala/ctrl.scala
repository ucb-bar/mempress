//see LICENSE for license
//authors: Joonho Whangbo
package mempress

import Chisel._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.config._

class CtrlModuleIO()(implicit val p: Parameters) extends Bundle
  with HasCoreParameters {
  val rocc_in = Flipped(Decoupled(new ROCCCommand))
}

class CtrlModule()(implicit val p: Parameters) extends Module
    with HasCoreParameters 
    with MemoryOpConstants {

  val io = Bundle(new CtrlModuleIO)

  when (io.rocc_in.fire()) {
    val rs1 = io.rocc_in.rs1
    val rs2 = io.rocc_in.rs2
    printf(s"rs1: $rs1 rs2 $rs2")
  }
}
