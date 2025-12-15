package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import liftoff.chiselbridge.ChiselBridge
import liftoff.simulation.Time._
import liftoff.misc.PathToFileOps
import liftoff.simulation.verilator.VerilatorSimModelFactory
import liftoff.misc.Reporting
import liftoff.coroutine.ContinuationBackend

class PeekPokeAPITests extends AnyWordSpec with Matchers with liftoff.chiselbridge.ChiselPeekPokeAPI {

  class MyBundle extends Bundle {
    val a = UInt(4.W)
    val b = SInt(8.W)
  }

  class MyModule extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
      val bundleIn = Input(new MyBundle)
      val vecIn = Input(Vec(3, UInt(3.W)))
    })

    io.out := io.in + 1.U + io.bundleIn.a + io.bundleIn.b.asUInt + io.vecIn.reduce(_ +& _)
  }

  "The Chisel PeekPokeAPI" should {
    "not throw errors when poking and peeking ports" in {
      


      val workingDir = "build/peekpoke_test".toDir
      workingDir.createIfNotExists()
      workingDir.clean()
      ChiselBridge.emitSystemVerilogFile(new MyModule, workingDir)

      val runDir = workingDir.addSubDir(workingDir / "sim")

      val simModel = VerilatorSimModelFactory.create(
        "MyModule",
        workingDir.addSubDir(workingDir / "verilator"),
        Seq(workingDir / "MyModule.sv"),
        verilatorOptions = Seq(),
        cOptions = Seq()
      ).createModel(runDir)

      val controller = new SimController(simModel)

      val dut = ChiselBridge.elaborate(new MyModule)

      Reporting.withOutput(runDir.addLoggingFile("simulation.log"), colored = false) {

        controller.addClock(controller.getInputPortHandle("clock").get, 10.fs)

        SimController.runWith(controller) {

          controller.addActiveTask("root") {


            SimController.current.addInactiveTask("monitor") { for (_ <- 0 until 5) {
                //print inputs and outputs
                Reporting.info(Some(controller.currentTime), "Test", s"in: ${dut.io.in.peek().litValue}")
                Reporting.info(Some(controller.currentTime), "Test", s"out: ${dut.io.out.peek().litValue}")
                Reporting.info(Some(controller.currentTime), "Test", s"bundleIn: a=${dut.io.bundleIn.a.peek().litValue}," +
                  s" b=${dut.io.bundleIn.b.peek().litValue}")
                Reporting.info(Some(controller.currentTime), "Test", s"vecIn: ${dut.io.vecIn.map(_.peek().litValue).mkString(",")}")
                
                dut.clock.step(1, 10)
              }
            }

            dut.io.in.poke(42.U)
            val outValue = dut.io.out.peek().litValue

            dut.io.bundleIn.poke((new MyBundle).Lit(
              _.a -> 3.U,
              _.b -> (-5).S
            ))

            dut.io.bundleIn.b.expect((-5).S)

            dut.io.vecIn.poke(Vec.Lit(4.U, 3.U, 5.U))

            dut.clock.step(1, 10)

            val outValue2 = dut.io.out.peek().litValue

            dut.io.in.poke(10.U)

            dut.clock.step(1, 10)

            dut.io.bundleIn.a.poke(7.U)
            dut.io.bundleIn.b.poke(4.S)
            dut.io.vecIn(0).poke(0.U)
            dut.io.vecIn(1).poke(1.U)
            dut.io.vecIn(2).poke(2.U)
            dut.clock.step(1, 10)

            dut.io.bundleIn.peek().a.litValue shouldBe 7
            dut.io.bundleIn.peek().b.litValue shouldBe 4

            dut.io.vecIn.peek().map(_.litValue) shouldBe Seq(0, 1, 2)

          }

          controller.run()
          simModel.cleanup()
        }
      }
    }
  }

}
