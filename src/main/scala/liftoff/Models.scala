package liftoff

import java.io.File

import chisel3.Element
import chisel3.reflect.DataMirror

import liftoff.chisel.ChiselBridge
import liftoff.coroutine.{ContinuationBackend, CoroutineBackend, PlatformThreadBackend, VirtualThreadBackend}
import liftoff.misc.{Reporting, WorkingDirectory}
import liftoff.simulation.Time
import liftoff.simulation.control.SimController
import liftoff.simulation.verilator.{Verilator, VerilatorBuild, VerilatorSimModel, VerilatorSimModelFactory}
import liftoff.verilog.{VerilogModule, VerilogSimModel}

/** Formats a model can record waves in. */
object Waves {
  val Off: Verilator.TraceFormat = Verilator.TraceFormat.NoTrace
  val Vcd: Verilator.TraceFormat = Verilator.TraceFormat.Vcd
  val Fst: Verilator.TraceFormat = Verilator.TraceFormat.Fst
  val Saif: Verilator.TraceFormat = Verilator.TraceFormat.Saif
}

/** Coroutine backends the tasks of a simulation can run on. */
object Backend {
  val Continuations: CoroutineBackend = ContinuationBackend
  val VirtualThreads: CoroutineBackend = VirtualThreadBackend
  val PlatformThreads: CoroutineBackend = PlatformThreadBackend
}

/** Options that only affect how a built model is simulated.
  *
  * @param clock   name and period of the clock; `clock` with a period of 1 ns for Chisel modules
  * @param backend coroutine backend of the tasks; the fastest available one if empty
  * @param log     file in the run directory that receives the reports of the simulation
  */
case class RunSettings(
    clock: Option[(String, Time)] = None,
    backend: Option[CoroutineBackend] = None,
    log: Option[String] = None
)

/** Options of a model: how it is built and how it is simulated. Settings are plain values, so a
  * test suite can share them: `ModelSettings().waves(Waves.Off).jobs(8)`.
  */
case class ModelSettings(build: VerilatorBuild = VerilatorBuild(), run: RunSettings = RunSettings())
    extends BuildOptions[ModelSettings] {
  protected def current: ModelSettings = this
  protected def updated(settings: ModelSettings): ModelSettings = settings
}

/** Options that can change between simulations of a built model. */
trait RunOptions[Self] {
  protected def current: ModelSettings
  protected def updated(settings: ModelSettings): Self

  private def updatedRun(f: RunSettings => RunSettings): Self =
    updated(current.copy(run = f(current.run)))

  /** Period of the clock called `clock`, the clock of every Chisel `Module`. */
  def clock(period: Time): Self = clock("clock", period)

  /** Drives the input `name` as a clock with `period`. All other ports belong to its domain. */
  def clock(name: String, period: Time): Self = updatedRun(_.copy(clock = Some(name -> period)))

  /** Runs the tasks of the simulation on `backend` instead of the fastest available one. */
  def backend(backend: CoroutineBackend): Self = updatedRun(_.copy(backend = Some(backend)))

  /** Writes the reports of each simulation to `fileName` in its run directory. */
  def log(fileName: String): Self = updatedRun(_.copy(log = Some(fileName)))
}

/** Options for building a model, and the run options it starts with.
  *
  * `verilator`, `cxx` and `link` give complete control over the three commands that build the
  * model. Each receives the whole command liftoff would run, program first and with all options
  * already turned into flags, and returns the command to run instead. Calling a hook again
  * applies both, in order. See [[VerilatorBuild]] for the flags liftoff appends afterwards.
  */
trait BuildOptions[Self] extends RunOptions[Self] {

  private def updatedBuild(f: VerilatorBuild => VerilatorBuild): Self =
    updated(current.copy(build = f(current.build)))

  private def withArguments(arguments: Seq[Verilator.Argument]): Self =
    updatedBuild(b => b.copy(arguments = b.arguments ++ arguments))

  /** Format of the waves each simulation records; `Waves.Fst` unless set. */
  def waves(format: Verilator.TraceFormat): Self = updatedBuild(_.copy(waves = format))

  /** Timescale of Verilog files that do not declare one, such as "1ns/1ps". */
  def timescale(timescale: String): Self = withArguments(Seq(Verilator.Arguments.DefaultTimeScale(timescale)))

  /** Overrides parameters of the top module, such as `"WIDTH" -> 32`. */
  def params(params: (String, Any)*): Self =
    withArguments(params.map { case (name, value) => Verilator.Arguments.TopParam(name, value.toString) })

  /** Checks SystemVerilog assertions during simulation. */
  def assertions: Self = withArguments(Seq(Verilator.Arguments.Assertions))

  /** Number of parallel jobs building the model. */
  def jobs(n: Int): Self = withArguments(Seq(Verilator.Arguments.Jobs(n)))

  /** Additional Verilog sources, such as black boxes. */
  def sources(files: File*): Self = updatedBuild(b => b.copy(sources = b.sources ++ files))

  /** C++ sources, such as DPI functions, compiled and linked into the model. */
  def dpi(files: File*): Self = updatedBuild(b => b.copy(dpi = b.dpi ++ files))

  /** Rewrites the Verilator command. */
  def verilator(hook: Seq[String] => Seq[String]): Self =
    updatedBuild(b => b.copy(verilator = b.verilator.andThen(hook)))

  /** Rewrites the command compiling the C++ harness that connects the model to liftoff. */
  def cxx(hook: Seq[String] => Seq[String]): Self = updatedBuild(b => b.copy(cxx = b.cxx.andThen(hook)))

  /** Rewrites the command linking the model into a shared library. */
  def link(hook: Seq[String] => Seq[String]): Self = updatedBuild(b => b.copy(link = b.link.andThen(hook)))
}

private[liftoff] object ModelRun {

  /** Runs `block` with the reports going to the log file of `run`, if it has one. */
  def logged[R](run: RunSettings, runDir: WorkingDirectory)(block: => R): R = run.log match {
    case Some(fileName) =>
      val stream = runDir.addLoggingFile(fileName)
      try Reporting.withOutput(stream, colored = false)(block)
      finally stream.close()
    case None => block
  }

  def waveFile(simModel: VerilatorSimModel): Option[File] =
    Option.when(simModel.factory.waves != Waves.Off)(simModel.waveFile)

  def commands(factory: VerilatorSimModelFactory): Seq[String] = factory.commands.map(_.mkString(" "))
}

/** A Chisel module to build into a model. Chain options, then `build` it or `simulate` it right away. */
class ChiselModelBuilder[M <: chisel3.Module] private[liftoff] (
    gen: () => M,
    protected val current: ModelSettings
) extends BuildOptions[ChiselModelBuilder[M]] {

  protected def updated(settings: ModelSettings): ChiselModelBuilder[M] = new ChiselModelBuilder(gen, settings)

  /** Elaborates the module and builds the model in `buildDir`. */
  def build(buildDir: WorkingDirectory): ChiselModel[M] = {
    val startTime = System.nanoTime()
    val dut = ChiselBridge.elaborate(gen())
    val files = ChiselBridge.emitSystemVerilogFile(dut.name, gen(), buildDir)
    val factory = VerilatorSimModelFactory.create(dut.name, buildDir, files, current.build)
    val endTime = System.nanoTime()
    Reporting.info(None, "ChiselModel", f"Elaboration and Verilator model compilation took ${(endTime - startTime) / 1e6.toDouble}%.2f ms")
    val ports = DataMirror.fullModulePorts(dut).collect {
      case (_, el: Element) => el // only collect leaf ports
    }
    new ChiselModel[M](gen, factory, ports.toSeq, (endTime - startTime).ns, current)
  }

  /** Builds the model in `dir` and simulates it there. */
  def simulate[T](dir: WorkingDirectory)(block: M => T): SimulationResult[T] = build(dir).simulate(dir)(block)
}

/** A built model of a Chisel module. Only run options can change, so nothing needs a rebuild. */
class ChiselModel[M <: chisel3.Module] private[liftoff] (
    dutGen: () => M,
    modelFactory: VerilatorSimModelFactory,
    ports: Seq[Element],
    compilationTime: Time,
    protected val current: ModelSettings
) extends RunOptions[ChiselModel[M]] {

  protected def updated(settings: ModelSettings): ChiselModel[M] =
    new ChiselModel(dutGen, modelFactory, ports, compilationTime, settings)

  /** The commands that built the model: Verilator, the harness compilation and the link. */
  def commands: Seq[String] = ModelRun.commands(modelFactory)

  def simulate[T](runDir: WorkingDirectory)(block: M => T): SimulationResult[T] = ModelRun.logged(current.run, runDir) {
    val simModel = modelFactory.createModel(runDir)
    val dut = ChiselBridge.elaborate(dutGen())
    val controller = new SimController(simModel, current.run.backend)
    val (clockName, period) = current.run.clock.getOrElse("clock" -> 1.ns)
    SimController.runWith(controller) {

      controller.addClockDomain(
        clockName,
        period,
        ports.filterNot(_.name == clockName).map(port => ChiselBridge.Port.fromData(port).handle)
      )

      val root = controller.addTask("rootTask", 0, None)(block(dut))
      try {
        val startSimTime = System.nanoTime()
        val startGcTime = GcTime.totalGcTimeMs
        controller.run()
        val endSimTime = System.nanoTime()
        val total = (endSimTime - startSimTime).ns
        val endGcTime = GcTime.totalGcTimeMs
        val totalGc = (endGcTime - startGcTime).ms
        val verilator = controller.getModelRunTimeNanos().ns
        val tasks = controller.getTaskRunTimeNanos().ns
        val overhead = total - verilator - tasks
        val frequencykhz = dut.clock.cycle / total.ms.toDouble

        val timeOverview = Seq(
          "Total" -> total,
          "Verilator" -> verilator,
          "Tasks" -> tasks,
          "Scheduler" -> overhead,
          "GC" -> totalGc,
          "Compilation" -> compilationTime
        )
        Reporting.info(None, "ChiselSimulation", Reporting.table(Seq("Description", "Time") +: timeOverview.toSeq.map { case (k, v) => Seq(k, v.toString()) }))
        Reporting.info(None, "ChiselSimulation", f"Simulation frequency: ${frequencykhz}%.2f kHz (${dut.clock.cycle} cycles in ${total})")
        SimulationResult(root.result.get, timeOverview.toMap, frequencykhz, dut.clock.cycle, ModelRun.waveFile(simModel))
      } catch {
        // keyboard interrupt
        case e: InterruptedException =>
          Reporting.info(None, "ChiselSimulation", s"Simulation interrupted by user")
          throw e
        case e: Throwable =>
          Reporting.error(None, "ChiselSimulation", s"Simulation failed with exception: ${e.getMessage}")
          throw e
      } finally {
        Reporting.info(None, "ChiselSimulation", s"Cleaning up simulation model")
        simModel.cleanup()
      }
    }
  }
}

object ChiselModel {

  /** A model of the Chisel module `gen`. Chain options, then `build` or `simulate` it. */
  def apply[M <: chisel3.Module](gen: => M): ChiselModelBuilder[M] = new ChiselModelBuilder(() => gen, ModelSettings())

  /** A model of the Chisel module `gen`, starting from `settings`. */
  def apply[M <: chisel3.Module](gen: => M, settings: ModelSettings): ChiselModelBuilder[M] =
    new ChiselModelBuilder(() => gen, settings)
}

/** A Verilog design to build into a model. Chain options, then `build` it or `simulate` it right away. */
class VerilogModelBuilder private[liftoff] (
    name: String,
    files: Seq[File],
    protected val current: ModelSettings
) extends BuildOptions[VerilogModelBuilder] {

  protected def updated(settings: ModelSettings): VerilogModelBuilder = new VerilogModelBuilder(name, files, settings)

  /** Builds the model of the top module `name` in `buildDir`. */
  def build(buildDir: WorkingDirectory): VerilogModel = {
    val startTime = System.nanoTime()
    val simModelFactory = VerilatorSimModelFactory.create(name, buildDir, files, current.build)
    val endTime = System.nanoTime()
    Reporting.info(None, "VerilogModel", f"Verilator model compilation took ${(endTime - startTime) / 1e6.toDouble}%.2f ms")
    new VerilogModel(VerilogModule(name, files), simModelFactory, (endTime - startTime).ns, current)
  }

  /** Builds the model in `dir` and simulates it there. */
  def simulate[T](dir: WorkingDirectory)(block: VerilogSimModel => T): SimulationResult[T] =
    build(dir).simulate(dir)(block)
}

/** A built model of a Verilog design. Only run options can change, so nothing needs a rebuild. */
class VerilogModel private[liftoff] (
    module: VerilogModule,
    simModelFactory: VerilatorSimModelFactory,
    compilationTime: Time,
    protected val current: ModelSettings
) extends RunOptions[VerilogModel] {

  protected def updated(settings: ModelSettings): VerilogModel =
    new VerilogModel(module, simModelFactory, compilationTime, settings)

  /** The commands that built the model: Verilator, the harness compilation and the link. */
  def commands: Seq[String] = ModelRun.commands(simModelFactory)

  def simulate[T](runDir: WorkingDirectory)(block: VerilogSimModel => T): SimulationResult[T] = ModelRun.logged(current.run, runDir) {
    val simModel = simModelFactory.createModel(runDir)
    val controller = new SimController(simModel, current.run.backend)
    val verilogModule = new VerilogSimModel(controller)

    try {
      val startSimTime = System.nanoTime()
      val startGcTime = GcTime.totalGcTimeMs
      val res = controller.run {
        current.run.clock.foreach { case (clockName, period) =>
          val domain = verilogModule.nameToPort.collect { case (portName, port) if portName != clockName => port }
          verilogModule.addClockDomain(clockName, period)(domain.toSeq: _*)
        }
        block(verilogModule)
      }
      val endSimTime = System.nanoTime()
      val total = (endSimTime - startSimTime).ns
      val endGcTime = GcTime.totalGcTimeMs
      val totalGc = (endGcTime - startGcTime).ms
      val verilator = controller.getModelRunTimeNanos().ns
      val tasks = controller.getTaskRunTimeNanos().ns
      val overhead = total - verilator - tasks - totalGc
      SimulationResult(res, Map(
        "Total" -> total,
        "Verilator" -> verilator,
        "Tasks" -> tasks,
        "GC" -> totalGc,
        "Overhead" -> overhead,
        "Compilation" -> compilationTime
      ), 0.0d, 0L, ModelRun.waveFile(simModel))

    } finally {
      simModel.cleanup()
    }
  }
}

object VerilogModel {

  /** A model of the Verilog module `name`, defined in `files`. Chain options, then `build` or `simulate` it. */
  def apply(name: String, files: File*): VerilogModelBuilder = new VerilogModelBuilder(name, files, ModelSettings())

  /** A model of the Verilog module `name`, defined in `files`, starting from `settings`. */
  def apply(name: String, settings: ModelSettings, files: File*): VerilogModelBuilder =
    new VerilogModelBuilder(name, files, settings)
}
