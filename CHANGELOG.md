# Changelog

## Unreleased

- Reporting levels (`Off` to `Trace`) per component and its children, set in code or with
  `LIFTOFF_LOG`. They replace `addProviderFilter` and `removeProviderFilter`.
- Liftoff's own reports come from `liftoff.*`. Phase changes, runtime tables and scheduler details
  are debug or trace; a model reports its compilation time and simulation frequency under its
  module name.
- A Verilog quickstart in `examples/quickstart-verilog`; `import liftoff._` brings `VerilogSimModel`.
- `VerilogModel` reports like `ChiselModel`, and its `runTimes` have the same entries: `Scheduler`
  replaces `Overhead` and, as for Chisel models, includes GC time.

## 0.0.1

First release, for Chisel 3.6, 6 and 7.

- Simulate Chisel modules (`ChiselModel`) and Verilog designs (`VerilogModel`) with Verilator.
- Peek, poke and expect Chisel types; failed expectations fail the test.
- Tasks that run concurrently in simulated time.
- Testbench components: drivers, monitors, analysis components, phases and configuration.
- Model options for waves, parameters, clocks and logs, and hooks for the build commands.
