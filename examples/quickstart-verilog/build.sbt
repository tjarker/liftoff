// A Verilog project using liftoff the way users do. CI builds it against the locally published liftoff to
// check the packaging; LIFTOFF_VERSION selects the liftoff version.

scalaVersion := "2.13.18"

libraryDependencies ++= Seq(
  // Any liftoff artifact simulates Verilog; this one brings Chisel 7.
  "io.github.tjarker" %% "liftoff-chisel7" % sys.env.getOrElse("LIFTOFF_VERSION", "0.0.1"),
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

// liftoff runs tasks as coroutines and calls Verilator models through the foreign function API.
fork := true
javaOptions ++= Seq(
  "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED",
  "--enable-native-access=ALL-UNNAMED"
)
