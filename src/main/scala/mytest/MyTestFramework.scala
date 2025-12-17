package mytest

import sbt.testing._

class MyTestFramework extends Framework {
  def name(): String = {
    println("I was asked for my name") // Debugging line
    "MyTestFramework"
  }

  def fingerprints(): Array[Fingerprint] = {
    println("Creating fingerprints") // Debugging line
    Array(
      new SubclassFingerprint {
        def superclassName(): String = "mytest.MyTest"
        def isModule(): Boolean = false
        def requireNoArgConstructor(): Boolean = true
      },
      new SubclassFingerprint {
        def superclassName(): String = "mytest.MyTest"
        def isModule(): Boolean = true
        def requireNoArgConstructor(): Boolean = true
      },
      new SubclassFingerprint {
        def superclassName(): String = "mytest.MyTest"
        def isModule(): Boolean = false
        def requireNoArgConstructor(): Boolean = false
      },
      new SubclassFingerprint {
        def superclassName(): String = "mytest.MyTest"
        def isModule(): Boolean = true
        def requireNoArgConstructor(): Boolean = false
      }
    )
  }

  def runner(
      args: Array[String],
      remoteArgs: Array[String],
      testClassLoader: ClassLoader
  ): Runner = {
    println("Creating MyTestRunner") // Debugging line
    new MyTestRunner(args, remoteArgs, testClassLoader)
  }
}
