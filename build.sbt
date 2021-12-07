scalaVersion := "2.11.12"

// 编译器选项
scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-Dhttp.proxyHost=127.0.0.1",
  "-Dhttp.proxyPort=10809"
)

//spinalHDL
libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.5.0",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "1.5.0",
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % "1.5.0")
)
fork := true

//JGraphT
libraryDependencies ++= Seq(
  "org.jgrapht" % "jgrapht-core" % "1.4.0",
  "org.jgrapht" % "jgrapht-ext" % "1.4.0",
  "org.jgrapht" % "jgrapht-io" % "1.4.0"
)

//scalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9"

//java8
libraryDependencies += "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2"

//breeze
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"

//rings
libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7"

//dwickern
libraryDependencies += "com.github.dwickern" %% "scala-nameof" % "3.0.0" % "provided"

//spire
libraryDependencies += "org.typelevel" %% "spire" % "0.17.0-M1"

//ai
libraryDependencies ++= Seq(
  "ai.djl" % "model-zoo" % "0.12.0",
  "ai.djl" % "basicdataset" % "0.12.0",
  "ai.djl" % "api" % "0.12.0",
  "ai.djl.onnxruntime" % "onnxruntime-engine" % "0.12.0",
  "ai.djl.pytorch" % "pytorch-engine" % "0.12.0",
  "ai.djl.pytorch" % "pytorch-native-auto" % "1.8.1"
)

//oshi
libraryDependencies += "com.github.oshi" % "oshi-core" % "5.8.0"

//openhft
libraryDependencies += "net.openhft" % "affinity" % "3.21ea5"

//jetbrains
libraryDependencies += "org.jetbrains" % "annotations" % "23.0.0"
