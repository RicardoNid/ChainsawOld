name := "Chainsaw"
version := "0.1"
scalaVersion := "2.11.12"
//val spinalVersion = "1.4.3"
val spinalVersion = "1.5.0"

// 编译器选项
scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-Dhttp.proxyHost=127.0.0.1",
  "-Dhttp.proxyPort=10809"
)

// SpinalHDL依赖项
libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
)

//  .map(_.exclude("org.slf4j", "slf4j-simple"))

libraryDependencies ++= Seq(
  "org.jgrapht" % "jgrapht-core" % "1.4.0",
  "org.jgrapht" % "jgrapht-ext" % "1.4.0",
  "org.jgrapht" % "jgrapht-io" % "1.4.0",
)

libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"

//libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7"

libraryDependencies += "net.java.dev.jna" % "jna" % "4.2.2"

fork := true
EclipseKeys.withSource := true

// https://mvnrepository.com/artifact/ai.djl/api
libraryDependencies += "ai.djl" % "api" % "0.11.0"
// https://mvnrepository.com/artifact/ai.djl/model-zoo
libraryDependencies += "ai.djl" % "model-zoo" % "0.11.0"
// https://mvnrepository.com/artifact/ai.djl/basicdataset
libraryDependencies += "ai.djl" % "basicdataset" % "0.11.0"
// for matlab co-sim
javaOptions += "-Djava.library.path=/usr/local/MATLAB/R2018b/bin/glnxa64:/usr/local/MATLAB/R2018b/sys/os/glnxa64"
scalacOptions += "-Djava.library.path=/usr/local/MATLAB/R2018b/bin/glnxa64:/usr/local/MATLAB/R2018b/sys/os/glnxa64"