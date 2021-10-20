organization := "seit311"
name := "Chainsaw"
version := "0.1"
scalaVersion := "2.11.12"
//val spinalVersion = "1.4.3"
val spinalVersion = "1.5.0"
val jgraphtVersion = "1.5.1"

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
  "org.jgrapht" % "jgrapht-core" % jgraphtVersion,
  "org.jgrapht" % "jgrapht-ext" % jgraphtVersion,
  "org.jgrapht" % "jgrapht-io" % jgraphtVersion,
)

libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"

//libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9"

libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7"

libraryDependencies += "net.java.dev.jna" % "jna" % "4.2.2"

fork := true

EclipseKeys.withSource := true

//// https://mvnrepository.com/artifact/ai.djl/api
//libraryDependencies += "ai.djl" % "api" % "0.11.0"
//// https://mvnrepository.com/artifact/ai.djl.mxnet/mxnet-engine
//libraryDependencies += "ai.djl.mxnet" % "mxnet-engine" % "0.11.0" % "runtime"
//// https://mvnrepository.com/artifact/ai.djl.mxnet/mxnet-native-auto
//libraryDependencies += "ai.djl.mxnet" % "mxnet-native-auto" % "1.8.0" % "runtime"
// https://mvnrepository.com/artifact/ai.djl/model-zoo
libraryDependencies += "ai.djl" % "model-zoo" % "0.12.0"
// https://mvnrepository.com/artifact/ai.djl/basicdataset
libraryDependencies += "ai.djl" % "basicdataset" % "0.12.0"


// %mavenRepo snapshots https://oss.sonatype.org/content/repositories/snapshots/

val djlVersion = "0.12.0"

libraryDependencies += "ai.djl"%"api"% "0.12.0"
libraryDependencies += "ai.djl.onnxruntime"%"onnxruntime-engine"% "0.12.0"
libraryDependencies += "ai.djl.pytorch"%"pytorch-engine"% "0.12.0"
libraryDependencies += "org.slf4j"%"slf4j-api"% "1.7.26"
libraryDependencies += "org.slf4j"%"slf4j-simple"% "1.7.26"
libraryDependencies += "com.microsoft.onnxruntime"%"onnxruntime"% "1.4.0"
libraryDependencies += "ai.djl.pytorch"%"pytorch-native-auto"% "1.8.1"

//libraryDependencies += "org.nd4j" % "nd4j-native-platform" % "0.8.0"

libraryDependencies += "com.github.dwickern" %% "scala-nameof" % "3.0.0" % "provided"


