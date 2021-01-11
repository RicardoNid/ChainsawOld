name := "SpinalTemplateSbt"
version := "1.0"
scalaVersion := "2.11.12"
val spinalVersion = "1.4.2"


libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
).map(_.exclude("org.slf4j", "log4j12"))

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-Dhttp.proxyHost=127.0.0.1",
  "-Dhttp.proxyPort=10809"
)

// https://mvnrepository.com/artifact/org.tensorflow/tensorflow
libraryDependencies += "org.tensorflow" % "tensorflow" % "1.15.0"
// https://mvnrepository.com/artifact/ai.djl/api
libraryDependencies += "ai.djl" % "api" % "0.9.0"
libraryDependencies += "ai.djl" % "repository" % "0.4.1"

libraryDependencies += "ai.djl.mxnet" % "mxnet-engine" % "0.9.0" % "runtime"
libraryDependencies += "ai.djl.mxnet" % "mxnet-model-zoo" % "0.9.0"
libraryDependencies += "ai.djl.mxnet" % "mxnet-native-auto" % "1.7.0-b"
libraryDependencies += "ai.djl" % "examples" % "0.6.0"


libraryDependencies += "ai.djl" % "model-zoo" % "0.9.0"
// https://mvnrepository.com/artifact/ai.djl/basicdataset
libraryDependencies += "ai.djl" % "basicdataset" % "0.9.0"

// https://mvnrepository.com/artifact/org.jgrapht/jgrapht-core
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.4.0"
// https://mvnrepository.com/artifact/org.jgrapht/jgrapht-ext
libraryDependencies += "org.jgrapht" % "jgrapht-ext" % "1.4.0"
// https://mvnrepository.com/artifact/org.jgrapht/jgrapht-io
libraryDependencies += "org.jgrapht" % "jgrapht-io" % "1.4.0"


//// https://mvnrepository.com/artifact/org.slf4j/slf4j-api
//libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.26"
//// https://mvnrepository.com/artifact/org.slf4j/slf4j-simple
//libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.26" % Test
//
//// https://mvnrepository.com/artifact/net.java.dev.jna/jna
//libraryDependencies += "net.java.dev.jna" % "jna" % "5.3.0"
//libraryDependencies += "org.apache.spark" %% "spark-core" % "2.3.0"

fork := true
EclipseKeys.withSource := true

// https://mvnrepository.com/artifact/org.scalanlp/breeze
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"