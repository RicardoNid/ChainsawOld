name := "SpinalTemplateSbt"
version := "1.0"
scalaVersion := "2.11.12"
val spinalVersion = "1.4.2"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
)

// https://mvnrepository.com/artifact/org.tensorflow/tensorflow
libraryDependencies += "org.tensorflow" % "tensorflow" % "1.15.0"

fork := true
EclipseKeys.withSource := true

// https://mvnrepository.com/artifact/org.scalanlp/breeze
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"

