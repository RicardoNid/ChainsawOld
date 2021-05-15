name := "SpinalTemplateSbt"
version := "1.0"
scalaVersion := "2.11.12"
val spinalVersion = "1.4.3"

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

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.8" % Test

libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7"

// https://mvnrepository.com/artifact/net.java.dev.jna/jna
libraryDependencies += "net.java.dev.jna" % "jna" % "4.2.2"


fork := true
EclipseKeys.withSource := true