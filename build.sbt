enablePlugins(ProtobufPlugin)

name := "ecosystem"
version := "0.1"
scalaVersion := "2.12.8"

unmanagedJars in Compile ++= Seq(
  file("lib/jphantom-1.2.jar"),
  file("lib/axml-2.0.jar"),
  file("lib/herosclasses-trunk.jar"),
  file("lib/hamcrest-all-1.3.jar"),
  file("lib/dexlib2-2.0.7-dev.jar"),
  file("util-2.1.2-87d10dac.jar")
)

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.6",
  "org.slf4j" % "slf4j-simple" % "1.7.25",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.google.guava" % "guava" % "27.0.1-jre",
  "org.ow2.asm" % "asm-debug-all" % "5.2",
  "com.lihaoyi" %% "scalatags" % "0.6.7",
  "args4j" % "args4j" % "2.33",
  "net.sf.jgrapht" % "jgrapht" % "0.8.3"
)
