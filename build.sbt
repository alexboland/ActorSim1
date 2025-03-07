name := "actor-sim-one"

version := "0.1"

scalaVersion := "3.3.1" // Use the Scala version compatible with your project

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.8.0",
  "com.typesafe.akka" %% "akka-stream" % "2.8.0", // Explicitly adding Akka Stream
  "com.typesafe.akka" %% "akka-http" % "10.5.0",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.5.0",
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"    % "0.14.9",
  "io.circe" %% "circe-generic" % "0.14.9",
  "io.circe" %% "circe-parser"  % "0.14.9"
)

// SLF4J API (Required for Akka logging)
libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.9"

// Logback (Logging backend for SLF4J)
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.14"

fork in run := true
javaOptions in run ++= Seq(
  "-Xms1g",
  "-Xmx4g"
)