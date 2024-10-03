name := "actor-sim-one"

version := "0.1"

scalaVersion := "2.13.6" // Use the Scala version compatible with your project

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.15",
  "com.typesafe.akka" %% "akka-stream" % "2.6.15", // Explicitly adding Akka Stream
  "com.typesafe.akka" %% "akka-http" % "10.2.10",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.10",
  "ch.megard" %% "akka-http-cors" % "1.1.3"
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"    % "0.14.6",
  "io.circe" %% "circe-generic" % "0.14.6",
  "io.circe" %% "circe-parser"  % "0.14.6"
)

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % "1.39.2"
