scalaVersion := "2.10.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= 
  Seq("org.scala-lang" % "scala-swing" % "2.10.1",
      "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
      "com.typesafe.akka" %% "akka-actor" % "2.2.0-RC1")
