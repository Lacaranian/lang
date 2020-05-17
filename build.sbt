name := "lang"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

// Testing
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

logBuffered in Test := false

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"
