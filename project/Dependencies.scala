import sbt._

object Dependencies {

	//Versions
	lazy val akkaVersion = "2.3.9"
	lazy val parserCombinatorVersion = "1.0.3"
	lazy val playVersion = "2.3.4"
  	lazy val picklingVersion = "0.10.0"

	//Libraries
	val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
	val parserCombinator = "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorVersion
	val optParser = "com.github.scopt" %% "scopt" % "3.3.0"

	//Projects
	val nfnDeps = 
		Seq(akkaActor, parserCombinator, optParser)

}
