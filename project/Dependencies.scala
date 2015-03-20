import sbt._

object Dependencies {

	//Versions
	lazy val akkaVersion = "2.3.9"
	lazy val parserCombinatorVersion = "1.0.3"

	//Libraries
	val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
	val parserCombinator = "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorVersion


	//Projects
	val nfnDeps = 
		Seq(akkaActor, parserCombinator)

}
