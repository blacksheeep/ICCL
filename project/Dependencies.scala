import sbt._

object Dependencies {

	//Versions
	lazy val akkaVersion = "2.3.9"
	lazy val parserCombinatorVersion = "1.0.3"
  lazy val picklingVersion = "0.10.0"

	//Libraries
	val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
	val parserCombinator = "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorVersion
  val pickling = "org.scala-lang.modules" %% "scala-pickling" % picklingVersion

	//Projects
	val nfnDeps = 
		Seq(akkaActor, parserCombinator, pickling)

}
