import Dependencies._

lazy val commonSettings = Seq(
	version := "0.1.0",
	scalaVersion := "2.11.5"
)

lazy val NFN = (project in file(".")).
	settings(commonSettings: _*).
	settings(
		libraryDependencies ++= nfnDeps
	)
