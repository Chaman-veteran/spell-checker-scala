ThisBuild / scalaVersion := "3.4.2"
val uPickleVersion = "4.0.2"
val jacksonVersion = "2.17.1"

libraryDependencies ++= Seq(
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion,
    "com.lihaoyi" %% "upickle" % uPickleVersion
    )
