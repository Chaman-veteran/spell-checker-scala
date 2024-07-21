ThisBuild / scalaVersion := "3.4.2"
val uPickleVersion = "3.3.1"
val jacksonVersion = "2.17.1"

libraryDependencies ++= Seq(
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion
    )
