name := "SolarCarSimulator"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq("org.rogach" %% "scallop" % "3.1.0",
                            "com.github.melrief" %% "purecsv" % "0.1.0",
                            "joda-time" % "joda-time" % "2.9.9",
                            "com.typesafe" % "config" % "1.3.1")
