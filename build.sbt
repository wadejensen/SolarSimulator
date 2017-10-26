crossScalaVersions := Seq("2.12.3", "2.11.8")

lazy val root = (project in file("."))
  .settings(
    name := "solar-car-simulator",
    scalaVersion := "2.12.3",
    version := "0.1.0",
    mainClass in assembly :=
      Some("au.com.wadejensen.solarcar.main.StrategyEngineTestMain"))

val nd4jVersion = "0.9.1"

libraryDependencies ++=
  Seq(
    "com.github.melrief" %% "purecsv" % "0.1.0",
    "com.typesafe" % "config" % "1.3.1",
    "net.e175.klaus" % "solarpositioning" % "0.0.9",
    "org.nd4j" % "nd4j-native-platform" % nd4jVersion,
    "org.nd4j" % "nd4s_2.11" % nd4jVersion
  )
