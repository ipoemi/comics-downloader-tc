
lazy val scalaTestVersion = "3.0.3"
lazy val catsVersion = "1.0.1"
lazy val akkaHttpVersion = "10.0.9"
lazy val betterFilesAkkaVersion = "2.17.1"
lazy val scalaScraperVersion = "2.0.0"
lazy val macroParadiseVersion = "2.1.0"
lazy val kindProjectorVersion = "0.9.4"
lazy val simulacrumVersion = "0.11.0"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "comics-downloader-typeclass",
    libraryDependencies ++= Seq(
      compilerPlugin("org.scalamacros" % "paradise" % macroParadiseVersion cross CrossVersion.full),
      compilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),
      "com.github.mpilquist" %% "simulacrum" % simulacrumVersion,
      "org.typelevel" %% "cats-core" % catsVersion,
      "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
      "com.github.pathikrit" %% "better-files-akka" % betterFilesAkkaVersion,
      "net.ruippeixotog" %% "scala-scraper" % scalaScraperVersion,
      "org.scalatest" %% "scalatest" % scalaTestVersion
    ),

    addCompilerPlugin("org.scalamacros" % "paradise" % macroParadiseVersion cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Ypartial-unification",
      "-language:_"
    )
  )
