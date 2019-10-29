val validationVersion = "2.1.0"
val scalaTestVersion = "3.0.8"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

lazy val root = (project in file("."))
  .settings(
    organization := "com.example",
    name := "jto-validation-demo",
    version := "0.0.1",
    scalaVersion := "2.12.8",
    maxErrors := 3,
    libraryDependencies ++= Seq(
      "io.github.jto" %% "validation-core"      % validationVersion,
      "io.github.jto" %% "validation-playjson"  % validationVersion,
      "io.github.jto" %% "validation-jsonast"   % validationVersion,
      "io.github.jto" %% "validation-form"      % validationVersion,
      "io.github.jto" %% "validation-delimited" % validationVersion,
      "io.github.jto" %% "validation-xml"       % validationVersion
      // "io.github.jto" %%% "validation-jsjson"    % validationVersion
    ),
    libraryDependencies ++= Seq(
      "org.scalatest"    %% "scalatest" % scalaTestVersion % "test"
    )
  )

// Refine scalac params from tpolecat
scalacOptions --= Seq(
  "-Xfatal-warnings"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("chk", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
