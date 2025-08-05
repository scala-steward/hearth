import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import com.typesafe.tools.mima.core.{Problem, ProblemFilters}
import sbtwelcome.UsefulTask
import commandmatrix.extra.*

// Used to configure the build so that it would format+compile during development but not+CI.
lazy val isCI = sys.env.get("CI").contains("true")
ThisBuild / scalafmtOnCompile := !isCI

val mavenCentralSnapshots = "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"
credentials += Credentials(
  "Maven Central Repository",
  "central.sonatype.com",
  sys.env.getOrElse("SONATYPE_USERNAME", ""),
  sys.env.getOrElse("SONATYPE_PASSWORD", "")
)

// Versions:

val versions = new {
  val scala212 = "2.12.20"
  val scala213 = "2.13.16"
  val scala3 = "3.3.6"

  // Which versions should be cross-compiled for publishing
  val scalas = List(scala212, scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies
  val kindProjector = "0.13.3"
  val munit = "1.1.1"
  val scalaCollectionCompat = "2.13.0"
}

// Development settings:

val dev = new {

  val props = scala.util
    .Using(new java.io.FileInputStream("dev.properties")) { fis =>
      val props = new java.util.Properties()
      props.load(fis)
      props
    }
    .get

  // Which version should be used in IntelliJ
  val ideScala = props.getProperty("ide.scala") match {
    case "2.12" => versions.scala212
    case "2.13" => versions.scala213
    case "3"    => versions.scala3
  }
  val idePlatform = props.getProperty("ide.platform") match {
    case "jvm"    => VirtualAxis.jvm
    case "js"     => VirtualAxis.js
    case "native" => VirtualAxis.native
  }

  val logCrossQuotes = props.getProperty("log.cross-quotes") match {
    case "true"  => true
    case "false" => false
    case _       => !isCI
  }
}

// Common settings:

Global / excludeLintKeys += git.useGitDescribe
Global / excludeLintKeys += ideSkipProject
val only1VersionInIDE =
  MatrixAction
    .ForPlatform(dev.idePlatform)
    .Configure(
      _.settings(
        ideSkipProject := (scalaVersion.value != dev.ideScala),
        bspEnabled := (scalaVersion.value == dev.ideScala),
        scalafmtOnCompile := !isCI
      )
    ) +:
    versions.platforms.filter(_ != dev.idePlatform).map { platform =>
      MatrixAction
        .ForPlatform(platform)
        .Configure(_.settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false))
    }

val addScala213plusDir =
  MatrixAction
    .ForScala(v => (v.value == versions.scala213) || v.isScala3)
    .Configure(
      _.settings(
        Compile / unmanagedSourceDirectories += sourceDirectory.value.toPath.resolve("main/scala-2.13+").toFile,
        Test / unmanagedSourceDirectories += sourceDirectory.value.toPath.resolve("test/scala-2.13+").toFile
      )
    )

// hearth-cross-quotes on Scala 2 are macros (defined for all platforms) and on Scala 3 are plugins (defined only for JVM)
val defineCrossQuotes = versions.scalas.flatMap { scalaVersion =>
  if (scalaVersion == versions.scala3) {
    List(
      MatrixAction {
        case (version, List(VirtualAxis.js))     => version.isScala3
        case (version, List(VirtualAxis.native)) => version.isScala3
        case _                                   => false
      }.Skip
    )
  } else {
    List.empty
  }
}

// same reason as above
val useCrossQuotes = versions.scalas.flatMap { scalaVersion =>
  if (scalaVersion == versions.scala3) {
    List(
      MatrixAction
        .ForScala(v => v.value == scalaVersion)
        .Configure(
          _.settings(
            scalacOptions ++= {
              val jar = (hearthCrossQuotes.jvm(scalaVersion) / Compile / packageBin).value
              Seq(
                s"-Xplugin:${jar.getAbsolutePath}",
                s"-Jdummy=${jar.lastModified}", // ensures recompilation
                s"-P:hearth.cross-quotes:logging=${dev.logCrossQuotes}" // enable logging from cross-quotes
              )
            }
          )
        )
    )
  } else {
    List(
      MatrixAction {
        case (version, List(VirtualAxis.jvm)) => version.value == scalaVersion
        case _                                => false
      }.Configure(
        _.settings(
          scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=${dev.logCrossQuotes}" // enable logging from cross-quotes
        )
          .dependsOn(hearthCrossQuotes.jvm(scalaVersion))
      ),
      MatrixAction {
        case (version, List(VirtualAxis.js)) => version.value == scalaVersion
        case _                               => false
      }.Configure(
        _.settings(
          scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=${dev.logCrossQuotes}" // enable logging from cross-quotes
        )
          .dependsOn(hearthCrossQuotes.js(scalaVersion))
      ),
      MatrixAction {
        case (version, List(VirtualAxis.native)) => version.value == scalaVersion
        case _                                   => false
      }.Configure(
        _.settings(
          scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=${dev.logCrossQuotes}" // enable logging from cross-quotes
        )
          .dependsOn(hearthCrossQuotes.native(scalaVersion))
      )
    )
  }
}

val settings = Seq(
  git.useGitDescribe := true,
  git.uncommittedSignifier := None,
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq(
          // format: off
          "-encoding", "UTF-8",
          "-rewrite",
          "-source", "3.3-migration",
          // format: on
          "-unchecked",
          "-deprecation",
          "-explain",
          "-explain-cyclic",
          "-explain-types",
          "-feature",
          "-no-indent",
          "-Wconf:msg=Unreachable case:s", // suppress fake (?) errors in internal.compiletime
          "-Wconf:msg=Missing symbol position:s", // suppress warning https://github.com/scala/scala3/issues/21672
          "-Wnonunit-statement",
          // "-Wunused:imports", // import x.Underlying as X is marked as unused even though it is! probably one of https://github.com/scala/scala3/issues/: #18564, #19252, #19657, #19912
          "-Wunused:privates",
          "-Wunused:locals",
          "-Wunused:explicits",
          "-Wunused:implicits",
          "-Wunused:params",
          "-Wvalue-discard",
          "-Xfatal-warnings",
          "-Xcheck-macros",
          "-Ykind-projector:underscores"
        )
      case Some((2, 13)) =>
        Seq(
          // format: off
          "-encoding", "UTF-8",
          "-release", "8",
          // format: on
          "-unchecked",
          "-deprecation",
          "-explaintypes",
          "-feature",
          "-language:higherKinds",
          "-Wconf:origin=scala.collection.compat.*:s", // type aliases without which 2.12 fail compilation but 2.13/3 doesn't need them
          "-Wconf:cat=scala3-migration:s", // silence mainly issues with -Xsource:3 and private case class constructors
          "-Wconf:cat=deprecation&origin=hearth.*:s", // we want to be able to deprecate APIs and test them while they're deprecated
          "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s", // suppress fake(?) errors in internal.compiletime (adding origin breaks this suppression)
          "-Wconf:msg=discarding unmoored doc comment:s", // silence errors when scaladoc cannot comprehend nested vals
          "-Wconf:msg=Could not find any member to link for:s", // since errors when scaladoc cannot link to stdlib types or nested types
          "-Wconf:msg=Variable .+ undefined in comment for:s", // silence errors when there we're showing a buggy Expr in scaladoc comment
          "-Wunused:patvars",
          "-Xfatal-warnings",
          "-Xlint:adapted-args",
          "-Xlint:delayedinit-select",
          "-Xlint:doc-detached",
          "-Xlint:inaccessible",
          "-Xlint:infer-any",
          "-Xlint:nullary-unit",
          "-Xlint:option-implicit",
          "-Xlint:package-object-classes",
          "-Xlint:poly-implicit-overload",
          "-Xlint:private-shadow",
          "-Xlint:stars-align",
          "-Xlint:type-parameter-shadow",
          "-Xsource:3",
          "-Ywarn-dead-code",
          "-Ywarn-numeric-widen",
          "-Ywarn-unused:locals",
          "-Ywarn-unused:imports",
          "-Ywarn-macros:after",
          "-Ytasty-reader"
        )
      case Some((2, 12)) =>
        Seq(
          // format: off
          "-encoding", "UTF-8",
          "-target:jvm-1.8",
          // format: on
          "-unchecked",
          "-deprecation",
          "-explaintypes",
          "-feature",
          "-language:higherKinds",
          "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s", // suppress fake(?) errors in internal.compiletime (adding origin breaks this suppression)
          "-Wconf:msg=discarding unmoored doc comment:s", // silence errors when scaladoc cannot comprehend nested vals
          "-Wconf:msg=Could not find any member to link for:s", // since errors when scaladoc cannot link to stdlib types or nested types
          "-Wconf:msg=Variable .+ undefined in comment for:s", // silence errors when there we're showing a buggy Expr in scaladoc comment
          "-Xexperimental",
          "-Xfatal-warnings",
          "-Xfuture",
          "-Xlint:adapted-args",
          "-Xlint:by-name-right-associative",
          "-Xlint:delayedinit-select",
          "-Xlint:doc-detached",
          "-Xlint:inaccessible",
          "-Xlint:infer-any",
          "-Xlint:nullary-override",
          "-Xlint:nullary-unit",
          "-Xlint:option-implicit",
          "-Xlint:package-object-classes",
          "-Xlint:poly-implicit-overload",
          "-Xlint:private-shadow",
          "-Xlint:stars-align",
          "-Xlint:type-parameter-shadow",
          "-Xlint:unsound-match",
          "-Xsource:3",
          "-Yno-adapted-args",
          "-Ywarn-dead-code",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-numeric-widen",
          "-Ywarn-unused:locals",
          "-Ywarn-unused:imports",
          "-Ywarn-macros:after",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit"
        )
      case _ => Seq.empty
    }
  },
  Compile / doc / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq("-Ygenerate-inkuire") // type-based search for Scala 3, this option cannot go into compile
      case _ => Seq.empty
    }
  },
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  Test / compile / scalacOptions --= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq("-Ywarn-unused:locals") // Scala 2.12 ignores @unused warns
      case _             => Seq.empty
    }
  }
)

val hearthCompatSettings = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _))  => Seq.empty
      case Some((2, 13)) =>
        Seq(
          "-Wconf:origin=hearth.compat.*:s" // type aliases without which 2.12 fail compilation but 2.13/3 doesn't need them
        )
      case Some((2, 12)) => Seq.empty
      case _             => Seq.empty
    }
  }
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-collection-compat" % versions.scalaCollectionCompat,
    "org.scalameta" %%% "munit" % versions.munit % Test
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
          compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
        )
      case _ => Seq.empty
    }
  }
)

val versionSchemeSettings = Seq(versionScheme := Some("early-semver"))

val publishSettings = Seq(
  organization := "com.kubuszok",
  homepage := Some(url("https://scala-hearth.readthedocs.io")),
  organizationHomepage := Some(url("https://kubuszok.com")),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/MateuszKubuszok/hearth/"),
      "scm:git:git@github.com:MateuszKubuszok/hearth.git"
    )
  ),
  startYear := Some(2025),
  developers := List(
    Developer("MateuszKubuszok", "Mateusz Kubuszok", "", url("https://github.com/MateuszKubuszok"))
  ),
  pomExtra := (
    <issueManagement>
      <system>GitHub issues</system>
      <url>https://github.com/MateuszKubuszok/hearth/issues</url>
    </issueManagement>
  ),
  publishTo := {
    if (isSnapshot.value) Some(mavenCentralSnapshots)
    else localStaging.value
  },
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ =>
    false
  },
  // Sonatype ignores isSnapshot setting and only looks at -SNAPSHOT suffix in version:
  //   https://central.sonatype.org/publish/publish-maven/#performing-a-snapshot-deployment
  // meanwhile sbt-git used to set up SNAPSHOT if there were uncommitted changes:
  //   https://github.com/sbt/sbt-git/issues/164
  // (now this suffix is empty by default) so we need to fix it manually.
  git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty,
  git.uncommittedSignifier := Some("SNAPSHOT")
)

val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    val previousVersions = moduleName.value match {
      case "hearth-cross-quotes" | "hearth-compat" | "hearth-micro-fp" | "hearth" => Set() // add after RC-1 publish
      case "hearth-tests" | "hearth-sandwich-examples-213" | "hearth-sandwich-examples-3" | "hearth-sandwich-tests" =>
        Set()
      case name => sys.error(s"All modules should be explicitly checked or ignored for MiMa, missing: $name")
    }
    previousVersions.map(organization.value %% moduleName.value % _)
  },
  mimaFailOnNoPrevious := {
    moduleName.value match {
      case "hearth-cross-quotes" | "hearth-compat" | "hearth-micro-fp" | "hearth" => false // add after RC-1 publish
      case "hearth-tests" | "hearth-sandwich-examples-213" | "hearth-sandwich-examples-3" | "hearth-sandwich-tests" =>
        false
      case name => sys.error(s"All modules should be explicitly checked or ignored for MiMa, missing: $name")
    }
  }
)

val noPublishSettings =
  Seq(publish / skip := true, publishArtifact := false)

// Command generation

val al = new {

  private val prodProjects = Vector("hearthCrossQuotes", "hearthCompat", "hearthMicroFp", "hearth")
  private val testProjects = Vector("hearthTests", "hearthSandwichTests")

  private def isJVM(platform: String): Boolean = platform == "JVM"

  private def projects(platform: String, scalaSuffix: String): Vector[String] =
    for {
      name <- prodProjects ++ testProjects
      if (name != "hearthCrossQuotes" || isJVM(platform))
      if (name != "hearthSandwichTests") || (scalaSuffix != "2_12" && isJVM(platform))
    } yield s"$name${if (isJVM(platform)) "" else platform}$scalaSuffix"

  def ci(platform: String, scalaSuffix: String): String = {
    def tasksOf(name: String): Vector[String] = projects(platform, scalaSuffix).flatMap {
      case project if project.startsWith("hearthCrossQuotes") && Set("mimaReportBinaryIssues").contains(name) =>
        Vector()
      case project => Vector(s"$project/$name")
    }

    val clean = Vector("clean")
    val compileAndTest = tasksOf("compile") ++ tasksOf("test")
    val coverageCompileAndTest =
      if (isJVM(platform)) "coverage" +: compileAndTest :+ "coverageAggregate" :+ "coverageOff" else compileAndTest
    val mimaReport = tasksOf("mimaReportBinaryIssues")

    val tasks = clean ++ coverageCompileAndTest ++ mimaReport
    tasks.mkString(" ; ")
  }

  def test(platform: String, scalaSuffix: String): String =
    projects(platform, scalaSuffix).map(project => s"$project/test").mkString(" ; ")

  def release(tag: Seq[String]): String =
    if (tag.nonEmpty) "publishSigned ; sonaRelease" else "publishSigned"

  def publishLocal(platform: String, scalaSuffix: String): Vector[String] =
    for {
      name <- prodProjects
    } yield s"$name${if (isJVM(platform)) "" else platform}$scalaSuffix/publishLocal"

  val publishLocalForTests = (publishLocal("JVM", "") ++ publishLocal("JVM", "3")).mkString(" ; ")
}

// Modules

lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .settings(settings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(hearthCrossQuotes.projectRefs *)
  .aggregate(hearthCompat.projectRefs *)
  .aggregate(hearthMicroFp.projectRefs *)
  .aggregate(hearth.projectRefs *)
  .aggregate(hearthTests.projectRefs *)
  .aggregate(hearthSandwichTests.projectRefs *)
  .settings(
    moduleName := "hearth-build",
    name := "hearth-build",
    description := "Build setup for Hearth modules",
    logo :=
      s"""Hearth ${(version).value} build for (${versions.scala212}, ${versions.scala213}, ${versions.scala3}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.12 adds the suffix "2_12" to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         | - Scala 3 adds the suffix "3" to a project name seen in build.sbt
         |
         |When working with IntelliJ or Scala Metals, edit dev.properties to control which Scala version you're currently working with.
         |
         |If you need to test library locally in a different project, use publish-local-for-tests or manually publishLocal:
         | - hearth-cross-quotes (obligatory)
         | - hearth-compat (obligatory)
         | - hearth-micro-fp (obligatory)
         | - hearth (obligatory)
         |for the right Scala version and platform (see projects task).
         |""".stripMargin,
    usefulTasks := Seq(
      UsefulTask("projects", "List all projects generated by the build matrix").noAlias,
      UsefulTask(
        "test",
        "Compile and test all projects in all Scala versions and platforms (beware! it uses a lot of memory and might OOM!)"
      ).noAlias,
      UsefulTask(al.release(git.gitCurrentTags.value), "Publish everything to release or snapshot repository")
        .alias("ci-release"),
      UsefulTask(al.ci("JVM", "3"), "CI pipeline for Scala 3+JVM").alias("ci-jvm-3"),
      UsefulTask(al.ci("JVM", ""), "CI pipeline for Scala 2.13+JVM").alias("ci-jvm-2_13"),
      UsefulTask(al.ci("JVM", "2_12"), "CI pipeline for Scala 2.12+JVM").alias("ci-jvm-2_12"),
      UsefulTask(al.ci("JS", "3"), "CI pipeline for Scala 3+Scala JS").alias("ci-js-3"),
      UsefulTask(al.ci("JS", ""), "CI pipeline for Scala 2.13+Scala JS").alias("ci-js-2_13"),
      UsefulTask(al.ci("JS", "2_12"), "CI pipeline for Scala 2.12+Scala JS").alias("ci-js-2_12"),
      UsefulTask(al.ci("Native", "3"), "CI pipeline for Scala 3+Scala Native").alias("ci-native-3"),
      UsefulTask(al.ci("Native", ""), "CI pipeline for Scala 2.13+Scala Native").alias("ci-native-2_13"),
      UsefulTask(al.ci("Native", "2_12"), "CI pipeline for Scala 2.12+Scala Native").alias("ci-native-2_12"),
      UsefulTask(al.test("JVM", "3"), "Test all projects in Scala 3+JVM").alias("test-jvm-3"),
      UsefulTask(al.test("JVM", ""), "Test all projects in Scala 2.13+JVM").alias("test-jvm-2_13"),
      UsefulTask(al.test("JVM", "2_12"), "Test all projects in Scala 2.12+JVM").alias("test-jvm-2_12"),
      UsefulTask(al.test("JS", "3"), "Test all projects in Scala 3+Scala JS").alias("test-js-3"),
      UsefulTask(al.test("JS", ""), "Test all projects in Scala 2.13+Scala JS").alias("test-js-2_13"),
      UsefulTask(al.test("JS", "2_12"), "Test all projects in Scala 2.12+Scala JS").alias("test-js-2_12"),
      UsefulTask(al.test("Native", "3"), "Test all projects in Scala 3+Scala Native").alias("test-native-3"),
      UsefulTask(al.test("Native", ""), "Test all projects in Scala 2.13+Scala Native").alias("test-native-2_13"),
      UsefulTask(al.test("Native", "2_12"), "Test all projects in Scala 2.12+Scala Native").alias("test-native-2_12"),
      UsefulTask(
        al.publishLocalForTests,
        "Publishes all Scala 2.13 and Scala 3 JVM artifacts to test snippets in documentation"
      )
        .alias("publish-local-for-tests")
    )
  )

lazy val hearthCrossQuotes = projectMatrix
  .in(file("hearth-cross-quotes"))
  .someVariations(versions.scalas, versions.platforms)((defineCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, MimaPlugin)
  .settings(
    moduleName := "hearth-cross-quotes",
    name := "hearth-cross-quotes",
    description := "Utilities for hurting little kittens",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq("org.scala-lang" %% "scala3-compiler" % scalaVersion.value)
        case Some((2, _)) => Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value)
        case _            => ???
      }
    }
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)

lazy val hearthCompat = projectMatrix
  .in(file("hearth-compat"))
  .someVariations(versions.scalas, versions.platforms)((addScala213plusDir +: only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-compat",
    name := "hearth-compat",
    description := "Utilities for writing 2.12/2.13 code"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)

lazy val hearthMicroFp = projectMatrix
  .in(file("hearth-micro-fp"))
  .someVariations(versions.scalas, versions.platforms)((addScala213plusDir +: only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-micro-fp",
    name := "hearth-micro-fp",
    description := "Micro FP library, for using a few useful extension without fetching a whole FP library"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .dependsOn(hearthCompat)

lazy val hearth = projectMatrix
  .in(file("hearth"))
  .someVariations(versions.scalas, versions.platforms)((addScala213plusDir +: (only1VersionInIDE ++ useCrossQuotes)) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth",
    name := "hearth",
    description := "Utilities for writing cross-platform macro logic"
  )
  .settings(settings *)
  .settings(hearthCompatSettings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .dependsOn(hearthMicroFp)

// Test normal use cases

lazy val hearthTests = projectMatrix
  .in(file("hearth-tests"))
  .someVariations(versions.scalas, versions.platforms)((addScala213plusDir +: (only1VersionInIDE ++ useCrossQuotes)) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-tests",
    name := "hearth-tests",
    description := "Tests for hearth utilities"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .dependsOn(hearth)

// Test cross compilation: 2.13x3

lazy val hearthSandwichExamples213 = projectMatrix
  .in(file("hearth-sandwich-examples-213"))
  .someVariations(List(versions.scala213), List(VirtualAxis.jvm))()
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "hearth-sandwich-examples-213",
    name := "hearth-sandwich-examples-213",
    description := "Tests cases compiled with Scala 2.13 to test macros in 2.13x3 cross-compilation (non-publishable)"
  )

lazy val hearthSandwichExamples3 = projectMatrix
  .in(file("hearth-sandwich-examples-3"))
  .someVariations(List(versions.scala3), List(VirtualAxis.jvm))()
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "hearth-sandwich-examples-3",
    name := "hearth-sandwich-examples-3",
    description := "Tests cases compiled with Scala 3 to test macros in 2.13x3 cross-compilation (non-publishable)"
  )

lazy val hearthSandwichTests = projectMatrix
  .in(file("hearth-sandwich-tests"))
  .someVariations(List(versions.scala213, versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "hearth-sandwich-tests",
    name := "hearth-sandwich-tests",
    description := "Tests macros in 2.13x3 cross-compilation (non-publishable)"
  )
  .dependsOn(hearth % s"$Test->$Test;$Compile->$Compile")
  .dependsOn(hearthSandwichExamples213 % s"$Test->$Test;$Compile->$Compile")
  .dependsOn(hearthSandwichExamples3 % s"$Test->$Test;$Compile->$Compile")
  .settings(dependencies *)
  .dependsOn(hearth)

//when having memory/GC-related errors during build, uncommenting this may be useful:
Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Compile, 2) // only 2 compilations at once
)
