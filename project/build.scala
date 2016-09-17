import sbt._
import Keys._
import org.scalatra.sbt._
import com.earldouglas.xwp.JettyPlugin
import com.earldouglas.xwp.JettyPlugin.autoImport._
import com.earldouglas.xwp.ContainerPlugin.autoImport._
import sbtassembly.AssemblyPlugin.autoImport._

object LabdbBuild extends Build {
  val Organization = "org.labdb"
  val Name = "LabDB"
  val Version = "2.0.0-pre0"
  val ScalaVersion = "2.11.8"
  val ScalatraVersion = "2.4.1"

  lazy val project = Project (
    "labdb",
    file("."),
    settings = ScalatraPlugin.scalatraSettings ++ Seq(
      organization := Organization,
      name := Name,
      version := Version,
      scalaVersion := ScalaVersion,
      resolvers += Classpaths.typesafeReleases,
      resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
      libraryDependencies ++= Seq(
        "org.scalatra" %% "scalatra" % ScalatraVersion,
        "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
        "ch.qos.logback" % "logback-classic" % "1.1.5" % "runtime",
        "org.eclipse.jetty" % "jetty-webapp" % "9.3.11.v20160721" % "container;compile",
        "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
        "org.scalaj" %% "scalaj-http" % "2.3.0"
      ),
      containerPort in Jetty := 4000,
      mainClass in assembly := Some("org.labdb.labdb.JettyLauncher"),
      assemblyJarName in assembly := s"labdb.jar"
    )
  ).enablePlugins(JettyPlugin)
}
