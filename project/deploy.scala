import java.io.PrintWriter

import scala.io.Source
import scala.sys.process._

class ShellCommandFailed(msg: String) extends Exception(msg) {}

// TODO(colin): use scala.io.AnsiColor once sbt uses 2.11+
object Color {
  val GREEN = "\033[32m"
  val RED = "\033[31m"
  val BOLD = "\033[1m"
  val RESET = "\033[0m"
  val WHITE = "\033[37m"
}

object Deploy {
  val UPDATE_STAGES = List(
    "./labdb_web_deployment.yaml"
  )

  val DEPLOY_STAGES = List(
    "./postgres_controller.yaml",
    "./postgres_service.yaml",
    "./labdb_web_deployment.yaml",
    "./labdb_web_service.yaml"
  )

  val INJECT_VERSION_REPLACE = "<VERSION>"
  val INJECT_LAB_REPLACE = "<LAB>"
  val DOCKER_REGISTRY_NAME = "us.gcr.io"
  val PROJECT_NAME = "labdb-io"
  val CONTAINER_NAME = "labdb_web"

  val LAB_TEMPLATES = List(
    "./labdb_web_deployment.yaml",
    "./labdb_web_service.yaml",
    "./postgres_controller.yaml",
    "./postgres_service.yaml",
    "./config/database.yml"
  )

  val TEMPLATE_SUFFIX = ".template"

  lazy val PREVIOUS_VERSION = {
    val re = "labdb-web-([0-9a-f]*)".r
    "kubectl get services".!! match {
      case re(sha) => Some(sha)
      case _ => None
    }
  }

  lazy val NEXT_VERSION = "git log -n 1 --format=%h".!!.trim
  val CONTAINER_PREFIX = s"$DOCKER_REGISTRY_NAME/$PROJECT_NAME/$CONTAINER_NAME"

  def green(arg: String): String = Color.GREEN + arg + Color.RESET

  def red(arg: String): String = Color.RED + arg + Color.RESET

  def bold(arg: String): String = Color.WHITE + Color.BOLD + arg + Color.RESET

  def prevVersion(): String = {
    val pods = "kubectl -o yaml get pods".!!
    val re = s"$CONTAINER_PREFIX:([0-9a-f]+)".r
    pods match {
      case re(podName) => podName
      case _ => throw new IllegalStateException("No previous version available")
    }
  }

  def cmd(args: Seq[String]): Unit = {
    println(bold(s"Running: $args"))
    args.! match {
      // TODO(colin): use Nothing on 2.11
      case 0 => Nil
      case s => throw new ShellCommandFailed(red(s"$args exited with status $s"))
    }
    println(green("-> OK"))
  }

  def containerName(version: String): String =
    s"$CONTAINER_PREFIX:$version"

  def beforeDeploy(version: String): Unit = {
    cmd(Seq("gcloud", "config", "set", "project", PROJECT_NAME))
    cmd(Seq("npm", "install"))
    cmd(Seq("npm", "run-script", "coffee-compile"))
    cmd(Seq("npm", "run-script", "compile"))
    cmd(Seq("mv", "src/main/webapp/_s/app_.js", s"src/main/webapp/_s/app_$version.js"))
    println(bold("Writing version to config/version.txt"))
    val output = new java.io.PrintWriter("config/version.txt")
    output.print(version)
    output.close()
    println(green("-> OK"))
  }

  def dockerBuild(version: String): Unit = {
    cmd(Seq("docker", "build", "-t", containerName(version), "."))
    cmd(Seq("gcloud", "--project", PROJECT_NAME, "docker", "push", containerName(version)))
  }

  def interpolateTemplate(destFilename: String, version: String, lab: String): Unit = {
    val sourceFilename = destFilename + TEMPLATE_SUFFIX
    println(s"interpolating $sourceFilename to $destFilename")
    val interpolated = Source.fromFile(sourceFilename)
      .mkString
      .replaceAll(INJECT_LAB_REPLACE, lab)
      .replaceAll(INJECT_VERSION_REPLACE, version)
    val out = new PrintWriter(destFilename)
    out.println(interpolated)
    out.close()
  }

  def interpolateTemplates(version: String, lab: String): Unit =
    LAB_TEMPLATES.foreach(interpolateTemplate(_, version, lab))

  def doUpdate(version: String): Unit = {
    UPDATE_STAGES.foreach(f => {
        cmd(Seq("kubectl", "replace", "-f", f))
    })
  }

  case class CLOptions(
    lab: Option[String] = None,
    templatesOnly: Boolean = false
  )

  def parseOpts(args: List[String], opts: CLOptions): CLOptions = args match {
    case Nil => opts
    case "--lab" :: lab :: rest => parseOpts(rest, opts.copy(lab = Some(lab)))
    case "--templates-only" :: rest => parseOpts(rest, opts.copy(templatesOnly = true))
    case arg => throw new IllegalArgumentException(s"Invalid argument $arg")
  }

  def main(args: Seq[String]): Unit = {
    val version = NEXT_VERSION
    val opts = parseOpts(args.toList, CLOptions())
    val lab = opts.lab match {
      case Some(l) => l
      case None => throw new IllegalArgumentException("Must provide a lab using --lab <lab>")
    }
    interpolateTemplates(version, lab)
    if (!opts.templatesOnly) {
      beforeDeploy(version)
      dockerBuild(version)
      doUpdate(version)
    }
  }
}

