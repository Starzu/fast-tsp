import sbt._

object Dependencies {
  private val scalaLoggerVersion = "3.9.2"
  private val logbackVersion = "1.2.3"
  private val groovyVersion = "2.4.13"

  val deps = Def.setting(Seq[ModuleID](
    "org.codehaus.groovy" % "groovy-all" % groovyVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion,
    "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggerVersion,
  ))
}
