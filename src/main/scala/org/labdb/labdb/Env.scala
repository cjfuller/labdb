package org.labdb.labdb

import java.security.Key
import javax.crypto.spec.SecretKeySpec

import org.slf4j.{Logger, LoggerFactory}

object Env {
  def isProd: Boolean = !isDev
  def isDev: Boolean = sys.env contains "DEV"
}

object Config {
  val logger: Logger = LoggerFactory.getLogger(getClass)
  lazy val signingKey: Key = {
    val keyStr = if (Env.isDev) "development-key" else System.getenv("SIGNING_KEY")
    new SecretKeySpec(keyStr.getBytes("UTF-8"), "HmacSHA256")
  }
}