package org.labdb.labdb

object Env {
  def isProd: Boolean = !isDev
  def isDev: Boolean = sys.env contains "DEV"
}