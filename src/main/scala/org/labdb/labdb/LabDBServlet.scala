package org.labdb.labdb

import org.scalatra._

class LabDBServlet extends LabdbStack {
  get("*") {proxyRequest}
  post("*") {proxyRequest}
  put("*") {proxyRequest}
  delete("*") {proxyRequest}

  get("/_s/*") {
    serveStaticResource() getOrElse halt(404, "Not found")
  }
}
