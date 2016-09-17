package org.labdb.labdb

import org.scalatra._

class LabDBServlet extends LabdbStack {
  get("*") {proxyRequest(request)}
  post("*") {proxyRequest(request)}
  put("*") {proxyRequest(request)}
  delete("*") {proxyRequest(request)}

  get("/_s/*") {
    serveStaticResource() getOrElse halt(404, "Not found")
  }
}
