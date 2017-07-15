package org.labdb.labdb

import java.time.{ZoneId, ZonedDateTime}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import org.scalatra._
import org.slf4j.Logger

import scalaj.http.Http

trait LabdbStack extends ScalatraServlet {

  val logger: Logger = Config.logger
  val PROXY_SUFFIX = "-backend.labdb.io"
  val mac = javax.crypto.Mac.getInstance("HmacSHA256")

  def nowUTC: ZonedDateTime = ZonedDateTime.now(ZoneId.of("Z"))

  def proxyHost(currHost: String): String =
    if (Env.isDev) "localhost" else currHost.replaceAllLiterally(".labdb.io", "") + PROXY_SUFFIX
  def proxyPort: String = if (Env.isDev) ":3001" else ""
  def proxyProtocol: String = if (Env.isDev) "http://" else "https://"

  def proxyRequest(userId: Option[String])(implicit request: HttpServletRequest, response: HttpServletResponse): ActionResult = {
    mac.reset()
    mac.init(Config.signingKey)
    var proxiedUrl = proxyProtocol + proxyHost(request.getServerName) + proxyPort + request.getRequestURI
    val queryString = request.getQueryString
    if (queryString != null) {
      proxiedUrl += "?" + queryString
    }
    var proxiedRequest = Http(proxiedUrl)

    if (request.body != null && request.body.length > 0) {
      proxiedRequest = proxiedRequest.postData(request.body)
    }
    var headersForProxyRequest = request.headers
      .filter {
        case (key: String, _) => !key.toLowerCase.startsWith("cf-")
      } - "X-Forwarded-For"

    userId.foreach { userId: String =>
      val ts = java.time.OffsetDateTime.now(ZoneId.of("UTC")).toString
      val signature = javax.xml.bind.DatatypeConverter.printHexBinary(
        mac.doFinal((userId + ts).getBytes("UTF-8")))
      headersForProxyRequest = headersForProxyRequest
        .updated("X-LabDB-UserId", userId)
        .updated("X-LabDB-Signature", signature)
        .updated("X-LabDB-Signature-Timestamp", ts)
    }

    proxiedRequest = proxiedRequest
      .headers(headersForProxyRequest)
      .method(request.getMethod)

    val result = proxiedRequest.asString
    val headers = result.headers
      .map {
        // TODO(colin): is the last header what we want if there are repeated headers?
        case (key: String, values: IndexedSeq[String]) => (key, values.last)
      } - "Content-Encoding"

    if (List(301, 302, 303, 307) contains result.code) {
      // For redirect, we want to grab the relative url and then return our own
      // redirect.
      val newUrl = headers("Location").replaceAll("https?://[^/]+", "");
      val newHeaders = headers - "Location"
      TemporaryRedirect(
        newUrl,
        newHeaders)
    } else {
      ActionResult(
        ResponseStatus(result.code, ""),
        result.body,
        headers
      )
    }
  }

  notFound {
    // remove content type in case it was set through an action
    contentType = null
    resourceNotFound()
  }

  error {
    case e: Throwable =>
      val message: String = e.getStackTrace.foldLeft(e.getMessage) { (msg, tr) =>
        msg + "\n" + tr.toString
      }
      Config.logger.error(message)
      InternalServerError()
  }
}
