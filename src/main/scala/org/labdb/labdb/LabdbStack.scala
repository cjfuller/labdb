package org.labdb.labdb

import java.time.temporal.ChronoUnit
import java.time.{ZoneId, ZonedDateTime}
import java.util.Date
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import org.scalatra._
import org.slf4j.LoggerFactory

import scalaj.http.Http

trait LabdbStack extends ScalatraServlet {

  val logger = LoggerFactory.getLogger(getClass)
  val proxyHost = "http://localhost:3000"

  def nowUTC(): ZonedDateTime = {
    ZonedDateTime.now(ZoneId.of("Z"))
  }

  override def invoke(matchedRoute: MatchedRoute): Option[Any] = {
    val url = request.getRequestURL.toString
    val method = request.getMethod
    val startTs = nowUTC()
    val result = LabdbStack.super.invoke(matchedRoute)
    val stopTs = nowUTC()
    val status = result match {
      case Some(ActionResult(ResponseStatus(code, _), _, _)) => code
      case _ => response.status.code
    }
    val duration = startTs.until(stopTs, ChronoUnit.MILLIS)
    logger.info(
      s"$stopTs [$duration ms] $status $method $url"
    )
    result
  }

  def proxyRequest(implicit request: HttpServletRequest, response: HttpServletResponse): ActionResult = {
    var proxiedUrl = proxyHost + request.getRequestURI
    val queryString = request.getQueryString
    if (queryString != null) {
      proxiedUrl += "?" + queryString
    }
    var proxiedRequest = Http(proxiedUrl)

    if (request.body != null && request.body.length > 0) {
      proxiedRequest = proxiedRequest.postData(request.body)
    }
    proxiedRequest = proxiedRequest
      .headers(request.headers)
      .method(request.getMethod)


    val result = proxiedRequest.asString
    val headers = result.headers.map {
      // TODO(colin): is the last header what we want if there are repeated headers?
      case (key: String, values: IndexedSeq[String]) => (key, values.last)
    }

    if (List(301, 302, 303, 307) contains result.code) {
      // For redirect, we want to grab the relative url and then return our own
      // redirect.
      val newUrl = headers("Location").replaceAll(proxyHost, "")
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
}
