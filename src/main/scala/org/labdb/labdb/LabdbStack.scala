package org.labdb.labdb

import java.time.temporal.ChronoUnit
import java.time.{ZoneId, ZonedDateTime}
import java.util.Date
import javax.servlet.http.HttpServletRequest

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

  def proxyRequest(request: HttpServletRequest): ActionResult = {
    var url = proxyHost + request.getRequestURI
    val queryString = request.getQueryString
    if (queryString != null) {
      url += "?" + queryString
    }
    var proxiedRequest = Http(url)

    if (request.body != null && request.body.length > 0) {
      proxiedRequest = proxiedRequest.postData(request.body)
    }
    proxiedRequest = proxiedRequest
      .headers(request.headers)
      .method(request.getMethod)


    val result = proxiedRequest.asString

    ActionResult(
      ResponseStatus(result.code, ""),
      result.body,
      result.headers.map {
        // TODO(colin): is the last header what we want if there are repeated headers?
        case (key: String, values: IndexedSeq[String]) => (key, values.last)
      }
    )
  }

  notFound {
    // remove content type in case it was set through an action
    contentType = null
    resourceNotFound()
  }
}
