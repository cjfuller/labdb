package org.labdb.labdb

import java.time.temporal.ChronoUnit

import org.json4s.{DefaultFormats, Formats}
import org.scalatra._
import org.scalatra.json._
import org.labdb.labdb.plasmidmapping.PlasmidMap

class LabDBServlet extends LabdbStack with JacksonJsonSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  before() {
    val usesTLSOnHeroku = request.headers.getOrElse("X-Forwarded-Proto", "http") == "https"
    if (!usesTLSOnHeroku && Env.isProd) {
      val newLocation = "https://" + request.getServerName + request.getRequestURI
      redirect(newLocation)
    }
  }

  get("*") {proxyRequest}
  post("*") {proxyRequest}
  put("*") {proxyRequest}
  delete("*") {proxyRequest}

  post("/api/v1/plasmid_map") {
    contentType = formats("json")
    status = 200
    val sequence = request.body
    PlasmidMap.createForSequence(sequence).features
  }

  // TODO: JacksonJsonSupport overrides this, so it has to be here, but I'd rather
  // it be somewhere else.  Rearrange.
  override def invoke(matchedRoute: MatchedRoute): Option[Any] = {
    val url = request.getRequestURL.toString
    val method = request.getMethod
    val startTs = nowUTC
    val result = super.invoke(matchedRoute)
    val stopTs = nowUTC
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
}
