package org.labdb.labdb

import java.time.temporal.ChronoUnit
import javax.servlet.http.HttpSession

import org.json4s.{DefaultFormats, Formats}
import org.scalatra._
import org.scalatra.json._
import org.labdb.labdb.plasmidmapping.PlasmidMap

class LabDBServlet extends LabdbStack with JacksonJsonSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  def getSessionIdentity(session: HttpSession): Option[String] = {
    session.get("identity").flatMap {
      case s: String => Some(s)
      case _ => None
    }
  }

  before() {
    val usesTLSOnHeroku = request.headers.getOrElse("X-Forwarded-Proto", "http") == "https"
    if (!usesTLSOnHeroku && Env.isProd) {
      val newLocation = "https://" + request.getServerName + request.getRequestURI
      redirect(newLocation)
    }
  }

  get("*") {
    proxyRequest(getSessionIdentity(session))
  }
  post("*") {
    proxyRequest(getSessionIdentity(session))
  }
  put("*") {
    proxyRequest(getSessionIdentity(session))
  }
  delete("*") {
    proxyRequest(getSessionIdentity(session))
  }

  post("/api/v1/plasmid_map") {
    contentType = formats("json")
    status = 200
    val sequence = request.body
    PlasmidMap.createForSequence(sequence).features
  }

//  get("/api/v1/m/:modeltype/:id") {
//    status = 200
//    Resource.getItemById(params("modeltype"), params("id").toInt).toString
//  }

  post("/api/verify") {
    val email = Auth.getVerifiedIdentity(params("token"))
    // TODO(colin): why do I have to assign the session here?  It won't let me use it within the
    // `match`.
    val s = session
    email match {
      case Some(identity) => {
        s("identity") = identity
        SeeOther("/")
      }
      case None => Forbidden("Forbidden")
    }
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
