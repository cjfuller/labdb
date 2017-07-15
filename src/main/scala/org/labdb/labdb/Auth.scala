package org.labdb.labdb

import org.json4s.{DefaultFormats, Formats}

import scalaj.http.Http
import org.json4s.jackson.JsonMethods.parse

case class AuthResponse(aud: String, email_verified: String, email: String)

object Auth {
  protected implicit val jsonFormats: Formats = DefaultFormats
  val APP_ID = "146923434465-alq7iagpanjvoag20smuirj0ivdtfldk.apps.googleusercontent.com"
  val ENDPOINT = "https://www.googleapis.com/oauth2/v3/tokeninfo"

  def getVerifiedIdentity(token: String): Option[String] = {
    val response = Http(ENDPOINT)
      .params("id_token" -> token)
      .asString
    if (!response.is2xx) { return None }
    val authInfo = parse(response.body).extract[AuthResponse]
    if (authInfo.aud.contains(APP_ID) && authInfo.email_verified == "true") {
      return Some(authInfo.email)
    }
    None
  }
}