package securesocial.core.providers

import play.api.libs.ws.WS
import play.api.{Application, Logger}
import play.api.libs.json.JsObject
import securesocial.core._
import scala.concurrent.Future

/**
 * A Vk provider
 */
class VkProvider(application: Application) extends OAuth2Provider(application) {
  val GetProfilesApi = "https://api.vk.com/method/getProfiles?fields=uid,first_name,last_name,photo&access_token="
  val Response = "response"
  val Id = "uid"
  val FirstName = "first_name"
  val LastName = "last_name"
  val Photo = "photo"
  val Error = "error"
  val ErrorCode = "error_code"
  val ErrorMessage = "error_msg"

  def fillProfile(user: SocialUser) = {
    val accessToken = user.oAuth2Info.get.accessToken
    WS.url(GetProfilesApi + accessToken).get().map { response =>
      val json = response.json
      (json \ Error).asOpt[JsObject] match {
        case Some(error) =>
          val message = (error \ ErrorMessage).as[String]
          val errorCode = (error \ ErrorCode).as[Int]
          Logger.error("[securesocial] error retrieving profile information from Vk. Error code = %s, message = %s"
            .format(errorCode, message))
          throw new AuthenticationException()
        case _ =>
          val me = (json \ Response).apply(0)
          val userId = (me \ Id).as[Long]
          val firstName = (me \ FirstName).as[String]
          val lastName = (me \ LastName).as[String]
          val avatarUrl = (me \ Photo).asOpt[String]
          user.copy(
            identityId = IdentityId(userId.toString, id),
            firstName = firstName,
            lastName = lastName,
            fullName = firstName + " " + lastName,
            avatarUrl = avatarUrl,
            email = None
          )
      }
    } recoverWith {
      case e: Exception => {
        Logger.error("[securesocial] error retrieving profile information from VK", e)
        Future.failed(new AuthenticationException())
      }
    }
  }

  def id = VkProvider.Vk
}

object VkProvider {
  val Vk = "vk"
}
