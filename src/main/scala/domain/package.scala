import io.chrisdavenport.fuuid.FUUID
import shapeless.tag
import shapeless.tag.@@

package object domain {
  type UserId = FUUID @@ UserIdTag

  def tagFUUIDAsUserId(id: FUUID): UserId = tag[UserIdTag][FUUID](id)
}
