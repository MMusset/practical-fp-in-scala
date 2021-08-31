import cats.data.EitherT
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits.global
import cats.implicits._

import java.util.UUID
import scala.util.control.NoStackTrace

/**
 * Either Error
 */

/**
 * ApplicativeError
 */

/**
 * Errors Domain
 */
sealed trait UsersError extends NoStackTrace

case class UserNotFound(error: String)    extends UsersError
case class UserCanNotSayHi(error: String) extends UsersError
case class ARandomError(error: String)    extends UsersError

/**
 * Business Domain - User
 */

final case class User(id: UUID)

/**
 * Business Domain - Users
 */

trait Users[F[_], E] {
  def findUser(id: UUID): EitherT[F, Throwable, User]
  def sayHi(id: UUID): EitherT[F, Throwable, Unit]
  def getName(id: UUID): EitherT[F, Throwable, String]
}

object Users {

  def make[F[_]: Sync, E >: UsersError](adminUser: User, canSayHi: Boolean): Users[F, E] =
    new Users[F, E] {
      def findUser(id: UUID): EitherT[F, Throwable, User] = {
        val either: F[Either[Throwable, User]] =
          Sync[F].delay(if (id == adminUser.id) Right(adminUser) else Left(UserNotFound("UserNotFound")))

        EitherT[F, Throwable, User](either)
      }

      def sayHi(id: UUID): EitherT[F, Throwable, Unit] = {
        val either: F[Either[Throwable, Unit]] =
          Sync[F].delay(if (canSayHi) Right(println("hiii")) else Left(UserCanNotSayHi("UserNotFound")))

        EitherT[F, Throwable, Unit](either)
      }

      def getName(id: UUID): EitherT[F, Throwable, String] = {
        val either: F[Either[Throwable, String]] =
          Sync[F].delay(if (id == adminUser.id) Right("Sam Smith") else Left(ARandomError("UserNotFound")))

        EitherT[F, Throwable, String](either)
      }
    }
}

/**
 * Program1
 */

val adminUser = User(UUID.randomUUID)

// Change these guys and see how the program responds
val findUser = adminUser.id //adminUser.id OR User(UUID.randomUUID).id
val canSayHi = true

val users: Users[IO, UsersError] = Users.make[IO, UsersError](adminUser, canSayHi)

val program1EitherT: EitherT[IO, Throwable, String] = for {
  sayHi <- users.sayHi(adminUser.id)
  user_ <- users.findUser(findUser)
  name  <- users.getName(adminUser.id)
} yield name

val program1Either: IO[Either[Throwable, String]] = program1EitherT.value

val result: Either[Throwable, String] = program1Either.unsafeRunSync()

result match {
  case Right(res) => println(res)
  case Left(error) => println(error)
}
