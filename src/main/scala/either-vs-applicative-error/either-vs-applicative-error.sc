import cats._
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.implicits._

import java.util.UUID
import scala.util.control.NoStackTrace

/**
 * Either Vs ApplicativeError
 */

/**
 * Errors Domain
 */
case class ARandomError(error: String) extends NoStackTrace

sealed trait UsersError extends NoStackTrace

case object UserNotFound    extends UsersError
case object UserCanNotSayHi extends UsersError

/**
 * Business Domain - User
 */

final case class User(id: UUID)

/**
 * Business Domain - Users
 */

trait Users[F[_]] {
  def findUser(id: UUID): F[User]
  def sayHi(id: UUID): F[Unit]
  def getName(id: UUID): F[String]
}

object Users {

  def make[F[_]: ApplicativeThrow](adminUser: User, canSayHi: Boolean): Users[F] =
    new Users[F] {
      def findUser(id: UUID): F[User] =
        if (id == adminUser.id) adminUser.pure[F]
        else UserNotFound.raiseError[F, User]

      def sayHi(id: UUID): F[Unit] =
        if (canSayHi) println("hiii").pure[F]
        else UserCanNotSayHi.raiseError[F, Unit]

      def getName(id: UUID): F[String] = "Sam Smith".pure[F]
    }
}

/**
 * Program1
 */

val adminUser = User(UUID.randomUUID)

// Change these guys and see how the program responds
val findUser = adminUser.id //adminUser.id OR User(UUID.randomUUID).id
val canSayHi = true

val users: Users[IO] = Users.make[IO](adminUser, canSayHi)

val program1: IO[String] = for {
  _     <- users.sayHi(adminUser.id)
  user_ <- users.findUser(findUser)
  name  <- users.getName(adminUser.id)
} yield name

val program1Attempt: IO[Either[Throwable, String]] = program1.attempt

/**
 * Program1 - With error handling
 */
// Try remove a case statement. Compile will complain about a non exhaustive list.
// Ie. you have not covered all possible subset errors of UsersError.
def handleError(error: UsersError): Unit =
  error match {
    case UserNotFound    => println("UserNotFound")
    case UserCanNotSayHi => println("UserCanNotSayHi")
  }

val program1WithErrorHandling: IO[Unit] = program1Attempt.map { (program1: Either[Throwable, String]) =>
  program1 match {
    case Right(name)               => println(name)
    case Left(error: UsersError)   => handleError(error)
    case Left(error: ARandomError) => println(s"A Random Error: $error")
    case Left(error)               => println(s"Dunno Error: $error")
  }
}

program1WithErrorHandling.unsafeRunSync()
