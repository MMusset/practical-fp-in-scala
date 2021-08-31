import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxApplicativeId

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
case class ARandomError(error: String) extends NoStackTrace

sealed trait UsersError extends NoStackTrace

case class UserNotFound(error: String)    extends NoStackTrace
case class UserCanNotSayHi(error: String) extends NoStackTrace

/**
 * Business Domain - User
 */

final case class User(id: UUID)

/**
 * Business Domain - Users
 */

trait Users[F[_]] {
  def findUser(id: UUID): F[Either[UserNotFound, User]]
  def sayHi(id: UUID): F[Either[UserCanNotSayHi, Unit]]
  def getName(id: UUID): F[String]
}

object Users {

  def make[F[_]: Sync](adminUser: User, canSayHi: Boolean): Users[F] =
    new Users[F] {
      def findUser(id: UUID): F[Either[UserNotFound, User]] = {
        if (id == adminUser.id) Sync[F].delay(Right(adminUser))
        else Sync[F].pure(Left(UserNotFound("UserNotFound")))
      }

      def sayHi(id: UUID): F[Either[UserCanNotSayHi, Unit]] =
        if (canSayHi) Sync[F].delay(Right(println("hiii")))
        else Sync[F].pure(Left(UserCanNotSayHi("UserNotFound")))

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

// Chaos, when dependancy of side effects is involved
val program1: IO[Object] = for {
  user <- users.findUser(findUser)
  result = user match {
    case Right(user) => users.sayHi(user.id).map { sayHiEither: Either[UserCanNotSayHi, Unit] =>
      sayHiEither match {
        case Right(unit) => Right(unit)
        case Left(userCanNotSayHiError) => Left(userCanNotSayHiError)
      }
    }
    case Left(error) => Left(error)
  }
} yield result

program1.unsafeRunSync()

// Explicit Error Handling
// When independant side effects
val program2 = {
  users.findUser(findUser).flatMap { eitherA: Either[UserNotFound, User] =>
    users.sayHi(findUser).flatMap { eitherB: Either[UserCanNotSayHi, Unit] =>
      (eitherA, eitherB) match {
        case (Right(a), Right(b)) => IO(Right(println("success")))
        case (Left(error), Right(b)) => IO(Left(println("eitherA fail")))
        case (Right(a), Left(error)) => IO(Left(println("eitherB fail")))
        case (Left(leftError), Left(rightError)) => IO(Left(println("Both fail")))
      }
    }
  }
}

program2.unsafeRunSync()
