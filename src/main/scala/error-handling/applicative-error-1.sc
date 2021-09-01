import cats._
import cats.implicits._

import java.util.UUID

/**
 * ApplicativeError Cats Example
 */

sealed trait TwitterServiceError extends Throwable

case object NotFoundError extends TwitterServiceError

object Twitter {
  def find[F[_]](id: String)(implicit ae: ApplicativeError[F, TwitterServiceError]): F[String] =
    if (id == "123") ae.raiseError(NotFoundError)
    else ae.pure(id)
}

/**
 * ApplicativeError
 */

/**
 * Errors Domain
 */
case class ARandomError(error: String) extends Throwable

sealed trait UsersError extends Throwable

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

  def make[F[_]](adminUser: User, canSayHi: Boolean)(implicit ae: ApplicativeError[F, UsersError]): Users[F] =
    new Users[F] {
      def findUser(id: UUID): F[User] =
        if (id == adminUser.id) adminUser.pure[F]
        else ae.raiseError(UserNotFound)

      def sayHi(id: UUID): F[Unit] =
        if (canSayHi) println("hiii").pure[F]
        else ae.raiseError(UserCanNotSayHi)

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

val users = Users.make(adminUser, canSayHi)

val program1 = for {
  _     <- users.sayHi(adminUser.id)
  user_ <- users.findUser(findUser)
  name  <- users.getName(adminUser.id)
  tweet <- Twitter.find("456")
} yield name

/**
 * Program1 - With error handling
 */

def handleUsersError(error: UsersError): Unit             =
  error match {
    case UserNotFound    => println("UserNotFound")
    case UserCanNotSayHi => println("UserCanNotSayHi")
  }
def handleTwitterServiceError(error: TwitterServiceError) =
  error match {
    case NotFoundError => println("NotFoundError")
  }

// Try remove a case statement. Compile will complain about a non exhaustive list.
// Ie. you have not covered all possible subset errors of UsersError.
def handleError(error: Throwable): Unit =
  error match {
    case x: UsersError          => handleUsersError(x)
    case x: TwitterServiceError => handleTwitterServiceError(x)
  }

val program1WithErrorHandling: Unit =
  program1 match {
    case Right(name)     => println(name)
    case Left(throwable) => handleError(throwable)
  }

program1WithErrorHandling
