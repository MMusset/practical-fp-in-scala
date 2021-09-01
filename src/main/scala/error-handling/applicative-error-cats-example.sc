import cats._
import cats.implicits._

import scala.util.control.NoStackTrace

/**
 * ApplicativeError Cats Example
 */

sealed trait UsersError extends NoStackTrace

case object UserNotFound    extends UsersError
case object UserCanNotSayHi extends UsersError

def attemptDivideApplicativeError[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, UsersError]): F[Int] =
  if (y == 0) ae.raiseError(UserNotFound)
  else ae.pure(x / y)

val program: Either[UsersError, Int] = attemptDivideApplicativeError(4, 0)

program
