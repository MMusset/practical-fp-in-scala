import cats.effect.IO
import cats.effect.kernel.Sync
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.types.string.NonEmptyString

/**
 * Refinement Types
 *
 * Allow us to validate input data at compile time as well as at runtime.
 */

final case class User(username: String)

/** NonEmptyString */

def lookup1[F[_]: Sync](username: NonEmptyString): F[Option[User]] = Sync[F].pure(Some(User(username)))

//lookup1[IO]("") // fail
lookup1[IO]("aeinstein")
lookup1[IO]("csagan")

/** Username */

type Username = String Refined Contains['g']

def lookup2[F[_]: Sync](username: Username): F[Option[User]] = Sync[F].pure(Some(User(username)))

//lookup2[IO]("") // fail
//lookup2[IO]("aeinstein") // fail
lookup2[IO]("csagan")


