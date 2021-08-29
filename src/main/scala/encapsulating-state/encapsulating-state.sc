import cats.Functor
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

/**
 * Encapsulating State
 *
 * One of the best approaches to managing state is to encapsulate state in the interpreter and only expose an abstract
 * interface with the functionality the user needs.
 *
 * Our interface should know nothing about state.
 *
 * By doing so, we control exactly how the users interact with state.
 */

/**
 * In Memory Counter
 */

trait Counter[F[_]] {
  def incr: F[Unit]
  def get: F[Int]
}

object Counter {
  def make[F[_]: Functor: Ref.Make]: F[Counter[F]] =
    Ref.of[F, Int](0).map { ref =>
      new Counter[F] {
        def incr: F[Unit] = ref.update(_ + 1)

        def get: F[Int] = ref.get
      }
    }
}

val program = for {
  counter     <- Counter.make[IO]
  beforeCount <- counter.get
  _           <- counter.incr
  afterCount  <- counter.get

} yield (beforeCount, afterCount)

program.unsafeRunSync()

/**
 * This is a smart constructor, which makes it impossible for the Ref to be directly accessed from outside it.
 *
 * State should not leak!
 *
 * If we pass in Ref as a param, the state could be used elsewhere.
 *
 * Remember that a new Counter will be created on every flatMap call.
 */

/**
 * Alternatively use a Class with private attributes, so that you cannot directly instantiate the class.
 *
 * You can only instantiate the class within the companion object.
 */

object LiveCounter {
  def make[F[_]: Functor: Ref.Make]: F[Counter[F]] = Ref.of[F, Int](0).map(new LiveCounter[F](_))
}

class LiveCounter[F[_]] private (ref: Ref[F, Int]) extends Counter[F] {
  def incr: F[Unit] = ref.update(_ + 1)
  def get: F[Int]   = ref.get
}

val program1 = for {
  counter     <- LiveCounter.make[IO]
  beforeCount <- counter.get
  _           <- counter.incr
  afterCount  <- counter.get

} yield (beforeCount, afterCount)

program1.unsafeRunSync()

/**
 * This code will not compile.
 *
 * Error Message
 * constructor LiveCounter in class LiveCounter cannot be accessed in object AnObject from object AnObject
 */

//object AnObject {
//  def make[F[_]: Functor: Ref.Make]: F[Counter[F]] = Ref.of[F, Int](0).map(new LiveCounter[F](_))
//}

