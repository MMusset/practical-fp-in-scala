import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.{ Pure, Stream }

// Creates a singleton pure stream that emits the supplied value.
val s1: Stream[Pure, Int] = Stream.emit(1)
s1.toList

/**
 * This effect, executes a side effect by printing and then returns a number as a result. Imagine a database INSERT,
 * that returns a 1 for success or 0 for failure.
 */
val eff1: IO[Int] = IO {
  println("BEING RUN!!")
  1 + 1
}

/**
 * Notice this stream in not pure.
 *
 * val s1: Stream[Pure, Int] = Stream.emit(1) val es1: Stream[IO, Int] = Stream.eval(eff1)
 *
 * This is a stream which contains multiple IOs. They are descriptions of side effects, but do not execute. ie.
 * Stream(getDbRecordIO, insertDbRecordIO, getDbRecordIO)
 */
val effs1: Stream[IO, Int] = Stream.eval(eff1)

// 'compile' the stream to a single effect.
val effsComp1: IO[List[Int]] = effs1.compile.toList
effsComp1.unsafeRunSync()

// We don't care about the Stream return value.
// Purely a Stream of effects.
val effsComp2: IO[Unit] = effs1.compile.drain
effsComp2.unsafeRunSync()

// Run and accumulate some result
val effsComp3: IO[Int] = effs1.compile.fold(0)(_ + _)
effsComp3.unsafeRunSync()
