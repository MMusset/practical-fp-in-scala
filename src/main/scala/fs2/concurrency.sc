import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.{ Pure, Stream }

val s1: Stream[Pure, Int] = Stream(1, 2, 3)
val s2: Stream[IO, Int]   = Stream.eval {
  IO {
    Thread.sleep(200)
    4
  }
}

/**
 * The merge function runs two streams concurrently, combining their outputs.
 * It halts when both inputs have halted.
 *
 * Interleaves the two inputs non-deterministically.
 * The output stream halts after BOTH s1 and s2 terminate normally
 */
/**
 * The merge function runs two streams concurrently, combining their outputs.
 * It halts when both inputs have halted.
 *
 * Interleaves the two inputs non-deterministically.
 * The output stream halts after BOTH s1 and s2 terminate normally
 */
val mergedStream: Stream[IO, Int]   = s1.merge(s2)
val mergedStreamComp: IO[List[Int]] = mergedStream.compile.toList
mergedStreamComp.unsafeRunSync()

/**
 * As opposed to ++, which appends s2 to the end of this stream.
 *
 * (Stream(1,2,3) ++ Stream(4,5,6)).toList
 * res0: List[Int] = List(1, 2, 3, 4, 5, 6)
 */
val combine: Stream[IO, Int] = s1 ++ s2
