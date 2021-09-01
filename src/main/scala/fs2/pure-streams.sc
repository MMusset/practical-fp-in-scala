import fs2.{Pure, Stream}

val s0 = Stream.empty
s0.toList

val s1: Stream[Pure, Int] = Stream.emit(1)
s1.toList

val s1b: Stream[Pure, Int] = Stream.emits(List(1,2,3)) // accepts any Seq
s1b.toList

val s2: Stream[Pure, Int] = (Stream(1,2,3) ++ Stream(4,5))
s2.toList

val s3: Stream[Pure, Int] = Stream(1,2,3).map(_ + 1)
s3.toList

val s4: Stream[Pure, Int] = Stream(1,2,3).filter(_ % 2 != 0)
s4.toList

// Filters and maps simultaneously
val s5: Stream[Pure, Int] = Stream(None, Some(2), Some(3)).collect { case Some(i) => i }
s5.toList

// FlatMap
val s6: Stream[Pure, Int] = Stream(1, 2, 3).flatMap(i => Stream(i, i))
s6.toList

val s7: Stream[Pure, Int] = Stream(1,2,3).map(i => Stream(i, i)).flatten
s7.toList

// Repeat stream contents infinitely.
// Take the first 9 items from the stream
val s8: Stream[Pure, Int] = Stream(1,2,3).repeat.take(9)
s8.toList

// Repeat the stream contents 2 times.
val s9: Stream[Pure, Int] = Stream(1,2,3).repeatN(2)
s9.toList
