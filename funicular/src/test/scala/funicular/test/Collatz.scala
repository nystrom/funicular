package funicular.test

import funicular._

// Take any natural number n. If n is even, divide it by 2 (n / 2), otherwise
// multiply it by 3 and add 1 to obtain 3n + 1. The conjecture is that for all
// numbers this process converges to 1.
object Collatz {
  def main(args : Array[String]) : Unit = {
    finish {
      val N = args(0).toInt

      val A = Array.fill[Future[List[Int]]](10000)(null)

      for (i <- 1 until A.length) {
          A(i) = delayedFuture[List[Int]] {
              compute(A, i)
          }
      }

      // for (i <- 1 to N) {
          // A(i).start
      // }

      for (i <- 1 to N) {
        println(i + " --> " + A(i).force)
      }
    }
  }

  def compute(A: Array[Future[List[Int]]], i: Int): List[Int] = {
    try {
      println("computing " + i)

    // Don't do 1 4 2 1 4 2 1 4 2 1 ...
    if (i == 1)
      return 1::Nil
    if (i == 2)
      return 2::1::Nil
    if (i == 4)
      return 4::2::1::Nil

    if (i % 2 == 0)
      println("forcing " + (i/2) + " for " + i)
    else
      println("forcing " + (i*3+1) + " for " + i)

    if (i % 2 == 0)
      // i::compute(A, i/2)
      i::(A(i/2).force)
    else
      i::(A(i*3+1).force)

  } finally println("done computing " + i)
  }
}
