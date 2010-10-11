package funicular.test

import funicular._
import funicular.Clock

object Test5 {
  val N = 1000
  val K = 1000

  class C {
    var n = 0

    def run = {
      foreach (0 until N) {
          _ => { atomic { n += 1 } }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    var errors = 0
    for (i <- 0 until K) {
      val c = new C
      finish { c.run }
      while (c.n != N) {
          errors += 1
    	  println(i + " " + c.n)
    	  funicular.runtime.Runtime.parkNanos(100)
      }
      assert(c.n == N, "got " + c.n + " not " + N + " at " + i)
    }
    println("errors = " + errors)
  }
}
