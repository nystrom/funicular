package funicular.test

import funicular._
import funicular.X10Application
import funicular.Config
import scala.util.Random
import compat.Platform._

object Sums extends X10Application {
    def sum(lo: Int, hi: Int, f: Int => Int) = {
      var s = 0
      for (i <- lo to hi)
        s += f(i)
      s
    }

    def run(s: Array[String]) {
        val sums = Array.ofDim[Int](2)

        finish {
          async {
            sums(0) = sum(1, 100, i => i*i)
          }
          async {
            sums(1) = sum(1, 1000, i => i)
          }
        }

        val t = sums(0) + sums(1)
        println(t)
    }
}
