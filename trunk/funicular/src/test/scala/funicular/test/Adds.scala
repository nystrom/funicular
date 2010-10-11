package funicular.test

import funicular._
import funicular.X10Application
import funicular.Config
import scala.util.Random
import compat.Platform._

object Adds extends X10Application {
    def add1 = {
      for (i <- 0 until size) {
        data(i) += 5
      }
    }
    def add2 = {
      finish {
        for (i <- 0 until size async) {
          data(i) += 5
        }
      }
    }
    def add2b = {
      finish {
        for (i <- 0 until size) async {
          data(i) += 5
        }
      }
    }
    def add3 = {
      val th = 2
      val sz = size / th
      finish {
        for (p <- 0 until th) async {
          for (i <- p*sz until ((p+1)*sz min size)) {
            data(i) += 5
          }
        }
      }
    }

    val size = 1000000
    val data = Array.ofDim[Int](size)

    def run(s: Array[String]) {
        val N = 100

        {
          val t0 = currentTime
          for (i <- 1 to N) {
            add1
          }
          val t1 = currentTime
          println("add1 " + (t1 - t0).toDouble / N)
        }
        {
          val t0 = currentTime
          for (i <- 1 to N) {
            add2
          }
          val t1 = currentTime
          println("add2 " + (t1 - t0).toDouble / N)
        }
        {
          val t0 = currentTime
          for (i <- 1 to N) {
            add2b
          }
          val t1 = currentTime
          println("add2b " + (t1 - t0).toDouble / N)
        }
        {
          val t0 = currentTime
          for (i <- 1 to N) {
            add3
          }
          val t1 = currentTime
          println("add3 " + (t1 - t0).toDouble / N)
        }
    }
}
