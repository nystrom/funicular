package x10test

import x10.Intrinsics._
import x10.Clock

object Test4 {
  def main(args: Array[String]): Unit = {
    finish {
      println("0 started")
      val c = Clock()
      println("c = " + c)

      async (c) {
      println("1 started")
        next
        for (i <- 0 to 9) {
            println("-- a" + i)
            next
            next
        }
      }

      async (c) {
      println("2 started")
        next
        for (i <- 0 to 9) {
            next
            println("b" + i + " --")
            next
        }
      }

      c.drop
      println("0 done")
    }
  }
}
