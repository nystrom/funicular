package funicular.test

import funicular.Intrinsics._

object Test7 {
  def main(args : Array[String]) : Unit = {
    finish {
      val A = new Array[Int](100).parInit(i => i)
      /*
      for (ai <- A.inParallel) {
          println(ai)
      }
      println("----------------------------------")
      */
      for (ai <- A.inParallel if ai % 2 == 0) {
          println(ai)
      }
      println("----------------------------------")
    }
  }
}