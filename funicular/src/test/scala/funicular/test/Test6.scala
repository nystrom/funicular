package funicular.test

import funicular._

object Test6 {
  def main(args : Array[String]) : Unit = {
    finish {
      val A = new Array[Double](100).parInit(i => i)
      println("created A")
      println(A.reduce(_+_))
    }
  }
}
