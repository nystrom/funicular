package funicular.test

import funicular.Intrinsics._

object Test1 {
  def main(args : Array[String]) : Unit = {
    finish {
      async {
        println("Hello")
      }
      async {
        println("World")
      }
    }
  }
}
