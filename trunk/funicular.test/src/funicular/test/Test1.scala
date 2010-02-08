package funicular.test

import funicular.Intrinsics._

object Test1 {
  def main(args : Array[String]) : Unit = {
    finish {
      async {
        sleep(50)
        println("Hello")
      }
      async {
        sleep(50)
        println("World")
      }
    }
  }
}
