package funicular.test

import funicular.Intrinsics._

object Test1 {
  def main(args : Array[String]) : Unit = {
    finish {
      async {
        funicular.runtime.Runtime.sleep(50)
        println("Hello")
      }
      async {
        funicular.runtime.Runtime.sleep(50)
        println("World")
      }
    }
  }
}
