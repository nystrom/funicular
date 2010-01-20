package x10test

import x10.Intrinsics._

object Test1 {
  def main(args : Array[String]) : Unit = {
    finish {
      async {
        x10.runtime.Runtime.sleep(50)
        println("Hello")
      }
      async {
        x10.runtime.Runtime.sleep(50)
        println("World")
      }
    }
  }
}
