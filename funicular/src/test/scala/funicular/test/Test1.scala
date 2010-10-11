package funicular.test

import funicular._

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
