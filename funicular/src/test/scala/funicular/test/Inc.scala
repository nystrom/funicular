package funicular.test

import funicular._

object Inc {
  class Incrementor {
       var i: Int = 0
       var incremented: Boolean = false

       def increment() = {
           println("Entering increment(): " + incremented)

           atomic {
           i += 1
           incremented = true
          }

           println("Leaving increment(): " + incremented)
       }
   }

   def func() = {
       val inc = new Incrementor()

       async(inc.increment)
       await(inc.incremented)

       println("It's done: " + inc.i)
   }

  def main(args : Array[String]) : Unit = finish {
    func()
  }
}
