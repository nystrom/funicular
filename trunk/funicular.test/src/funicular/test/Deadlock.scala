package funicular.test

import funicular.Intrinsics._
import funicular.Clock
import funicular.X10Application
import funicular.Config
import scala.util.Random

object Deadlock extends X10Application {
    def run(s: Array[String]) = {
         println("main: before ClockTest")
         (new Hello).ClockTest()
         println("main: after ClockTest")
    }

    
    class Hello {
        def ClockTest() {
            finish {
                val c = Clock()
                for (p <- 0 until 8) { // Config.NTHREADS) {
                    async (c) {
                        println("Before next");
                        next;
                        println("After next");
                    }
                }
                c.drop
            }
        }
    }
}
