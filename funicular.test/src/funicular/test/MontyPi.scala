package funicular.test

import funicular.Intrinsics._
import funicular.X10Application
import funicular.Config
import scala.util.Random

object MontyPi extends X10Application {
    def run(s: Array[String]) {
        println("begin")

        if (s.length != 1) {
            println("Usage: MontyPi <number of points>")
            exit(-1)
        }

        val P = Config.NTHREADS

        val N = s(0).toInt

        val initializer = (i:Int) => {
            val r = new Random
            var result = 0.0
            for (j <- 1 to N) {
                val x = r.nextDouble
                val y = r.nextDouble
                if (x*x + y*y <= 1.0)
                    result += 1.0
            }
            result
        }

        val result = new Array[Double](P).parInit(initializer)

        val pi = 4 * result.reduce((x:Double,y:Double) => x+y, 0) / (N*P)

        println("The value of pi is " + pi)
    }
}
