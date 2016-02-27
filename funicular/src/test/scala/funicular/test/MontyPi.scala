package funicular.test

import funicular._
import funicular.Config
import scala.util.Random

object MontyPi {
    def run(s: Array[String]) {
        println("begin")

        if (s.length != 1) {
            println("Usage: MontyPi <number of points>")
            sys.exit(-1)
        }

        val P = 50

        val N = s(0).toInt

        val result = ParArray.tabulate[Double](P) {
            i => {
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
        }

        println(result.toList)

        val pi = 4 * result.sum / (N*P)

        println("The value of pi is " + pi)

        val spi = 4 * result.foldLeft[Double](0.0)(_+_) / (N*P)
        println("The value of sequential pi is " + spi)
    }

    def main(args: Array[String]) = finish { run(Array("100000")) }
}
