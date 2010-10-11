package funicular.test

import funicular._

object Stream {

    def main(args:Array[String]): Unit = finish {
        val s = new Stream()
        val result = s.once
        println("got " + result + "; expected " + expected)
    }

    val alpha = 1.5
    val beta = 2.5
    val gamma = 3.0

    val NUM_TIMES = 10
    val PARALLELISM = 2
    val localSize = 512*1024

    def operations = 1.0 * localSize * PARALLELISM * NUM_TIMES
    def expected = (localSize+1)*(alpha+gamma*beta)

class Stream {
    //
    //
    //

    val as = Array.tabulate(PARALLELISM)((p:Int) => new Array[Double](localSize))
    
    val bs = Array.tabulate(PARALLELISM)((p:Int) => Array.tabulate(localSize)((i:Int) => alpha*(p*localSize+i)))

    val cs = Array.tabulate(PARALLELISM)((p:Int) => Array.tabulate(localSize)((i:Int) => beta*(p*localSize+i)))

    def once = {
        finish {
            for (p <- 0 until PARALLELISM) {
                val a = as(p)
                val b = bs(p)
                val c = cs(p)
                async {
                    for (tt <- 0 until NUM_TIMES)
                        for (i <- 0 until localSize)
                            a(i) = b(i) + gamma*c(i)
                }
            }
        }
        as(1)(1)
    }
}
}

