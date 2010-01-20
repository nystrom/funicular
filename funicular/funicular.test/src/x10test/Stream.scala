package x10test

import x10.Intrinsics._

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

    val as = Array.fromFunction((p:Int) => new Array[Double](localSize))(PARALLELISM)
    
    val bs = Array.fromFunction((p:Int) => Array.fromFunction((i:Int) => alpha*(p*localSize+i))(localSize))(PARALLELISM)

    val cs = Array.fromFunction((p:Int) => Array.fromFunction((i:Int) => beta*(p*localSize+i))(localSize))(PARALLELISM)

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

