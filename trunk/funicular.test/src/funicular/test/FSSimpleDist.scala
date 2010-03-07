package funicular.test

import funicular.Intrinsics._

/**
 * Version of Stream with a collection of local arrays implementing a
 * global array.
 *
 * @seealso Stream
 * @author vj
 * @author bdlucas
 */

object FSSimpleDist {

    val MEG = 1024*1024
    val alpha = 3.0D

    val NUM_TIMES = 10

    val DEFAULT_SIZE = MEG / 8

    val NUM_PLACES = 8

    def main(args:Array[String]) {
        var verified = true
        val times = new Array[Double](NUM_TIMES)
          val N0 = if (args.length>0) args(0).toInt else DEFAULT_SIZE
        val N = N0 * NUM_PLACES
        val localSize = N0

        println("localSize=" + localSize)

        finish {
            for (p <- 0 until NUM_PLACES) async {
                val a = new Array[Double](localSize)
                val b = new Array[Double](localSize)
                val c = new Array[Double](localSize)
                
                for (i <- 0 until localSize) {
                    b(i) = 1.5 * (p*localSize+i)
                    c(i) = 2.5 * (p*localSize+i)
                }
                
                for (j <- 0 until NUM_TIMES) {
                    if (p==0)
                            times(j) = -now()
                    for (i <- 0 until localSize)
                        a(i) = b(i) + alpha*c(i)
                    if (p==0)
                            times(j) = times(j) + now()
                }
                
                // verification
                for (i <- 0 until localSize)
                    if (a(i) != b(i) + alpha*c(i)) 
                        verified = false
            }
        }

        var min:Double = 1000000
        for (j <- 0 until NUM_TIMES)
            if (times(j) < min)
                min = times(j)
        printStats(N, min, verified)
    }

    def now():Double = System.nanoTime() * 1e-9

    def printStats(N:Int, time:Double, verified:Boolean) {
        val size = (3*8*N/MEG)
        val rate = (3*8*N) / (1.0E9*time)
        println("Number of places=" + NUM_PLACES)
        println("Size of arrays: " + size +" MB (total)" + size/NUM_PLACES + " MB (per place)")
        println("Min time: " + time + " rate=" + rate + " GB/s")
        println("Result is " + (if (verified) "verified." else "NOT verified."))
    }                                
}
