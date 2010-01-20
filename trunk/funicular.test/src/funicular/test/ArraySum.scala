package funicular.test

import funicular.Intrinsics._

/**
 * A simple illustration of loop parallelization within a single place.
 * @author ??
 * @author vj
 */

object ArraySum {

  class ArraySum(size: Int) {
    var sum: Int = 0
    val data = Array.make(size, 1)

    def sum(a: Array[Int], start: Int, last: Int): Int = {
        var mySum: Int = 0
        for (i <- start until last) 
            mySum += a(i)
        mySum
    }

    def sum(numThreads: Int) {
        val mySize = size/numThreads
        finish {
            foreach (0 until numThreads) {
                p => {
                    val mySum = sum(data, p*mySize, (p+1)*mySize)
                    // Multiple activities will simultaneously update
                    // this location -- so use an atomic operation.
                    atomic { sum += mySum }
                }
            }
        }
    }
  }
    
    def main(args: Array[String]) = {

        val size = if (args.length >=1) args(0).toInt
                        else 5*1000*1000

        println("Initializing.")
        val a = new ArraySum(size)
        val P = List(1,2,4)

        //warmup loop
        println("Warming up.")
        for (p <- P)
            a.sum(p)
        
        for (p <- P) {
            println("Starting with " + p + " threads.")
            a.sum = 0
            var time: Long = - System.nanoTime()
            a.sum(p)
            time += System.nanoTime()
            println("For p=" + p
                    + " result: " + a.sum 
                    + (if (size==a.sum) " ok" else "  bad") 
                    + " (time=" + (time/(1000*1000)) + " ms)")
        }
        
        
    }
}
