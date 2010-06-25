package funicular.test

import funicular._
import scala.util.Random

object Stencil {
    def main (args: Array[String]) = finish {
        // 100 x 100 array of random points
        val rnd = new Random(0)
        val N = 100
        val A = Array.tabulate(N)(
                _ => Array.tabulate(N)(_ => rnd.nextFloat))

        val B = Array.ofDim[Float](N, N)

        var delta = Float.MaxValue
        var epsilon = 1e-4
        var iters = 0

        do {
            iters += 1
            for (i <- 1 until (N-1) async) {
                for (j <- 1 until (N-1) async) {
                    val t = ( A(i)(j-1) + A(i)(j+1) + A(i-1)(j) + A(i+1)(j) ) / 4.f
                    B(i)(j) = (A(i)(j) - t).abs
                    A(i)(j) = t
                }
            }

            // val D = B.max
            val D = B.mapPar[Float]((v: Array[Float]) => v.reduce(_ max _))
            delta = D.max // reduce((x:Float,y:Float) => x max y)
        } while (delta > epsilon);

        println("converged in " + iters + " iterations") 
    }
}
