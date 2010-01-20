package funicular.test

import funicular.Intrinsics._
import scala.util.Random;

/**
 * A KMeans object o can compute K means of a given set of 
 * points of dimension o.myDim.
 * <p> 
 * This class implements a sequential program, that is readily parallelizable.
 * 
 * @author cunningham
 * @author vj 
 */

object KMeans {
    val DIM=2
    val K=4
    val POINTS=2000
    val ITERATIONS=50
    val EPS=0.01F

    type ValVector = Array[Float]
    type Vector = Array[Float]
    type SumVector = V

    /**
     * V represents the sum of 'count' number of vectors of dimension 'dim'.
     */
    class V(dim:Int) {
        var vec: Vector = null
        var count: Int = 0

        def this(dim:Int, init:(Int)=>Float) {
            this(dim)
            vec = Array.fromFunction(init)(dim)
            count = 0
        }

        def apply(i:Int) = vec(i)

        def makeZero = {
            for (i <- 0 until dim)
                vec(i) = 0.0F
            count=0
            this
        }
        def addIn(a:ValVector) = {
            for (i <- 0 until dim)
                vec(i) += a(i)
            count += 1
            this
        }
        def div(f:Int) = {
            for (i <- 0 until dim)
                vec(i) /= f
            this
        }
        def dist(a:ValVector):Float = {
            var dist:Float=0.0F
            for (i <- 0 until dim) {
                val tmp = vec(i)-a(i)
                dist += tmp*tmp
            }
            dist
        }
        def dist(a:SumVector):Float = {
            var dist:Float=0.0F
            for (i <- 0 until dim) {
                val tmp = vec(i)-a(i)
                dist += tmp*tmp
            }
            dist
        }
        def print = {
            for (i <- 0 until dim) {
                if (i>0) System.out.print(" ")
                System.out.print(vec(i))
            }
            println()
        }
        def normalize = div(count)
    }
    
class KMeans(myDim:Int) {
    
     type KMeansData= Array[SumVector]

    /**
     * Compute myK means for the given set of points of dimension myDim.
     */

    def computeMeans(myK:Int, points: Array[ValVector]): KMeansData = {
        var redCluster : KMeansData =
            Array.fromFunction((i:Int)=> new V(myDim, (j:Int)=>points(i)(j)))(myK)
        var blackCluster : KMeansData =
            Array.fromFunction((i:Int)=> new V(myDim, (j:Int)=>0.0F))(myK)

        for (i <- 1 to ITERATIONS) {
            val tmp = redCluster
            redCluster = blackCluster
            blackCluster = tmp
            for (p <- 0 until POINTS) {
                var closest:Int = -1
                var closestDist:Float = Float.MaxValue
                val point = points(p)
                for (k <- 0 until myK) { // compute closest mean in cluster.
                    val dist = blackCluster(k).dist(point)
                    if (dist < closestDist) {
                        closestDist = dist
                        closest = k
                    }
                }
                redCluster(closest).addIn(point)
            }
            for (k <- 0 until myK)
                redCluster(k).normalize
            
            var b:Boolean = true
            for (k <- 0 until myK; if b) {
                if (redCluster(k).dist(blackCluster(k)) > EPS) {
                    b=false
                }
            }
            if (b) 
                return redCluster
            for (k <- 0 until myK)
                blackCluster(k).makeZero
        }
        return redCluster;  
    }
}
  
    def main(args : Array[String]) = {
        val rnd = new Random(0)
        val points = Array.fromFunction(
                _ => Array.fromFunction(_ => rnd.nextFloat)(DIM))(POINTS)
        val result = new KMeans(DIM).computeMeans(K, points)
        for (k <- 0 until K)
            result(k).print
    }

// vim: shiftwidth=4:tabstop=4:expandtab
}
