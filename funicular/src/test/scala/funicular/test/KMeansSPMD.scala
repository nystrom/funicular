package funicular.test

import funicular._
import funicular.Clock
import java.io._
import scala.util.Random

object KMeansSPMD {

    def printClusters (clusters:Array[Float], dims:Int) = {
        for (d <- 0 until dims) {
            for (k <- 0 until clusters.length/dims) {
                if (k>0)
                    print(" ")
                print(clusters(k*dims+d).toString)
            }
            println()
        }
    }

//    class Cloner[T: ClassManifest](a: Array[T]) {
//        def copy = a.map((t:T) => t)
//    }
//
//    implicit def array2cloner[T: ClassManifest](a: Array[T]) = new Cloner(a)

    def main (args : Array[String]): Unit = finish {
        var fname_ :String = "points.dat"
        var DIM_ :Int=3
        var CLUSTERS_ :Int=8
        var POINTS_ :Int=10000
        var ITERATIONS_ :Int=500
        for (n <- 0 until args.length) {
            n match { 
              case 4 => ITERATIONS_ = args(4).toInt
              case 3 => DIM_        = args(3).toInt
              case 2 => CLUSTERS_   = args(2).toInt
              case 1 => POINTS_     = args(1).toInt
              case 0 => fname_      = args(0)
            }
      }
        val fname = fname_
        val DIM = DIM_
        val CLUSTERS = CLUSTERS_
        val POINTS = POINTS_
        val ITERATIONS = ITERATIONS_

        println("points: "+POINTS+"  dim: "+DIM)

        try {

            val fr = new DataInputStream(new FileInputStream(fname))

            val init_points = (i:Int) =>
                java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes(fr.readInt))
            val points_cache = Array.tabulate(POINTS*DIM)(init_points)
            val points = Array.tabulate(POINTS)((i:Int) => Array.tabulate(DIM)((j:Int) => points_cache(i*DIM+j)))

            val central_clusters = Array.tabulate(CLUSTERS*DIM)((i:Int) => points_cache(i))
            // used to measure convergence at each iteration:
            val central_clusters_old = Array.tabulate(CLUSTERS*DIM)((i:Int) => central_clusters(i))
            val central_cluster_counts = Array.fill(CLUSTERS)(0)

            var finished = false
            // SPMD style for algorithm
            val clk = Clock()

            val start_time = System.currentTimeMillis

            val P = 8

            finish {

                for (d <- 0 until P) async (clk) {
                    val n = points.length
                    val min = d * (n/P)
                    val max = math.min(n, (d+1) * (n/P))

                    val clusters = Array.fill(CLUSTERS*DIM)(0.0F)
                    val new_clusters = Array.fill(CLUSTERS*DIM)(0.0F)
                    val cluster_counts = Array.fill(CLUSTERS*DIM)(0)

                    for (iter <- 0 until ITERATIONS; if (! finished)) {
                        println("Iteration: "+iter)

                        // fetch the latest clusters
                        val central_clusters_copy = central_clusters.copy
                        for (i <- 0 until CLUSTERS)
                            cluster_counts(i) = 0
                        for (j <- 0 until CLUSTERS*DIM) {
                            clusters(j) = central_clusters_copy(j)
                            new_clusters(j) = 0
                        }

                        // compute new clusters and counters
                        
                        for (p <- min until max) {
                            var closest:Int = -1
                            var closest_dist:Float = Float.MaxValue
                            for (k <- 0 until CLUSTERS) {
                                var dist : Float = 0.0F
                                for (d <- 0 until DIM) {
                                    val tmp = points(p)(d) - clusters(k*DIM+d)
                                    dist += tmp * tmp
                                }
                                if (dist < closest_dist) {
                                    closest_dist = dist
                                    closest = k
                                }
                            }
                            for (d <- 0 until DIM) {
                                new_clusters(closest*DIM+d) += points(p)(d)
                            }
                            cluster_counts(closest) += 1
                        }


                        if (d == 0) {
                            for (j <- 0 until CLUSTERS)
                                central_cluster_counts(j) = 0
                            for (j <- 0 until DIM*CLUSTERS) {
                                central_clusters_old(j) = central_clusters(j)
                                central_clusters(j) = 0
                            }
                        }

                        next

                        // have to create valrails for serialisation
                        val new_clusters_copy = new_clusters.copy
                        val cluster_counts_copy = cluster_counts.copy

                        atomic {
                            for (j <- 0 until DIM*CLUSTERS) {
                                central_clusters(j) += new_clusters_copy(j)
                            }
                            for (j <- 0 until CLUSTERS)
                                central_cluster_counts(j) += cluster_counts_copy(j)
                        }


                        next

                        if (d == 0) {
                            for (k <- 0 until CLUSTERS) {
                                for (d <- 0 until DIM) {
                                    central_clusters(k*DIM+d) /= central_cluster_counts(k)
                                }
                            }

                            // TEST FOR CONVERGENCE
                            var b:Boolean = true
                            finished = true
                            for (j <- 0 until CLUSTERS*DIM; if (b)) {
                                if (math.abs(central_clusters_old(j)-central_clusters(j))>0.0001) {
                                    finished = false
                                    b = false
                                }
                            }
                            
                        }



                    }


                }

                clk.drop

            }

            printClusters(central_clusters,DIM)

            val stop_time = System.currentTimeMillis
            println("Time taken: "+(stop_time-start_time)/1E3)

        } catch {
          case e : IOException => {
            println("We had a little problem:")
            e.printStackTrace
            sys.exit(1)
        }
        }
    }
}

// vim: shiftwidth=4:tabstop=4:expandtab
