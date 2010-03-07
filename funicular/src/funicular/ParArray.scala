package funicular

import funicular.Intrinsics._
import funicular.runtime.Runtime

class ParArray[A: ClassManifest](a: Array[A]) extends Proxy {
    private lazy val P = Runtime.concurrency

    def self = a

    def flatMap[B](f: A => Iterable[B]): Seq[B] = {
        println("flatmap")
        val result = Array.ofDim[Iterable[Future[Iterable[B]]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            val s = (min until max).map(a).map(aj => future { f(aj) })
            result(i) = s
        }
        new ParSeq[B](result.flatMap(identity).map(_.force).flatMap(identity))
    }

    def filter(p: A => Boolean): Seq[A] = {
        println("filter")
        val result = Array.ofDim[Future[Seq[A]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            result(i) = future[Seq[A]] {
                for (j <- min until max; if p(a(j)))
                        yield a(j)
            }
        }
        new ParSeq[A](scala.collection.mutable.WrappedArray.make(result.map(_.force).flatMap(identity)))
    }

    def map[B](f: A => B): Seq[B] = {
        println("map")
        val result = Array.ofDim[Seq[Future[B]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            result(i) = for (j <- min until max)
                            yield future[B] { f(a(j)) }
        }
        new ParSeq[B](result.flatMap(identity).map(_.force))
    }

    def foreach[B](f: A => B): Unit = {
        println("foreach")
        finish {
            Intrinsics.foreach (0 until P) {
                i => {
                    println(i + " of " + P)
                    val scale = (a.length + P - 1) / P
                    val min = i*scale
                    val max = Math.min((i+1)*scale, a.length)
                    for (j <- min until max)
                        f(a(j))
                }
            }
        }
    }
}
