package funicular.array

import funicular.Intrinsics.{foreach => par, _}
import funicular.Future
import funicular.runtime.Runtime

class ParArray[A: ClassManifest](a: Array[A]) extends Proxy {
    private lazy val P = Runtime.concurrency

    def self = a

    def flatMap[B](f: A => Iterable[B]): Seq[B] = {
        val spawn = (0 until a.length).map(j => future { f(a(j)) })
        new ParSeq[B](spawn.map(_.force).flatMap(identity))
/*

        val result = Array.ofDim[Iterable[Future[Iterable[B]]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            val s = (min until max).map(a).map(aj => future { f(aj) })
            result(i) = s
        }
        new ParSeq[B](result.flatMap(identity).map(_.force).flatMap(identity))
        */
    }

    def filter(p: A => Boolean): Seq[A] = {
        val spawn = Array.ofDim[Future[Seq[A]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            spawn(i) = future[Seq[A]] {
                for (j <- min until max; if p(a(j)))
                        yield a(j)
            }
        }
        val seq = (0 until spawn.length).map(spawn)
        new ParSeq[A](seq.map(_.force).flatMap(identity))
    }

    def map[B](f: A => B): Seq[B] = {
        /*
        val spawn = for (j <- 0 until a.length) yield future[B] { f(a(j)) }
        new ParSeq[B](spawn.flatMap(identity).map(_.force))
        */
        /*
        val spawn = Array.ofDim[Seq[Future[B]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            spawn(i) = for (j <- min until max)
                            yield future[B] { f(a(j)) }
        }
        new ParSeq[B](spawn.flatMap(identity).map(_.force))
        */
        /*
        val spawn = Array.ofDim[Future[Seq[Future[B]]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            spawn(i) = future[Seq[Future[B]]] {
                for (j <- min until max) yield future[B] { f(a(j)) }
            }
        }
        new ParSeq[B](spawn.map(_.force).flatMap(identity).map(_.force))
        */
        val spawn = Array.ofDim[Future[Seq[B]]](P)
        for (i <- 0 until P) {
            val scale = (a.length + P - 1) / P
            val min = i*scale
            val max = Math.min((i+1)*scale, a.length)
            spawn(i) = future[Seq[B]] {
                for (j <- min until max) yield f(a(j))
            }
        }
        val seq = (0 until spawn.length).map(spawn)
        new ParSeq[B](seq.map(_.force).flatMap(identity))
    }

    def foreach[B](f: A => B): Unit = {
        par (0 until P) {
            i => {
                val scale = (a.length + P - 1) / P
                val min = i*scale
                val max = Math.min((i+1)*scale, a.length)
                for (j <- min until max)
                    f(a(j))
            }
        }
    }
}
