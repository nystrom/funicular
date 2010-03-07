package funicular.array

import funicular.Intrinsics._
import funicular.runtime.Runtime

class ParSeq[A](a: Seq[A]) extends Proxy with Seq[A] {
    private lazy val P = Runtime.concurrency

    def self = a

    override def filter(p: A => Boolean): Seq[A] = a.filter(p)
    def apply(i: Int): A = a.apply(i)
    def length = a.length
    def iterator = a.iterator

    override def foreach[B](f: A => B): Unit = {
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
