package funicular.array

import funicular.Intrinsics._
import funicular.runtime.Runtime

class RichArray[A: ClassManifest](a: Array[A]) extends Proxy {
    private def P = Runtime.concurrency

    def self = a

    def copy = a.map((t:A) => t)

    def inParallel = new ParArray[A](a)
    def async = new ParArray[A](a)
    def asynchronously = new ParArray[A](a)

    def printPar =
        for (ai <- a.inParallel) {
            println(ai)
        }

    def parInit(f: Int => A): Array[A] = {
        finish {
            foreach (0 until a.length) {
                j => {
                  a(j) = f(j)
                }
            }
        }
        a
    }

    def lift[B: ClassManifest](f: A => B): Array[B] = 
        Array.ofDim[B](a.length).parInit(i => f(a(i)))

    def reduce(z: A)(f: (A,A) => A): A = {
        val r = Array.ofDim[A](P)

        finish {
            foreach (0 until P) {
                i => {
                    val scale = (a.length + P - 1) / P
                    val min = i*scale
                    val max = Math.min((i+1)*scale, a.length)
                    var x = z
                    for (j <- min until max)
                        x = f(x, a(j))
                    r(i) = x
                }
            }
        }

        var x = z
        for (i <- 0 until P)
            x = f(x, r(i))
        x
    }
}
