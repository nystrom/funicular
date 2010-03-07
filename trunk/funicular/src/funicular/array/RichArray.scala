package funicular.array

import funicular.Intrinsics._
import funicular.runtime.Runtime

object RichArray {
    def fromPar[T: ClassManifest](f: Int => T)(n: Int): Array[T] = Array.ofDim[T](n).parInit(f)
}

class RichArray[T: ClassManifest](a: Array[T]) extends Proxy {
    private def P = Runtime.concurrency

    def self = a

    def copy = a.map((t:T) => t)

    def inParallel = new ParArray[T](a)

    def print =
        for (ai <- a.inParallel) {
            println(ai)
        }

    def parInit(f: Int => T): Array[T] = {
        finish {
            foreach (0 until a.length) {
                j => {
                    a(j) = f(j)
                }
            }
        }
        a
    }

    def lift(f: T => T): Array[T] = 
        Array.ofDim[T](a.length).parInit((i: Int) => f(a(i)))

    /*
    def scan(f: (T,T) => T, z: T): Array[T] = {
        var r: T = z

        Array.ofDim[T](a.length).parInit(
          (i: Int) => {
              r = f(r, a(i))
              r
          })
    }
    */

    def mapReduce1[S: ClassManifest](map: T => S, reduce: (S,S) => S): S = {
        val r = Array.ofDim[S](P)

        finish {
            foreach (0 until P) {
                i => {
                    val min = i*(a.length/P)
                    val max = Math.min((i+1)*(a.length/P), a.length)
                    var x = map(a(min))
                    for (j <- min+1 until max) {
                        x = reduce(x, map(a(j)))
                    }
                    r(i) = x
                }
            }
        }

        var x = r(0)
        for (i <- 1 until P)
            x = reduce(x, r(i))
        x
    }

    def reduce(f: (T,T) => T, z: T): T = {
        val r = Array.ofDim[T](P)

        finish {
            foreach (0 until P) {
                i => {
                    val min = i*(a.length/P)
                    val max = Math.min((i+1)*(a.length/P), a.length)
                    var x = z
                    for (j <- min until max)
                        x = f(x, a(j))
                    r(i) = x
                    println(i + " " + a.length + " :: " + min + ".." + (max-1))
                }
            }
        }

        Console.out.flush

        println("end reduce")

        var x = z
        for (i <- 0 until P)
            x = f(x, r(i))
        x
    }
}



