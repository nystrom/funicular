package funicular.test

import funicular._
import funicular.Future

object Fib2 {
  def fib(n: Int): Future[Int] = future[Int] {
     if (n <= 2) 1
     else fib(n-1).force + fib(n-2).force }
  def main(args:Array[String]) = finish {
    val n = if (args.length > 0) args(0).toInt
            else 10
    println("fib(" + n + ") = " + fib(n).force)
  }
}

