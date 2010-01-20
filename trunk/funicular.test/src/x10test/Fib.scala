package x10test
/*
 *
 * (C) Copyright IBM Corporation 2009
 *
 *  This is an example program used in the X10 1.7 Tutorial.
 *
 */
import x10.Intrinsics._

/**
 * This is a small program to illustrate the use of 
 * <code>async</code> and <code>finish</code> in a 
 * prototypical recursive divide-and-conquer algorithm.  
 * It is obviously not intended to show a efficient way to
 * compute Fibonacci numbers in X10.<p>
 *
 * The heart of the example is the <code>run</code> method,
 * which directly embodies the recursive definition of 
 * <pre>
 *   Fib(n) = Fib(n-1)+Fib(n-
 * </pre>
 * by using an <code>async</code> to compute <code>Fib(n-1)</code> while
 * the current activity computes <code>Fib(n-2)</code>.  A <code>finish</code>
 * is used to ensure that both computations are complete before 
 * their results are added together to compute <code>Fib(n)</code>
 */
object Fib {
  /**
   * Used as an in-out parameter to the computation.
   * When the Fib object is created, r indicates the number to compute.
   * After the computation has completed, r holds the result (Fib(r)).
   */
  class Fib(var r: Int) {
    def run: Unit = {
      if (r < 2)
        return
      val f1 = new Fib(r-1)
      val f2 = new Fib(r-2)
      finish {
        async { f1.run }
        f2.run
      }
      r = f1.r + f2.r
    }
  }

  def main(args:Array[String]) = {
    val n = if (args.length > 0) args(0).toInt else 10
    println("Computing Fib("+n+")")
    val f = new Fib(n)
    f.run
    println("Fib("+n+") = "+f.r)
  }
}

