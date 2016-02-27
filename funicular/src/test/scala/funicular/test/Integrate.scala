package funicular.test

import funicular._

/*
 *
 * (C) Copyright IBM Corporation 2009
 *
 *  This is an example program used in the X10 2.0 Tutorial.
 *
 */

/**
 * This is a slightly more realistic example of the
 * basic computational pattern of using async/finish
 * to express recursive divide-and-conquer algorithms.
 * The program does integration via Guassian Quadrature.
 * <p>
 * It also can serve as an example of using a closure.
 */
object Integrate { 
  val epsilon = 1.0e-8

  class Integrate(fun:Double=>Double) {
    def computeArea(left:Double, right:Double) =
      recEval(left, fun(left), right, fun(right), 0)

    private def recEval(l:Double, fl:Double, r:Double, fr:Double, a:Double): Double = {
      val h = (r - l) / 2
      val hh = h / 2
      val c = l + h
      val fc = fun(c)
      val al = (fl + fc) * hh   
      val ar = (fr + fc) * hh
      val alr = al + ar

      if (math.abs(alr - a) < epsilon)
          return alr

      var res1:Double = 0.0
      var res2:Double = 0.0

      finish {
        async { res1 = recEval(c, fc, r, fr, ar) }
                res2 = recEval(l, fl, c, fc, al)  
      }
      println((l, fl, r, fr, a, res1, res2))
      res1 + res2
    }
  }
 
  def main(args:Array[String]) = finish {
    val obj = new Integrate((x:Double)=>(x*x + 1.0) * x)
    val xMax = if (args.length > 0) args(0).toInt else 10
    val area = obj.computeArea(0, xMax)
    println("The area of (x*x +1) * x from 0 to "+xMax+" is "+area)
  }
}
