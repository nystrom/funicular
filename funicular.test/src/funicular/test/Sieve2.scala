package funicular.test

import funicular._
import funicular.Future

object Sieve2 {
    class Front[T](val h: T, val t: Future[Front[T]])

    type Trickle[T] = Future[Front[T]]
    class TrickleClosedError extends RuntimeException

    def filter(n:Int, s:Trickle[Int]): Trickle[Int] = {
        val sf = s()
        if (sf.h % n == 0)
            filter(n, sf.t)
        else
            future(new Front[Int](sf.h, filter(n, sf.t)))
    }

    def gen(n:Int, m:Int):Trickle[Int] = {
        if (n > m) throw new TrickleClosedError
        future(new Front[Int](n, gen(n+1,m)))
    }
    def sieve(s:Trickle[Int]):Trickle[Int] = {
        val sf = s()
        future(new Front[Int](sf.h, sieve(filter(sf.h, sf.t))))
    }

    def printTrickle[T](v: Trickle[T]): Unit = {
      val vf = v()
      println(vf.h)
      try {
        printTrickle(vf.t)
      }
      catch {
        case e: TrickleClosedError => ()
      }
    }

    def main(args: Array[String]): Unit = finish {
        printTrickle(sieve(gen(2, 100)))
    }
}
