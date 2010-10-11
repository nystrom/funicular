package funicular.test

import funicular._

object Sieve {
  class Buffer {
    var empty: Boolean = true;
    var full: Boolean = false;
    var n: Int = 0;

    def produce(m: Int) = {
      println("produce " + m)
      when (empty) {
        n = m
        full = true
      }
    }

    def consume: Int = {
      println("consume")
      when (full) {
        empty = true
        n
      }
    }
  }

  def sieve(b1: Buffer): Unit = {
    val n = b1.consume
    println(n)

    val b2 = new Buffer

    finish {
      async {
        val i = b1.consume
        if (i % n != 0)
          b2.produce(i)
      }

      async {
        sieve(b2)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    finish {
      val b = new Buffer

      async {
        sieve(b)
      }

      async {
        for (n <- 1 to 1000)
          b.produce(n)
      }
    }
  }
}
