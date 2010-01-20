package x10test

import x10.Intrinsics._

object Test2 {
  class Buffer {
    var empty: Boolean = true;
    var full: Boolean = false;
    private var n: Int = 0;

    def value_=(m: Int) = {
      when (empty) {
        empty = false
        full = true
        n = m
      }
    }

    def value: Int = {
      when (full) {
        empty = true
        full = false
        n
      }
    }
  }

  def main(args: Array[String]): Unit = {
    finish {
      val b = new Buffer

      async {
        for (n <- 1 to 10)
          b.value = n
      }

      async {
        for (n <- 1 to 10)
          println(b.value)
      }
    }
  }
}
