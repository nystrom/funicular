package funicular.test

import funicular._

object Test3 {
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

      foreach (1 to 100) {
        n => { b.value = n }
      }

      foreach (1 to 100) {
        n => { println(n + ": " + b.value) }
      }
    }
  }
}
