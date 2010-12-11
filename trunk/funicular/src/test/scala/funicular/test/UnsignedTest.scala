package funicular.test

object UnsignedTest {
  import funicular.unsigned._

  def test(s: String, v: Any, u: String) {
    if (v.toString == u)
      // println("OK: " + s + " --> " + v)
      ()
    else
      println("ERROR: " + s + " --> " + v + ", should be " + u)
  }

  def main(args: Array[String]) = {
    for (u <- 0.toUInt to 10.toUInt) {
      println(u)
    }

    // exit(0)

    test("1e9", 1e9.toUInt, "1000000000")
    test("2e9", 2e9.toUInt, "2000000000")
    test("3e9", 3e9.toUInt, "3000000000")
    test("4e9", 4e9.toUInt, "4000000000")

    test("1e9|1", 1e9.toUInt|(1).toUInt, "1000000001")
    test("2e9|1", 2e9.toUInt|(1).toUInt, "2000000001")
    test("3e9|1", 3e9.toUInt|(1).toUInt, "3000000001")
    test("4e9|1", 4e9.toUInt|(1).toUInt, "4000000001")

    for (s <- List(1, 2, 4, 5, 8, 9, 10, 15, 16, 17, 22, 0x12345678)) {
      val u = s.toUInt
      for (t <- List(1, 2, 4, 5, 8, 9, 10, 15, 16, 17, 22, 0x12345678)) {
        val v = t.toUInt

        test("(" + u + " / " + v + ")", (u / v), ((s.toLong / t.toLong) & 0xffffffffL).toString)
        test("(" + u + " * " + v + ")", (u * v), ((s.toLong * t.toLong) & 0xffffffffL).toString)
        test("(" + u + " + " + v + ")", (u + v), ((s.toLong + t.toLong) & 0xffffffffL).toString)
        test("(" + u + " - " + v + ")", (u - v), ((s.toLong - t.toLong) & 0xffffffffL).toInt.toString)

        test("(" + u + " / " + t + ")", (u / t), ((s.toLong / t.toLong) & 0xffffffffL).toInt.toString)
        test("(" + u + " * " + t + ")", (u * t), ((s.toLong * t.toLong) & 0xffffffffL).toInt.toString)
        test("(" + u + " + " + t + ")", (u + t), ((s.toLong + t.toLong) & 0xffffffffL).toInt.toString)
        test("(" + u + " - " + t + ")", (u - t), ((s.toLong - t.toLong) & 0xffffffffL).toInt.toString)

        test("(" + u + " < " + v + ")", (u < v), (s.toLong < t.toLong).toString)
        test("(" + u + " > " + v + ")", (u > v), (s.toLong > t.toLong).toString)
        test("(" + u + " <= " + v + ")", (u <= v), (s.toLong <= t.toLong).toString)
        test("(" + u + " >= " + v + ")", (u >= v), (s.toLong >= t.toLong).toString)
        test("(" + u + " == " + v + ")", (u == v), (s.toLong == t.toLong).toString)
        test("(" + u + " != " + v + ")", (u != v), (s.toLong != t.toLong).toString)
      }
    }
  }
}
