package funicular.test

import funicular._

object Histogram {
    /**
    * Compute the histogram of the array a in the rail b.
    */
    def run(a:Array[Int], b: Array[Int]) {
        foreach (0 until a.length) {
            i => {
              val bin = a(i) % b.length
              atomic { b(bin) += 1 }
            }
        }
    }

    def main(args:Array[String]) {
	if (args.length != 2) {
	    println("Usage: Histogram SizeOfArray Buckets")
	    sys.exit(-1)
        }

	val N = args(0).toInt
	val S = args(1).toInt
	val a = Array.range(0, N)
	val b = new Array[Int](S)
	run(a, b)
	val v = b(0)
        var ok: Boolean = true
	for (x <- b) {
            println(x)
            ok &= (x==v)
          }
	if (ok) {
	    println("Test ok.")
	} else {
	    println("Test failed.")
	}
    }
}
