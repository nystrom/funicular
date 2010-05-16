package funicular.test

import java.util.concurrent.atomic.AtomicBoolean
import jsr166y.ForkJoinTask
import jsr166y.RecursiveAction
import jsr166y.ForkJoinPool

object NQueensJava {

    val pool = new ForkJoinPool(8)



    /**
     * Return an array of P regions, which together block divide the 1-D region R.
     */
    def block(R: Range, P: Int): Array[Range] = {
        assert(P >= 0)
        val low = R.start
        val high = R.end-1
        val count = high - low + 1
        val baseSize = count/P
        val extra = count - baseSize*P
        Array.tabulate[Range](P)((i:Int) => {
                val start = low+i*baseSize + (if (i < extra) i else extra)
                start to (start + baseSize + (if (i < extra) 0 else -1))
        })
    }

    val expectedSolutions =
        List(0, 1, 0, 0, 2, 10, 4, 40, 92, 352, 724, 2680, 14200, 73712, 365596, 2279184, 14772512)

  class Q(val N: Int, P: Int) {
    @volatile var nSolutions:Int = 0

    def start {
        new Board().search
    }

    class Board private (q: Array[Int]) {
        def this() = this(new Array[Int](0))
        def this(old: Array[Int], newItem: Int) =
            this(Array.tabulate(old.length+1)((i:Int) => (if (i < old.length) old(i) else newItem)))

        def safe(j: Int): Boolean = {
            val n = q.length
            for (k <- 0 until n) {
                if (j == q(k) || Math.abs(n-k) == Math.abs(j-q(k)))
                    return false
            }
            true
        }

        /** Search for all solutions in parallel, on finding
         * a solution update nSolutions.
         */
        def search(R: Range): Unit = {
            for (k <- R) {
                if (safe(k))
                    new Board(q, k).search
            }
        }

        val lock = new AtomicBoolean(false)

        def atomic(body: => Unit): Unit = {
            while (lock.getAndSet(true))
                ()
            try {
                body
            }
            finally {
                lock.set(false)
            }
        }

        def foreach(r: Range)(body: Int => Unit): Unit = {
            var tasks = List[RecursiveAction]()
            for (i <- r) {
                val t = new RecursiveAction() {
                    def compute: Unit = {
                        body(i)
                    }}
                tasks = t :: tasks
                pool.execute(t)
            }
            for (t <- tasks) {
                t.join
            }
        }

        def search: Unit = {
            if (q.length == N) {
                atomic {
                    nSolutions += 1
                    // println("found " + nSolutions)
                    // Console.flush
                }
            }
            else if (q.length == 0) {
                val R = block(0 until N, P)
                foreach (0 until P) {
                    q => search(R(q))
                }
            }
            else
                search(0 until N)
        }
    }
  }

    def main(args: Array[String]) = {
        val n = if (args.length > 0) args(0).toInt else 8
        println("N=" + n)
        //warmup
        //finish new NQueensJava(12, 1).start
        val ps = List(1,2,4,8,16)
        for (p <- ps) {
            println("starting " + p + " threads")
            val nq = new Q(n,p)
            var start:Long = -System.nanoTime
            val t = new RecursiveAction() {
                    def compute: Unit = {
                     nq.start
                    }}
            pool.invoke(t)
            val result = nq.nSolutions==expectedSolutions(nq.N)
            start += System.nanoTime
            start /= 1000000
            println("NQueensJava " + nq.N + "(P=" + p +
                    ") has " + nq.nSolutions + " solutions" +
                    (if (result) " (ok)." else " (wrong).") +
                    "time=" + start + "ms")
        }
    }
}
