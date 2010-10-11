package funicular.test

import funicular._
import funicular.Clock

object BFS {

    class V(val value: Int) {
        var neighbors: Array[V] = null
        var parent: V = null

        override def toString = "" + value + "->" + neighbors.foldLeft(if (parent == null) "[null]" else "[" + parent.value + "]")((s:String,t:V) => s + "," + t.value)

        def tryColor(n: V) = { 
            atomic { 
                if (parent == null)
                    parent = n 
            } 
            parent == n
        } 

        def computeDFS: Unit = {
            for (e <- neighbors) {
                if (e.tryColor(this))
                    async { 
                        e.computeDFS
                    }
            }
        }

        def dfs = {
            parent = this
            finish {
                computeDFS
            } 
        }

        def computeBFS(c: Clock): Unit = {
            for (e <- neighbors) {
                if (e.tryColor(this))
                    async (c) { 
                        next
                        e.computeBFS(c)
                    }
            }
        }

        def bfs = {
            parent = this
            finish {
                async {
                    val c = Clock()
                    computeBFS(c)
                } 
            } 
        }
    }

    val N = 10

    object M {
      def apply(i: Int, j: Int): Int =
        if (i >= N) M(i-N, j)
        else if (i < 0) M(i+N, j)
        else if (j >= N) M(i, j-N)
        else if (j < 0) M(i, j+N)
        else (i * N) + j

      def unapply(i: Int) = Some(i / N, i % N)
    }

  def main(args : Array[String]) : Unit = finish {

    val A = new Array[V](N*N).parInit(i => new V(i))

    finish {
        for (a <- A async) {
            val M(i,j) = a.value
            val n = M(i-1,j)
            val s = M(i+1,j)
            val e = M(i,j+1)
            val w = M(i,j-1)
            a.neighbors = Array(A(n), A(s), A(e), A(w))
        }
    }

    A(0).bfs

    finish {
      for (ai <- A async) {
          println(ai)
      }
    }
    println("----------------------------------")
  }
}
