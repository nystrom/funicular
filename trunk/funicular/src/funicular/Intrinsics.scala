package funicular

import funicular.runtime.Runtime
import funicular.array.RichArray

object Intrinsics {
  implicit def wrapArray[T: ClassManifest](a: Array[T]) = new RichArray[T](a)

  /**
   * Sleep for the specified number of milliseconds.
   * [IP] NOTE: Unlike Java, funicular sleep() simply exits when interrupted.
   * @param millis the number of milliseconds to sleep
   * @return true if completed normally, false if interrupted
   */
  def sleep(millis: Long) = Runtime.sleep(millis)

  ////////////////////////////////////////////////////////////////
  // Clock operations
  ////////////////////////////////////////////////////////////////

  // Usage:
  // next
  def next = Runtime.next

  ////////////////////////////////////////////////////////////////
  // Asyncs
  ////////////////////////////////////////////////////////////////
  
  // usage:
  // async { body }
  def async(body: => Unit) = {
    Runtime.runAsync(body)
  }
  
  // usage:
  // async (clocks) { body }
  def async(clocks: Clock*)(body: => Unit) = {
    Runtime.runAsync(clocks.map((c: Clock) => c.asInstanceOf[funicular.runtime.Clock]), body)
  }

  // usage:
  // foreach (A) { Ai => body }
  def foreach[T](a: Array[T])(body: T => Unit) = {
      val P = Runtime.concurrency
      for (p <- 0 until P) {
          async {
              val min = p * a.length / P
              val max = Math.min(a.length, (p+1) * a.length / P)
              for (i <- min until max)
                  body(a(i))
          }
      }
      /*
      for (i <- 0 until a.length)
          async { body(a(i)) }
          */
  }

  // usage:
  // foreach (1 to 10) { i => body }
  def foreach(rng: Range)(body: Int => Unit) = {
      val P = Runtime.concurrency
      val N = rng.end - rng.start + 1 
      for (p <- 0 until P) {
          async {
              val min = rng.start + p * N / P
              val max = Math.min(rng.end, rng.start + (p+1) * N / P)
              for (i <- min until max)
                  body(i)
          }
      }
      /*
      for (i <- rng)
          async { body(i) }
      */
  }

  // usage:
  // future(e)
  def future[T](eval: => T): Future[T] = {
      Runtime.evalFuture[T](eval)
  }

  // usage:
  // delayedFuture(e)
  def delayedFuture[T](eval: => T): Future[T] = {
      Runtime.evalDelayedFuture[T](eval)
  }

  ////////////////////////////////////////////////////////////////
  // Synchronization
  ////////////////////////////////////////////////////////////////

  // usage:
  // finish { body }
  def finish(body: => Unit): Unit = {
      Runtime.runFinish(body)
  }

  // usage:
  // when(cond) { body }
  def when[T](cond: => Boolean)(body: => T) = atomic {
      while (! cond)
          Runtime.await
      body
  }
  
  // usage:
  // await(cond)
  def await(cond: => Boolean): Unit = atomic {
      while (! cond)
          Runtime.await
  }

  // usage:
  // atomic { body }
  def atomic[T](body: => T) = {
      try {
          Runtime.lock
          body
      }
      finally {
          Runtime.release
      }
  }
}
