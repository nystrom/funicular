/*
 *
 * (C) Copyright IBM Corporation 2006-2008.
 *
 *  This file is part of X10 Language.
 *
 */

package funicular.runtime

import funicular.Intrinsics._

/**
 * The representation of an X10 future expression.
 * @author tardieu
 */
class Future[A](name: String, eval: => A) extends AnyRef with funicular.Future[A] {
    /**
     * Latch for signaling and wait
     */
    private val latch = new Latch
    private var isStarted = false
    private val startLock = new Lock

    /**
     * Set if the activity terminated with an exception.
     * Can only be of type Error or RuntimeException
     */
    private var exception: Option[Throwable] = None
    private var result: Option[A] = None

    def forced: Boolean = latch.apply
    def started: Boolean = startLock.withLock { isStarted }
    
    def force = {
        start
        latch.await
        exception match {
            case Some(e) => {
                if (e.isInstanceOf[Error])
                    throw e.asInstanceOf[Error]
                if (e.isInstanceOf[RuntimeException])
                    throw e.asInstanceOf[RuntimeException]
                throw new RuntimeException(e)
            }
            case None => ()
        }
        result match {
            case Some(v) => v
            case None => throw new RuntimeException("future forced, but no value")
        }
    }

    def start: Unit = {
        startLock.withLock {
            if (isStarted) {
                return
            }
            isStarted = true
        }
        Runtime.runAsync(this.run)
    }

    private def run: Unit = {
        try {
            result = Some(eval)
        }
        catch {
            case t: Throwable => {
                exception = Some(t)
            }
        }
        finally {
            latch.release
        }
    }

    override def toString = name
}

// vim:shiftwidth=4:tabstop=4:expandtab
