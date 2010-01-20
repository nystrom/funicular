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
class Future[T](eval: => T) extends AnyRef with funicular.Future[T] {
    /**
     * Latch for signaling and wait
     */
    private val latch = new Latch

    /**
     * Set if the activity terminated with an exception.
     * Can only be of type Error or RuntimeException
     */
    private var exception = List[Throwable]()

    private var result:List[T] = List[T]()

    def forced: Boolean = latch.apply
    
    def force = {
        latch.await
        if (exception.length > 0) {
            val e = exception(0)
            if (e.isInstanceOf[Error])
                throw e.asInstanceOf[Error]
            if (e.isInstanceOf[RuntimeException])
                throw e.asInstanceOf[RuntimeException]
            throw new RuntimeException(e)
        }
        result.head
    }

    def start: Unit = {
        Runtime.runAsync(this.run)
    }

    def run: Unit = {
        try {
            finish {
                result = result ::: List(eval)
                latch.release
            }
        }
        catch {
            case t: Throwable => {
                exception = exception ::: List(t)
                latch.release
            }
        }
    }
}

// vim:shiftwidth=4:tabstop=4:expandtab
