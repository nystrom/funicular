/*
 *
 * (C) Copyright IBM Corporation 2006-2008
 *
 *  This file is part of X10 Language.
 *
 */

package funicular.runtime

import funicular.MultipleExceptions
import scala.collection.mutable.Stack
import java.util.concurrent.locks.ReentrantLock

/**
 * @author tardieu
 */
class Finish {
    def withLock(body: => Unit) = 
        try {
            lock.lock
            body
        }
        finally {
            lock.unlock
        }

    def withoutLock(body: => Unit) = 
        try {
            lock.unlock
            body
        }
        finally {
            lock.lock
        }

    def join: Unit = {
        println("*** finishing")
        Console.flush
        withLock {
            if (activities == null)
                return

            while (! activities.isEmpty) {
                val a = activities.pop
                withoutLock {
                    a.join
                    println("joining " + a)
                    Console.flush
                }
            }
        }
        println("*** finished")
        Console.flush
    }

    def run(a: Activity) = {
        withLock {
            if (null == activities)
                activities = new Stack[Activity]
            activities push a
        }
        Runtime.pool.execute(a)
    }

    def runAsync(clocks: Seq[Clock], body: => Unit): Unit = {
        for (clock <- clocks)
            clock.register
        run(new Activity(body, this, clocks.toArray))
    }

    def runAsync(body: => Unit): Unit = {
        run(new Activity(body, this))
    }

    private var exceptions: Stack[Throwable] = null
    private var activities: Stack[Activity] = null

    def throwExceptions = {
        if (exceptions != null)
            if (exceptions.size == 1)
                throw exceptions(0)
            else
                throw new MultipleExceptions(exceptions)
    }

    val lock = new ReentrantLock

    def pushException(t:Throwable) = withLock {
        if (null == exceptions)
            exceptions = new Stack[Throwable]
        exceptions push t
    }
}

// vim:shiftwidth=4:tabstop=4:expandtab
