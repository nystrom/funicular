/*
 *
 * (C) Copyright IBM Corporation 2006-2008
 *
 *  This file is part of X10 Language.
 *
 */

package funicular.runtime

import funicular.MultipleExceptions
import java.util.concurrent.locks.ReentrantLock

/**
 * @author tardieu
 */
class Finish {
    def withLock[T](body: => T) = 
        try {
            lock.lock
            body
        }
        finally {
            lock.unlock
        }

    def withoutLock[T](body: => T) = 
        try {
            lock.unlock
            body
        }
        finally {
            lock.lock
        }

    def join: Unit = {
        withLock {
            while (true) {
                // println("joining " + activities)
                activities match {
                    case Nil => return
                    case a::as => {
                        activities = as
                        // println("popping " + a)
                        withoutLock {
                            a.join
                        }
                    }
                }
            }
        }
    }

    def run(a: Activity) = {
        withLock {
            activities = a::activities
            // println("pushing " + a)
            // println("forked " + activities)
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

    private var exceptions: List[Throwable] = Nil
    private var activities: List[Activity] = Nil

    def throwExceptions = {
        withLock {
            exceptions match {
                case Nil => null
                case e::Nil => throw e
                case es => throw new MultipleExceptions(es)
            }
        }
    }

    val lock = new ReentrantLock

    def pushException(t:Throwable) = withLock {
        exceptions = t::exceptions
    }
}

// vim:shiftwidth=4:tabstop=4:expandtab
