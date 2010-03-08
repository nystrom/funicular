/*
 *
 * (C) Copyright IBM Corporation 2006-2008
 *
 *  This file is part of X10 Language.
 *
 */

package funicular.runtime

import funicular.MultipleExceptions

/**
 * @author tardieu
 */
class Finish {
    private val lock = new Lock
    private var exceptions: List[Throwable] = Nil
    private var activities: List[Activity] = Nil

    def aaa = activities

    def join: Unit = {
        lock.withLock {
            while (true) {
                activities match {
                    case Nil => return
                    case a::as => {
                        activities = as
                        lock.withoutLock {
                            a.join
                        }
                    }
                }
            }
        }
    }

    def run(a: Activity) = {
        lock.withLock {
            activities = a::activities
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

    def throwExceptions = {
        lock.withLock {
            exceptions match {
                case Nil => null
                case e::Nil => throw e
                case es => throw new MultipleExceptions(es)
            }
        }
    }

    def pushException(t:Throwable) = lock.withLock {
        exceptions = t::exceptions
    }
}

// vim:shiftwidth=4:tabstop=4:expandtab
