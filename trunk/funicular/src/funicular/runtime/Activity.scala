/*
*
* (C) Copyright IBM Corporation 2006-2008.
*
*  This file is part of X10 Language.
*
*/

package funicular.runtime

/**
* @author tardieu
*/
class Activity (body: => Unit, val finish: Finish, val clocks: Array[Clock]) extends jsr166y.RecursiveAction {
    /**
     * Create clocked activity.
     */
    def this(body: => Unit, finish: Finish) {
        this(body, finish, null)
        // println("this " + this)
    }

    def next: Unit = {
        if (clocks != null)
            for (clock <- clocks)
                clock.next
    }

    /**
     * the finish state governing the execution of this activity
     */
    var innermostFinish: Finish = finish

    def runFinish(body: => Unit): Unit = {
        val old = innermostFinish
        val f = new Finish
        try {
            innermostFinish = f
            body
            f.join
            f.throwExceptions
        }
        finally {
            innermostFinish = old
        }
    }

    override def compute: Unit = {
        val old = Runtime.myActivity.get
        try {
            Runtime.myActivity.set(this)
            body
        }
        catch { 
            case t:Throwable => innermostFinish.pushException(t)
        }
        finally {
            Runtime.myActivity.set(old)
        }

        if (null != clocks)
            for (clock <- clocks)
                clock.drop
    }
}

// vim:shiftwidth=4:tabstop=4:expandtab
