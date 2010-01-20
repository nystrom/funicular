package funicular.runtime

import funicular.Intrinsics._
import funicular.ClockUseException

object Clock {
    val FIRST_PHASE = 1
}

class Clock(name:String) extends jsr166y.Phaser with funicular.Clock {
    def this() = this("clock")

    super.register
    reg = true

    def withExceptions[T](body: => T): T = {
        try {
            body
        }
        catch {
            case e: IllegalStateException => throw new ClockUseException
        }
    }

    override def register: Int = {
        if (dropped)
            throw new ClockUseException
        withExceptions {
            val r = super.register
            reg = true
            r
        }
    }

    private var reg = true

    def registered = reg

    def dropped = ! reg

    def phase: Int = super.getPhase

    def resume: Unit = {
        if (dropped)
            throw new ClockUseException
        withExceptions { super.arrive }
    }

    def next : Unit = {
        if (dropped)
            throw new ClockUseException
        withExceptions { super.arriveAndAwaitAdvance }
    }

    def drop: Unit =  {
        if (dropped)
            throw new ClockUseException
        withExceptions { super.arriveAndDeregister }
    }
}
