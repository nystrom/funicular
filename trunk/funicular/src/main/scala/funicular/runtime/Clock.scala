package funicular.runtime

import funicular._
import funicular.ClockUseException

object Clock {
  val FIRST_PHASE = 1
}

class Clock(name: String) extends funicular.Clock { self =>

  def this() = this("cluck")

  private val ph = new jsr166y.Phaser(0) {
    override def onAdvance(phase: Int, registered: Int) = {
      println(self + " + advancing to " + phase + ", " + registered + " registered")
      false
    }
  }

  private var reg = Set.empty[Activity]

  //    register

  def withExceptions[T](body: => T): T = {
    try {
      body
    } catch {
      case e: IllegalStateException => {
          e.printStackTrace
          throw new ClockUseException
        }
    }
  }

  def register: Unit = {
    new Exception("registering " + this + " with " + Runtime.activity).printStackTrace
    withExceptions {
      ph.register
      reg += Runtime.activity
    }
  }

  // BUG: should return true if THIS activity is registered on the clock
  def registered = reg contains Runtime.activity

  def dropped = !registered

  def phase: Int = ph.getPhase

  def resume: Unit = {
    println("resume " + this + " arrived=" + ph.getArrivedParties + "/" + ph.getRegisteredParties)
    new Exception("resume " + this + " " + Runtime.activity).printStackTrace
    if (dropped)
      throw new ClockUseException
    withExceptions { ph.arrive }
    println("done with resume " + this + " arrived=" + ph.getArrivedParties + "/" + ph.getRegisteredParties)
  }

  def next: Unit = {
    println("next " + this + " arrived=" + ph.getArrivedParties + "/" + ph.getRegisteredParties)
    new Exception("next " + this + " " + Runtime.activity).printStackTrace
    if (dropped)
      throw new ClockUseException
    withExceptions { ph.arriveAndAwaitAdvance }
    println("done with next " + this + " arrived=" + ph.getArrivedParties + "/" + ph.getRegisteredParties)
    new Exception("done with next " + this + " " + Runtime.activity).printStackTrace
  }

  def drop: Unit = {
    if (dropped)
      throw new ClockUseException
    doDrop
  }

  private def doDrop: Unit = {
    new Exception("dropping " + this).printStackTrace
    withExceptions { ph.arriveAndDeregister }
  }

  override def toString = "#" + name + "#arrived=" + ph.getArrivedParties + "/" + ph.getRegisteredParties
}
