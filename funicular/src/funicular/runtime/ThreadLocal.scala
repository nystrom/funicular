package funicular.runtime

import java.lang.{ThreadLocal => TL}

class ThreadLocal[T](init: => T) extends TL[T] with Function0[T] {
  override def initialValue: T = init
  def apply = get
  def withValue[S](thunk: (T => S)): S = thunk(get)
}

