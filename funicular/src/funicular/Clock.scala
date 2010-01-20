package funicular

import funicular.Intrinsics._

object Clock {
    def apply(name:String): Clock = new funicular.runtime.Clock(name)
    def apply(): Clock = new funicular.runtime.Clock("clock")
}

trait Clock {
    def drop: Unit;
    def registered: Boolean;
    def dropped: Boolean;
    def resume: Unit;
    def next: Unit;
}
