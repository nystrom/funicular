package x10

import x10.Intrinsics._

object Clock {
    def apply(name:String): Clock = new x10.runtime.Clock(name)
    def apply(): Clock = new x10.runtime.Clock("clock")
}

trait Clock {
    def drop: Unit;
    def registered: Boolean;
    def dropped: Boolean;
    def resume: Unit;
    def next: Unit;
}
