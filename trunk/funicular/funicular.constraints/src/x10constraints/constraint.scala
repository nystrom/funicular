package x10constraints

class constraint(exp: String) extends scala.StaticAnnotation { }
class where(exp: Nothing => Boolean) extends scala.StaticAnnotation { }
// class where[T](exp: T => Boolean) extends scala.StaticAnnotation { }
