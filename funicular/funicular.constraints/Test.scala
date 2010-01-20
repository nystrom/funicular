import x10constraints.constraint
import x10constraints.where

object Test {
  val t = 5
  val u: Int = 5
  val w: Int @where(_ => true) = 5
  val y: Int @where((x:Int) => x > 4) = 5
  val z: Int @where((x:Int) => x == 5) = 5
  val x: Int @where((x:Int) => x >= 5) = 5
  val v: Int @where((x:Int) => x > 0) = 5
  // val s: Int @where((x:Int) => x == t) = 5
  def main(args: Array[String]) = ()
}
