import opencl.OpenCL._

object Test {
    def main(args: Array[String]) {
        val x = Local[int4_t.type]("x", int4_t)
        val y = Local[int4_t.type]("y", int4_t)
        val z = Local[int4_t.type]("z", int4_t)
        val p = x + y
        println(p)
        val s = generate(p)
        println(s)
    }
}
