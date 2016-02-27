package funicular.test

import funicular._
import scala.util.Random

/* This class represents a real-world problem in graphics engines --
 * determining which objects in a large sprawling world are close enough to the
 * camera to be considered for rendering.  The naive implementation produces a
 * lot of objects and is thus a good benchmark for garbage collection in X10.
 *
 * @Author Dave Cunningham
 * @Author Vijay Saraswat
*/
object GCSpheres {

    type Real = Float

    case class Vector3(val x:Real, val y:Real, val z:Real) {
        def +(other:Vector3) = Vector3(x+other.x, y+other.y, z+other.z)
        def -(other:Vector3) = Vector3(x-other.x, y-other.y, z-other.z)
        def neg = Vector3(-x, -y, -z)
        def length = Math.sqrt(length2)
        def length2 = x*x + y*y + z*z
    }

    class WorldObject private (val pos: Vector3, val renderingDistance:Real) {

        def this(x:Real, y:Real, z:Real, r:Real) = {
            this(Vector3(x,y,z), r)
        }

        def intersects(home: Vector3) =
                (home - pos).length2 < renderingDistance * renderingDistance
    }


    def main(args: Array[String]) = {

        val reps = 7500

        // The following correspond to a modern out-door computer game:
        val num_objects = 50000
        val world_size = 6000
        val obj_max_size = 400

        val ran = new Random(0)

        // the array can go on the heap
        // but the elements ought to be /*inlined*/ in the array
        val spheres = Array.tabulate(num_objects)(
            (i:Int) => {
                val x = (ran.nextDouble()*world_size).toFloat
                val y = (ran.nextDouble()*world_size).toFloat
                val z = (ran.nextDouble()*world_size).toFloat
                val r = (ran.nextDouble()*obj_max_size).toFloat
                new WorldObject(x,y,z,r)
            })

        val time_start = System.nanoTime()

        var counter: Long = 0

        // HOT LOOP BEGINS
        for (frame <- 1 to reps) {
            val x = (ran.nextDouble()*world_size).toFloat
            val y = (ran.nextDouble()*world_size).toFloat
            val z = (ran.nextDouble()*world_size).toFloat

            val pos = Vector3(x,y,z)

            val n = spheres.map((s:WorldObject) => if (s.intersects(pos)) 1 else 0).reduce((x:Int,y:Int) => x+y)
            counter += n

            /*
          finish {
            foreach (spheres) {
                sphere => {
                    counter += 1
                }
            }
          }
          */

            /*
            for (sphere <- spheres) {
                if (sphere.intersects(pos)) {
                    counter += 1
                }
            }
            */
        }
        // HOT LOOP ENDS

        val time_taken = System.nanoTime() - time_start
        println("Total time: "+time_taken/1E9)

        val expected = 109173
        if (counter != expected) {
            println("number of intersections: "+counter
                                +" (expected "+expected+")")
            sys.exit(1)
        }
    }
}
