package funicular.test

import funicular.Intrinsics._

object FRASimpleDist {

    // shadow funicular async
    // def async(b: => Unit): Unit = b

  class LocalTable( val a: Array[Long], val mask: Int) {
    
    def this(size:Int) {
        this(Array.fromFunction((i:Int) => i.toLong)(size), size-1)
    }
    
    def update(ran:Long) {
        //a(ran&mask as int) ^= ran
        val index = (ran&mask).toInt
        a(index) = a(index) ^ ran
    }
  }

    val POLY = 0x0000000000000007L
    val PERIOD = 1317624576693539401L
    val MAX_PLACES = 8

    // Utility routine to start random number generator at Nth step
    def HPCC_starts(nn:Long): Long = {
        var n = nn
        val m2 = new Array[Long](64)
        while (n < 0) n += PERIOD
        while (n > PERIOD) n -= PERIOD
        if (n == 0) return 0x1L
        var temp:Long = 0x1
        for (i <- 0 until 64) {
            m2(i) = temp
            temp = (temp << 1) ^ (if (temp < 0) POLY else 0L)
            temp = (temp << 1) ^ (if (temp < 0) POLY else 0L)
        }
        var i = (() => { for (i <- (0 to 62).reverse)
                          if (((n >> i) & 1) != 0) return i
                        0 })()
        var ran:Long = 0x2
        while (i > 0) {
            temp = 0
            for (j <- 0 until 64)
                if (((ran >> j) & 1) != 0) temp ^= m2(j)
            ran = temp
            i -= 1
            if (((n >> i) & 1) != 0)
                ran = (ran << 1) ^ (if (ran < 0) POLY else 0L)
        }
        ran
    }

    def randomAccessUpdate(
	num_updates: Long,
        logLocalTableSize: Long,
        tables: Array[LocalTable]
    ) {
        finish {
            for (p <- 0 until MAX_PLACES) {
            val vp = p
              async {
                var ran:Long = HPCC_starts(vp*(num_updates/MAX_PLACES))
                for (i <- 0 until (num_updates/MAX_PLACES).toInt) {
                    val placeId = ((ran>>logLocalTableSize) & (MAX_PLACES-1)).toInt
                    val vran = ran
                    async {
                        tables(placeId).update(vran)
                    }
                    ran = (ran << 1) ^ (if (ran<0L) POLY else 0L)
                }
                }
            }
        }
    }


    def main(args:Array[String]) {
      finish {
        // calculate the size of update array (must be a power of 2)
        val logLocalTableSize = if (args.length > 1 && (args(0) eq "-m")) args(1).toInt else 12
        val localTableSize = 1<<logLocalTableSize
        val tableSize = localTableSize*MAX_PLACES
        val num_updates = 4*tableSize

        // create local tables
        val tables = Array.fromFunction[LocalTable]((i:Int) => new LocalTable(localTableSize))(MAX_PLACES)

        // print some info
        println("Main table size   = 2^" +logLocalTableSize + "*" + MAX_PLACES+" = " + tableSize+ " words")
        println("Number of places = " + MAX_PLACES)
        println("Number of updates = " + num_updates)

        // time it
        var cpuTime:Double = -now()
        randomAccessUpdate(num_updates, logLocalTableSize, tables)
        cpuTime += now()

        // print statistics
        val GUPs = (if (cpuTime > 0.0) 1.0 / cpuTime else -1.0) * num_updates / 1e9
        println("CPU time used  = "+cpuTime+" seconds")
        println(GUPs+" Billion(10^9) Updates per second (GUP/s)")

        // repeat for testing.
        randomAccessUpdate(num_updates, logLocalTableSize, tables)
        for (i <- 0 until MAX_PLACES) {
            async {
	        val table = tables(i)
                var err:Int = 0
                for (j <- 0 until table.a.length)
                    if (table.a(j) != j) err += 1
                println("Found " + err + " errors.")
            }
        }
      }
    }

    def now() = System.nanoTime() * 1e-9D
}
