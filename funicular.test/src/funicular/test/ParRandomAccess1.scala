package funicular.test

import funicular.Intrinsics._

object ParRandomAccess1 {
    def main(args: Array[String]): Unit = {
        (new Test).run
    }

    val PARALLELISM = 2
    val logLocalTableSize = 16

    val localTableSize = 1 << logLocalTableSize
    val tableSize = PARALLELISM * localTableSize
    val numUpdates = 4 * tableSize
    val placeMask = PARALLELISM - 1

    val POLY = 0x0000000000000007L
    val PERIOD = 1317624576693539401L

    def HPCCStarts(nn: Long): Long = {
        var n = nn

        while (n < 0)
            n += PERIOD
        while (n > PERIOD)
            n -= PERIOD
        if (n == 0)
            return 0x1L

        val m2 = new Array[Long](64)
        var temp: Long = 0x1

        for (i <- 0 until 64) {
            m2(i) = temp
            temp = (temp << 1) ^ (if (temp < 0) POLY else 0L)
            temp = (temp << 1) ^ (if (temp < 0) POLY else 0L)
        }

        var k = 0
        for (i <- 0 to 62)
            if (((n >> i) & 1) != 0)
                k = i

        var ran:Long = 0x2

        while (k > 0) {
            temp = 0
            for (j <- 0 until 64)
                if (((ran >> j) & 1) != 0)
                    temp ^= m2(j)
            ran = temp
            k -= 1
            if (((n >> k) & 1) != 0)
                ran = (ran << 1) ^ (if (ran < 0) POLY else 0L)
        }

        ran
    }

    class Test {
        def expected = 0.0
        def operations = 1.0 * ParRandomAccess1.numUpdates

        //
        // the benchmark
        //

        class LocalTable (a: Array[Long], mask: Int) {
            def array: Array[Long] = a

            def this(size:Int) = {
                this(Array.fromFunction((i:Int) => i.toLong)(size), size-1)
            }
            
            def update(ran: Long) = {
                //a(ran&mask as int) ^= ran
                val index = (ran & mask).toInt
                a(index) ^= ran
            }
        }

        val tables = Array.fromFunction(_ => new LocalTable(localTableSize))(PARALLELISM)

        final def randomAccessUpdate(tables: Array[LocalTable]) = {
            finish {
                for (p <- 0 until PARALLELISM) async {
                    var ran: Long = ParRandomAccess1.HPCCStarts(p * (numUpdates/PARALLELISM))
                    var i = 0L
                    while (i < numUpdates/PARALLELISM) {
                        val placeId = ((ran>>logLocalTableSize) & placeMask).toInt
                        val table = tables(placeId)
                        table.update(ran)
                        ran = (ran << 1) ^ (if (ran < 0L) POLY else 0L)
                        i += 1
                    }
                }
            }
        }

        def run() = {
              // do the updates
              randomAccessUpdate(tables)

              // First time through do verification. The test by design
              // runs without synchronization and is allowed .01*tableSize errors
              randomAccessUpdate(tables)

              var errors:Int = 0
              for (p <- 0 until PARALLELISM) {
                  val table = tables(p)
                  for (j <- 0 until table.array.length)
                      if (table.array(j) != j)
                          errors += 1
              }

              println(errors+" error(s); allowed " + tableSize/100)

              errors * 100.0 / tableSize
        }
    }
}
