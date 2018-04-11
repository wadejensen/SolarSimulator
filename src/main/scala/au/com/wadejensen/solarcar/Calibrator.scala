package au.com.wadejensen.solarcar

object Calibrator {
  /**
    *
    * @return
    */
  def findNanoTimeOverhead(warmupRuns: Int, testRuns: Int): Double = {
    println("Calculating the overhead of System.nanoTime on this machine...")
    println(s"JVM warming with $warmupRuns runs...")

    val overheads =
      for (i <- 0 until warmupRuns + testRuns) yield {
        if (i == warmupRuns) {
          println(s"JVM is warm. Starting $testRuns testing runs.")
        }
        val t1 = System.nanoTime
        callNanoTime(999999)
        val t2 = System.nanoTime

        val overhead = (t2 - t1) / 1000000.0
        println(s"Iteration $i took an average of $overhead in 1,000,000 " +
          s"calls to System.nanoTime.")
        overhead
      }

    // Ignore the warmup runs and average only the test runs
    overheads.takeRight(testRuns).sum / testRuns
  }

  /**
    * Calls System.nanoTime with as many times as specified.
    * Very short helper function which the JVM happily inlines into
    * Calibrator.findNanoTimeOverhead. Uses while loop with primitive
    * mutable iterator to make it easy for the JVM to unroll the fixed
    * length loop.
    * @param count Number of calls to make to System.nanoTime
    */
  private def callNanoTime( count: Int): Unit = {
    var i = 0
    while ( i < count ) {
      i += 1
      System.nanoTime
    }
  }
}
