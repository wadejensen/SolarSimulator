package au.com.wadejensen.solarcar.model

class CodeTiming( val t1: Long,
                  val t2: Long,
                  val courseRules: Long,
                  val planRace: Long,
                  val geospatial: Long,
                  val sunPosition: Long,
                  val solarPower: Long,
                  val motorPower: Long,
                  val batteryPower: Long) {

  val nanoCallCount = 15

  /**
    * A helper method for timings of each code section to be displayed and saved
    * to file in a csv friendly format.
    *
    * Each timing will have two calls to System.nanoTime, of which only
    * one with be included in the measurement. We can subtract the average
    * overhead of the timing function to adjust the results.
    * The overall time includes @param nanoCallCount calls to System.nanoTime;
    * we adjust accordingly.
    *
    * @param i The iteration number of the current performance test.
    * @param nanoTimeOverhead The average overhead of System.nanoTime in ns
    * @return A csv-friendly string which can be written to file cleanly.
    */
  def toString(i: Int, nanoTimeOverhead: Double): String = {
    val totalTime = (t2 - t1 - nanoCallCount * nanoTimeOverhead) / 1000
    val courseRulesTime = (courseRules - nanoTimeOverhead) / 1000
    val planRaceTime = (planRace - nanoTimeOverhead) / 1000
    val geospatialTime = (geospatial - nanoTimeOverhead) / 1000
    val sunPositionTime = (sunPosition - nanoTimeOverhead) / 1000
    val solarPowerTime = (solarPower - nanoTimeOverhead) / 1000
    val motorPowerTime = (motorPower - nanoTimeOverhead) / 1000
    val batteryPowerTime = (batteryPower - nanoTimeOverhead) / 1000

    // Calculate timings as a proportion of total time
    val totalTimePC = totalTime / totalTime * 100
    val courseRulesPC = courseRulesTime / totalTime * 100
    val planRacePC =   planRaceTime / totalTime * 100
    val geospatialPC = geospatialTime / totalTime * 100
    val sunPositionPC = sunPositionTime / totalTime * 100
    val solarPowerPC = solarPowerTime / totalTime * 100
    val motorPowerPC = motorPowerTime / totalTime * 100
    val batteryPowerPC = batteryPowerTime / totalTime * 100

    s"Iteration,    code section,     duration (us),        duration (%)\n"+
    s"   $i,        Total Time,       $totalTime,   $totalTimePC \n"+
    s"   $i,        Course Rules,     $courseRulesTime,   $courseRulesPC \n"+
    s"   $i,        Plan Race,        $planRaceTime,    $planRacePC \n" +
    s"   $i,        Geospatial,       $geospatialTime,    $geospatialPC \n" +
    s"   $i,        Sun Position,     $sunPositionTime,   $sunPositionPC \n"+
    s"   $i,        Solar Power,      $solarPowerTime,    $solarPowerPC \n" +
    s"   $i,        Motor Power,      $motorPowerTime,    $motorPowerPC \n" +
    s"   $i,        Battery Power,    $batteryPowerTime,    $batteryPowerPC \n"
  }
}


