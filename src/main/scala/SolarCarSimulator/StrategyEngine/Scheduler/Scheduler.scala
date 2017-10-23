package SolarCarSimulator

import scala.collection.mutable.ListBuffer

/**
  *
  * @param d1 Initial position as distance from starting line (meters)
  * @param t1 Initial time as seconds since midnight of day 0
  * @param d2 Position at the end of race leg from starting line (meters)
  * @param t2 Time at the end of race leg
  * @param speed Speed travelled during race leg (meters / second)
  * @param stopDuration Time until the next race leg starts (seconds)
  */
case class RaceLeg(d1: Double,
                   t1: Int,
                   d2: Double,
                   t2: Int,
                   speed: Double,
                   stopDuration: Int,
                   stopType: String)

class Scheduler(val morningStartTime: Int,
                val nightStopTime: Int,
                val checkpointStopLength: Int) {

  val secondsInDay = 24 * 60 * 60
  val daysInRace = 6

  val nightStopDuration = (secondsInDay - nightStopTime) + morningStartTime

  def generateRacePlan(initialDistance: Double,
                       initialTime: Int,
                       stopTimeToServe: Int,
                       checkpoints: List[Double],
                       speeds: List[Double]): List[RaceLeg] = {

    // List of night stop times, as seconds from HHmm = 0000 on day 0 of race
    var nightStopsLeft = ListBuffer
      .range(0, daysInRace)
      .map(day => day * secondsInDay + nightStopTime )
      .filter(_ > initialTime)

    // Create a list of remaining checkpoints in case we start the race midway
    var checkpointsLeft =
      checkpoints
        .filter(_ > initialDistance)
        .to[ListBuffer]

    var speedsLeft = speeds.to[ListBuffer]
    val racePlan = ListBuffer.empty[RaceLeg]

    racePlan.append(
      RaceLeg(initialDistance,
              initialTime,
              initialDistance,
              initialTime,
              Int.MaxValue,
              stopTimeToServe,
              "START"))

    while (checkpointsLeft.nonEmpty) {
      val prior = racePlan.last
      val raceleg = planRaceLeg(prior.d2,
                                prior.t2 + prior.stopDuration,
                                speedsLeft.head,
                                checkpointsLeft.head,
                                nightStopsLeft.head)

      raceleg.stopType match {
        case "CHECKPOINT" => {
          checkpointsLeft = checkpointsLeft.tail
          speedsLeft = speedsLeft.tail
        }
        case "NIGHT" => {
          nightStopsLeft = nightStopsLeft.tail
          speedsLeft = speedsLeft.tail
        }
        case "BOTH" => {
          checkpointsLeft = checkpointsLeft.tail
          nightStopsLeft = nightStopsLeft.tail
          speedsLeft = speedsLeft.tail
        }
        case _ => throw new Exception("Scheduler: Unknown stop type.")
      }
      racePlan.append(raceleg)
    }
    racePlan.to[List]
  }

  def planRaceLeg(d1: Double,
                  t1: Int,
                  speed: Double,
                  checkpoint: Double,
                  nightStop: Int): RaceLeg = {

    val timeToCheckpoint = ((checkpoint - d1) / speed).toInt
    val timeToNextNightStop = nightStopTime - (t1 % secondsInDay)

    /* --- Determine where/when we will stop next and the stop type --- */
    // Check if stop is control checkpoint
    if (timeToCheckpoint <= timeToNextNightStop) {
      // Check if the control stop will overlap with night stop
      if (timeToNextNightStop - timeToCheckpoint < checkpointStopLength) {
        // Both a control and night stop
        val timeServedToday = timeToNextNightStop - timeToCheckpoint
        RaceLeg(d1,
                t1,
                checkpoint,
                t1 + timeToCheckpoint,
                speed,
                checkpointStopLength + nightStopDuration,
                "BOTH")
      } else {
        // Just a normal checkpoint stop
        RaceLeg(d1,
                t1,
                checkpoint,
                t1 + timeToCheckpoint,
                speed,
                checkpointStopLength,
                "CHECKPOINT")
      }
    } else {
      // We're heading to camp for the night
      RaceLeg(d1,
              t1,
              timeToNextNightStop * speed + d1,
              nightStop,
              speed,
              nightStopDuration,
              "NIGHT")
    }
  }
}
