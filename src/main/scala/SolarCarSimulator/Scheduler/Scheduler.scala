package SolarCarSimulator

/**
  *
  * @param dist1 Initial position as distance from starting line (meters)
  * @param t1 Initial time as seconds since midnight of day 0
  * @param dist2 Position at the end of race leg from starting line (meters)
  * @param t2 Time at the end of race leg
  * @param speed Speed travelled during race leg (meters / second)
  * @param stopDuration Time until the next race leg starts (seconds)
  */
case class RaceLeg( dist1: Double, t1: Int,
                    dist2: Double, t2: Int,
                    speed: Double, stopDuration: Int)

object Scheduler {

  val ONE_DAY = 24 * 60 * 60

  def generateRacePlan(initialDistance: Double,
                       initialTime: Int,
                       checkpoints: List[Double],
                       speeds: List[Double],
                       NIGHT_STOP_TIME: Int,
                       CHECKPOINT_STOP_LENGTH: Int): Unit = { // List[RaceLeg]

    val NIGHT_STOP_DURATION = (ONE_DAY - NIGHT_STOP_TIME) + 8 * 60 * 60

    def planRace(
      dist1: Double,
      t1: Int,
      speeds: List[Double],
      checkpoints: List[Double],
      racePlan: List[RaceLeg]): List[RaceLeg] = {

      val timeToNextCheckpoint = ((checkpoints.head - dist1) / speeds.head).toInt
      val timeToNextNightStop = NIGHT_STOP_TIME - (t1 % ONE_DAY)

      if ( timeToNextCheckpoint < timeToNextNightStop ) {
        // We're going to a control stop
        val leg = RaceLeg(
          dist1, t1,
          checkpoints.head, t1 + timeToNextCheckpoint.toInt,
          speeds.head, CHECKPOINT_STOP_LENGTH )

        planRace(
          checkpoints.head, t1 + timeToNextCheckpoint.toInt + CHECKPOINT_STOP_LENGTH,
          speeds.tail, checkpoints.tail,
          racePlan :+ leg)
      }
      else {
        // We're heading to camp for the night
        val leg = RaceLeg(
          dist1, t1,
          dist1 + timeToNextNightStop * speeds.head,
          t1 + timeToNextNightStop.toInt,
          speeds.head, NIGHT_STOP_DURATION )

        planRace(
          checkpoints.head, t1 + timeToNextNightStop.toInt + NIGHT_STOP_DURATION,
          speeds.tail, checkpoints.tail,
          racePlan :+ leg)
      }
    }

    planRace(initialDistance, initialTime, speeds, checkpoints, List.empty[RaceLeg])
  }

}



//import SolarCarSimulator.StrategyEngine.StrategyEngine.Geography.Location
//import SolarCarSimulator.StrategyEngine.StrategyEngine.StopType.StopType
//
//object StopType extends Enumeration {
//  type StopType = Value
//  val CHECKPOINT, NIGHT = Value
//}
//
//case class StopPoint(val time: Long, val stopType: StopType, val duration: Int)
//
//class Scheduler( route: Array[Location], timeStep: Int, startTime: Int = 0 ) {
//
//}
