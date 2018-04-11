package au.com.wadejensen.solarcar.model

/**
  * Created by WadeJensen on 22/10/2017.
  */
case class RaceCourse( gpsRoute: Array[Pin],
                       pinDistances: Array[Double],
                       bearings: Array[Double],
                       gradients: Array[Double],
                       checkpointDistances: List[Double])

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
