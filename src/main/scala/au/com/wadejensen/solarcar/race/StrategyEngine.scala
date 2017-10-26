package au.com.wadejensen.solarcar.race

import java.io.{BufferedWriter, File, FileWriter}

import au.com.wadejensen.solarcar.{EV, PV}
import au.com.wadejensen.solarcar.geography.{GeoMath, Pin, Poi, Sun}
import purecsv.unsafe.CSVReader

/**
  * Created by WadeJensen on 22/10/2017.
  */
object StrategyEngine {

  /**
    * Construct RaceCourse from user defined data files
    *
    * @param routeFilePath      File path to a csv file containing the latitude,
    *                           longitude, and altitude of GPS pins along the route
    * @param checkpointFilePath File path to a csv file containing the latitude,
    *                           longitude, and town / city name of GPS pins for
    *                           the race mandatory control stops.
    * @return A RaceCourse case class containing route information
    *         formatted for the StrategyEngine planner and optimiser
    */
  def generateRaceCourse(routeFilePath: String,
                         checkpointFilePath: String): RaceCourse = {

    // Race route: read latitude, longitude and altitude data from csv file
    val gpsRoute: Array[Pin] =
      CSVReader[Pin]
        .readCSVFromFile(
          new File(routeFilePath)
        )
        .toArray

    // Race checkpoints: read mandatory control stop locations from csv file
    val checkpoints: Array[Poi] =
      CSVReader[Poi]
        .readCSVFromFile(
          new File(checkpointFilePath)
        )
        .toArray

    val pinDistances = GeoMath.cumulativeDistance(gpsRoute)
    val bearings = GeoMath.findBearings(gpsRoute)
    val gradients = GeoMath.findGradients(gpsRoute)
    val checkpointDistances =
      GeoMath.findCheckpointDistances(checkpoints, gpsRoute, pinDistances)

    RaceCourse(gpsRoute,
               pinDistances,
               bearings,
               gradients,
               checkpointDistances)
  }

  def simulateRace(timeInitial: Int,
                   distanceInitial: Double,
                   batteryInitial: Double,
                   stopTimeToServe: Int,
                   raceCourse: RaceCourse,
                   raceStartTime: Int,
                   t0: Long,
                   morningStartTime: Int,
                   nightStopTime: Int,
                   controlStopLength: Int,
                   speedStrategy: List[Double]): Unit = {

    val scheduler =
      new Scheduler(morningStartTime, nightStopTime, controlStopLength)

    val racePlan = scheduler.generateRacePlan(distanceInitial,
                                              timeInitial,
                                              stopTimeToServe,
                                              raceCourse.checkpointDistances,
                                              speedStrategy)

    val speeds = scheduler.findRaceSpeeds(racePlan: List[RaceLeg])

    val times = new Array[Long](speeds.length)
    for (i <- times.indices) {
      times(i) = t0 + timeInitial + i
    }
    // Cumulative sum
    val distances = speeds.scanLeft(0.0)(_ + _).tail

    val (lats, lons, alts) =
      GeoMath.distance2Gps(distances,
                           raceCourse.pinDistances,
                           raceCourse.gpsRoute)

    val t1 = System.currentTimeMillis()
    val (azimuths, zeniths) = Sun.findSunPositionsGrena3(times, lats, lons, alts)
    val t2 = System.currentTimeMillis()

    println(t2-t1)

    val t3 = System.currentTimeMillis()
    val (directRadiation, diffuseRadiation) = Sun.findIrrandiance(zeniths)
    val t4 = System.currentTimeMillis()

    println(t4-t3)

    val t5 = System.currentTimeMillis()
    // Get the angles of the array with respect to the solar normal
    val sun2Arr = PV.tiltArray(zeniths, speeds)
    val t6 = System.currentTimeMillis()

    println(t6-t5)

    val t7 = System.currentTimeMillis()
    val inputPower =
      PV.findArrayPower(sun2Arr, directRadiation, diffuseRadiation)
    val t8 = System.currentTimeMillis()

    println(t8-t7)

    val motorPower = EV.findMotorPower(speeds)

    Unit

  }
}
