package au.com.wadejensen.solarcar.race

import java.io.File

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

    val classLoader = getClass.getClassLoader

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

    val (azimuths, zeniths) = Sun.findSunPositionsSPA(times, lats, lons, alts)
  }
}
