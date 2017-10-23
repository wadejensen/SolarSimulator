package SolarCarSimulator.StrategyEngine

import SolarCarSimulator.geography.{GeoMath, Pin, Poi}
import purecsv.unsafe.CSVReader
import java.io.File

import SolarCarSimulator.Scheduler

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
          new File(classLoader.getResource(routeFilePath).getFile())
        )
        .toArray

    // Race checkpoints: read mandatory control stop locations from csv file
    val checkpoints: Array[Poi] =
      CSVReader[Poi]
        .readCSVFromFile(
          new File(classLoader.getResource(checkpointFilePath).getFile())
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
                   raceStartTime: Long,
                   morningStartTime: Int,
                   nightStopTime: Int,
                   controlStopLength: Int,
                   speeds: List[Double]): Unit = {

    val scheduler =
      new Scheduler(morningStartTime, nightStopTime, controlStopLength)

    val racePlan = scheduler.generateRacePlan(distanceInitial,
                                              timeInitial,
                                              stopTimeToServe,
                                              raceCourse.checkpointDistances,
                                              speeds)

    println(racePlan)
  }
}
