package SolarCarSimulator.StrategyEngine

/**
  * Created by WadeJensen on 11/09/2017.
  */

import SolarCarSimulator.Geography.{GeoMath, Location, Pin, Poi}
import SolarCarSimulator.{RaceLeg, Scheduler}
import org.joda.time.DateTime
import org.rogach.scallop._
import purecsv.unsafe._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  // Fancy reflection is used to infer option name from variable name
  val checkpointFile = opt[String](required = true)
  val routeFile = opt[String](required = true)

  verify()


}
object StrategyEngine extends App {
  val conf = new Conf(args) // Note: This line also works for "object Main extends App"
  //println("routeFile is: " + conf.routeFile())

  val TIME_STEP = 1 // (seconds)
  // Race rules

  val ONE_DAY = 24 * 60 * 60 / TIME_STEP

  // Start date in seconds from epoch
  val raceStartDate = new DateTime(2017, 10, 12, 0, 0, 0).getMillis / 1000
  // Race start time (seconds after midnight)
  val raceStartTime = (8 * 60 + 30) * 60 / TIME_STEP
  // Time the race starts and finishes each day
  val MORNING_START_TIME = 8 * 60 * 60 / TIME_STEP
  // seconds after midnight
  val NIGHT_STOP_TIME = 17 * 60 * 60 / TIME_STEP
  // seconds after midnight
  val NIGHT_STOP_DURATION = 24 * 60 * 60 + MORNING_START_TIME - NIGHT_STOP_TIME

  val CONTROL_STOP_LENGTH = 30 * 60 / TIME_STEP

  val initialBattery = 1
  // Fraction of total capacity
  val initialLocation = Pin(-12.46284, 130.84179, 10.555) // Darwin

  val initialTime = raceStartTime

  // Race route
  // Read latitude, longitude and altitude data from csv file
  val gpsRoute: Array[Pin] =
    CSVReader[Pin].readCSVFromFileName(conf.routeFile()).toArray

  // Mandatory control stop locations
  val checkpoints: Array[Poi] =
    CSVReader[Poi].readCSVFromFileName(conf.checkpointFile()).toArray

  val distances = GeoMath.cumulativeDistance(gpsRoute)

  val raceLength = distances.last

  val bearings = GeoMath.findBearings(gpsRoute)
  val gradients = GeoMath.findGradients(gpsRoute)

  val checkpointDistances = GeoMath.findCheckpointDistances(
    checkpoints,
    gpsRoute,
    distances)

  val distanceInitial = GeoMath.findDistanceFromStart(
    initialLocation.lat,
    initialLocation.long,
    gpsRoute,
    distances
  )

  // Speed in meters / TIME_STEP
  val speeds = List(60,60,60,60,60,60,60,60,60,60,60,60,60,60,60).map( s => s / 3.6 / TIME_STEP )

  // If we have started the race already, ignore any checkpoints we have passed
  checkpointDistances.filter( distanceInitial < _ )

  val racePlan = Scheduler.generateRacePlan(
    distanceInitial,
    initialTime,
    checkpointDistances,
    speeds,
    NIGHT_STOP_TIME,
    CONTROL_STOP_LENGTH)

  print(racePlan)
}
