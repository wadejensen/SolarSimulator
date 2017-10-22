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

object StrategyEngineMain extends App {
  val conf = new Conf(args)

  strategyEngine(conf)

  def strategyEngine(conf: Conf) {
    val TIME_STEP = 1 // (seconds)
    val ONE_DAY = 24 * 60 * 60 / TIME_STEP

    /* --- Race rules --- */
    // Start date in seconds from epoch
    val raceStartDate = new DateTime(2017, 10, 8, 0, 0, 0).getMillis / 1000
    // Race start time (seconds after midnight)
    val raceStartTime = (8 * 60 + 30) * 60 / TIME_STEP
    // Time the race starts each morning
    val MORNING_START_TIME = 8 * 60 * 60 / TIME_STEP
    // Time the race stops at night
    val NIGHT_STOP_TIME = 17 * 60 * 60 / TIME_STEP
    val NIGHT_STOP_DURATION = 24 * 60 * 60 + MORNING_START_TIME - NIGHT_STOP_TIME
    // How long are mandatory distance-based control stops?
    val CONTROL_STOP_LENGTH = 30 * 60 / TIME_STEP

    // Race route: read latitude, longitude and altitude data from csv file
    val gpsRoute: Array[Pin] =
      CSVReader[Pin]
        .readCSVFromFileName(conf.routeFile())
        .toArray

    // Mandatory control stop locations
    val checkpoints: Array[Poi] =
      CSVReader[Poi]
        .readCSVFromFileName(conf.checkpointFile())
        .toArray

    val pinDistances = GeoMath.cumulativeDistance(gpsRoute)
    val raceLength = pinDistances.last

    val bearings = GeoMath.findBearings(gpsRoute)
    val gradients = GeoMath.findGradients(gpsRoute)

    /* --- Initial conditions --- */
    val initialTime = raceStartTime
    val initialLocation = Pin(-12.46284, 130.84179, 10.555) // Darwin
    val initialBattery = 1

    val distanceInitial = GeoMath.findDistanceFromStart(
      initialLocation.lat,
      initialLocation.long,
      gpsRoute,
      pinDistances
    )

    val checkpointDistances =
      GeoMath.findCheckpointDistances(checkpoints, gpsRoute, pinDistances)


    /* --- Calculate optimal race strategy --- */

    // A bunch of parallel stuff, wrap around
       // results: RaceResult = simulateRace


    /* --- Configure race strategy --- */


    // Speed in meters / TIME_STEP
    val speeds =
    List(60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60).map( s =>
      s / 3.6 / TIME_STEP)

    val scheduler =
      new Scheduler(MORNING_START_TIME, NIGHT_STOP_TIME, CONTROL_STOP_LENGTH)

    val racePlan = scheduler.generateRacePlan(distanceInitial,
      initialTime,
      stopTimeToServe,
      checkpointDistances,
      speeds)

    val

    print(racePlan)
  }
}


