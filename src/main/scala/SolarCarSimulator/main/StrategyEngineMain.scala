package SolarCarSimulator.main

/**
  * Created by WadeJensen on 11/09/2017.
  */
import java.time._
import java.time.format.DateTimeFormatter

import SolarCarSimulator.StrategyEngine.StrategyEngine
import SolarCarSimulator.StrategyEngine.geography.GeoMath
import com.typesafe.config._

/**
  * Strategy engine entry point. This main calculates the optimal variable
  * speed strategy for the World Solar Challenge.
  * Configurations are specified in the packaged file:
  * src\main\resources\reference.conf
  * This file can be overridden by adding an application.conf file to the
  * classpath, or passing -Dconfig.file=path/to/config-file as a command line
  * arg when running the Strategy Engine application jar
  *
  * All simulation calculations are performed using a one second time step
  */
object StrategyEngineMain extends App {
  val conf = ConfigFactory.load()

  val raceCourse = StrategyEngine.generateRaceCourse(
    conf.getString("strategy-engine.route-file-path"),
    conf.getString("strategy-engine.checkpoint-file-path"))

  val distanceInitial =
    GeoMath.findDistanceFromStart(
      conf.getDouble("strategy-engine.latitude-initial"),
      conf.getDouble("strategy-engine.longitude-initial"),
      raceCourse.gpsRoute,
      raceCourse.pinDistances)

  /* --- Calculate optimal race strategy --- */

  val dateTimeInitial =
    ZonedDateTime
      .of(
        LocalDateTime.parse(conf.getString("strategy-engine.time-initial")),
        ZoneId.of("Australia/Darwin")
      )
      .toEpochSecond

  val raceStartTime =
    LocalTime
      .parse(conf.getString("strategy-engine.race-start-time"))
      .toSecondOfDay

  val raceStartDateTime =
    ZonedDateTime
      .of(
        LocalDate.parse(conf.getString("strategy-engine.race-start-date")),
        LocalTime.parse(conf.getString("strategy-engine.race-start-time")),
        ZoneId.of("Australia/Darwin")
      )
      .toEpochSecond

  // Midnight on the eve of the race (unix timestamp)
  val t0 = raceStartDateTime - raceStartTime

  // Time to start the simulation at. Greater than 8.5 * 3600 when we start
  // midway through the race
  val timeInitial = (dateTimeInitial - t0).toInt

  val morningStartTime =
    LocalTime
      .parse(conf.getString("strategy-engine.morning-start-time"))
      .toSecondOfDay

  val nightStopTime =
    LocalTime
      .parse(conf.getString("strategy-engine.night-stop-time"))
      .toSecondOfDay

  val controlStopLength =
    LocalTime
      .parse(conf.getString("strategy-engine.control-stop-length"))
      .toSecondOfDay

  val speeds =
    List(69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69)
      .map(s => s / 3.6)

  val simulateRaceWithDefaults =
    StrategyEngine.simulateRace(
      timeInitial,
      distanceInitial,
      conf.getDouble("strategy-engine.battery-initial"),
      conf.getInt("strategy-engine.stop-time-to-serve"),
      raceCourse,
      raceStartTime,
      morningStartTime,
      nightStopTime,
      controlStopLength,
      _: List[Double]
    )

  val result = simulateRaceWithDefaults(speeds)

}
