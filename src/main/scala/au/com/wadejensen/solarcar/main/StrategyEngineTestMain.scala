package au.com.wadejensen.solarcar.main

import java.io.{BufferedWriter, File, FileWriter}
import java.time._
import java.time.format.DateTimeFormatter

import au.com.wadejensen.solarcar.{GeoMath, StrategyEngine}
import com.typesafe.config.ConfigFactory

/**
  * Test version of StrategyEngineTestMainStrategy.
  * Generates a standard set of results and writes them to a csv.
  * By comparing this csv to a known good serial implementation we can
  * be assured of program correctness.
  * All simulation calculations are performed using a one second time step.
  */
object StrategyEngineTestMain extends App {

  println("Running test main...")

  val currentTime =
    LocalDateTime.now
      .format(DateTimeFormatter.ofPattern("yyyyMMdd'T'hhmm"))
  println(s"$currentTime :: Starting strategy engine and profiler.")

  val conf = ConfigFactory.load

  val cpus = Runtime.getRuntime.availableProcessors
  val parallelism = conf.getString("strategy-engine.parallelism")

  println(s"$cpus logical CPU cores detected.\n" +
    s"Parallelism set to $parallelism")

  /** ------------------------- Timed code starts ----------------------- **/
  val codeTimingStart = System.nanoTime
  /** -------- Read config file and configure race course / rules ------- **/
  val timerRaceCourse1 = System.nanoTime

  val raceCourse = StrategyEngine.generateRaceCourse(
    conf.getString("strategy-engine.route-file-path"),
    conf.getString("strategy-engine.checkpoint-file-path"))

  val distanceInitial =
    GeoMath.findDistanceFromStart(
      conf.getDouble("strategy-engine.latitude-initial"),
      conf.getDouble("strategy-engine.longitude-initial"),
      raceCourse.gpsRoute,
      raceCourse.pinDistances)

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

  // Time to start the simulation at. Greater than 8.5 * 3600 means we start
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

  val file = new File(s"strategy-results-short-$currentTime.csv")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write("Speed, Final Battery, Duration, Min Battery, Max Battery, Max Motor, Max Array \n")

  val raceSpeeds = List.tabulate[Double](400)( i => 50 + i*0.1 )

  for ( raceSpeed <- raceSpeeds.iterator ) {

    //val raceSpeed = conf.getDouble("strategy-engine.race-speed")
    val speeds = List.fill(100)(raceSpeed).map(s => s / 3.6)
    val timerRaceCourse2 = System.nanoTime

    /** --------------------------- Simulate race ------------------------ **/
    val simulateRaceWithDefaults =
      StrategyEngine.simulateRace(
        timeInitial,
        distanceInitial,
        conf.getDouble("strategy-engine.battery-initial"),
        conf.getInt("strategy-engine.stop-time-to-serve"),
        raceCourse,
        raceStartTime.toInt,
        t0,
        morningStartTime,
        nightStopTime,
        controlStopLength,
        0,
        0,
        _: List[Double],
        bw,
        false
      )

    val codeTiming = simulateRaceWithDefaults(speeds)

    /** -------------------- Timed code has already ended ----------------- **/

  }
  bw.close
  val filePath = file.getAbsolutePath
  println(s"Test output written saved to file: $filePath.\n")
}
