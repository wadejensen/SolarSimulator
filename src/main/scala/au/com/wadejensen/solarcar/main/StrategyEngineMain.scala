package au.com.wadejensen.solarcar.main

import java.io.{BufferedWriter, File, FileWriter}
import java.time._
import java.time.format.DateTimeFormatter

import au.com.wadejensen.solarcar.{Calibrator, GeoMath, StrategyEngine}
import com.typesafe.config.ConfigFactory
import org.nd4j.linalg.api.buffer.DataBuffer
import org.nd4j.linalg.api.buffer.util.DataTypeUtil
import org.nd4j.linalg.api.complex.IComplexNDArray
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.ops.transforms.Transforms._
import org.nd4s.Implicits._

/**
  * Strategy engine entry point. This main calculates the performance of
  * a Challenger class solar vehicle in the World Solar Challenge.
  * Configurations are specified in the packaged file:
  * src\main\resources\reference.conf
  * This file can be overridden by adding an application.conf file to the
  * classpath, or passing -Dconfig.file=path/to/config-file as a command line
  * arg when running the Strategy Engine application jar.
  *
  * All simulation calculations are performed using a one second time step.
  */
object StrategyEngineMain extends App {


  val x = Nd4j.createComplex(Array(1,2,3,4,5,6,7,8,9,10).toNDArray)
  val y: IComplexNDArray = Nd4j.createComplex(Array(2,2,2,2,2,2,2,2,2,2).toNDArray)

  val z =

  val currentTime =
    LocalDateTime.now
      .format(DateTimeFormatter.ofPattern("yyyyMMdd'T'hhmm"))
  println(s"$currentTime :: Starting strategy engine and profiler.")

  val conf = ConfigFactory.load
  // Calibrate timing mechanism to get overhead of System.nanoTime
  val warmupRuns = conf.getInt("strategy-engine.warmup-runs")
  val testRuns = conf.getInt("strategy-engine.test-runs")

  val timerOverhead =
    Calibrator.findNanoTimeOverhead( warmupRuns, testRuns )
  println(s"Average overhead calculated as $timerOverhead ns.\n")

  val cpus = Runtime.getRuntime.availableProcessors
  val parallelism = conf.getString("strategy-engine.parallelism")

  // Initialise ND4J
  DataTypeUtil.setDTypeForContext(DataBuffer.Type.DOUBLE)
  println(Nd4j.getExecutioner.getEnvironmentInformation)
  Nd4j.create(1)
//  DataTypeUtil.setDTypeForContext(DataBuffer.Type.FLOAT)
//  NativeOpsHolder.getInstance().getDeviceNativeOps().setElementThreshold(100)
//  NativeOpsHolder.getInstance().getDeviceNativeOps().setTADThreshold(64)

  println(s"$cpus logical CPU cores detected.\n" +
    s"Parallelism set to $parallelism")

  // Write out timing results
  val file = new File(s"strategy-timing-par-$parallelism-time-$currentTime.csv")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(s"------------------------------------------------------\n")
  bw.write(s"Start time = $currentTime\n")
  bw.write(s"Available cores = $cpus\n")
  bw.write(s"Parallelism = $parallelism\n")
  bw.write(s"JVM warming with $warmupRuns runs.\n")

  for (i <- 0 until warmupRuns + testRuns) {
    val isLast =
      if (i == (warmupRuns + testRuns - 1)) true
      else false
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

    val raceSpeed = conf.getDouble("strategy-engine.race-speed")
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
        codeTimingStart,
        timerRaceCourse2 - timerRaceCourse1,
        _: List[Double],
        null,
        isLast
      )

    val codeTiming = simulateRaceWithDefaults(speeds)
    /** -------------------- Timed code has already ended ----------------- **/

    // Print timings to console on the last iteration
    if ( isLast) println(codeTiming.toString(i, timerOverhead))

    if (i == warmupRuns) {
      bw.write(s"------------------------------------------------------\n")
      bw.write(s"JVM is warm. Start $testRuns testing runs.\n")
    }
    bw.write(codeTiming.toString(i, timerOverhead))
  }
  bw.close()
  val filePath = file.getAbsolutePath
  println(s"Code timing statistics saved to file: $filePath.\n")
}
