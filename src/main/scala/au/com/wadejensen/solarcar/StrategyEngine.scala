package au.com.wadejensen.solarcar

import java.io.{BufferedWriter, File}
import java.time._
import java.time.format.DateTimeFormatter

import au.com.wadejensen.solarcar.model._
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
                   codeTimingStart: Long,
                   timerRaceCourse: Long,
                   speedStrategy: List[Double],
                   bw: BufferedWriter,
                   isLast: Boolean): CodeTiming = {

    /** ------ Plan when to drive and at what speed during the race ------**/
    val timerPlanRace1 = System.nanoTime
    val scheduler =
      new Scheduler(morningStartTime, nightStopTime, controlStopLength)

    val racePlan = scheduler.generateRacePlan(distanceInitial,
                                              timeInitial,
                                              stopTimeToServe,
                                              raceCourse.checkpointDistances,
                                              speedStrategy)

    val speeds = scheduler.findRaceSpeeds(racePlan: List[RaceLeg])
    val timerPlanRace2 = System.nanoTime

    /** ----- Calculate when and where the car is during in the race ----- **/
    val timerGeospatial1 = System.nanoTime

    // the unix timestamp for every second of the race
    val times = Array.tabulate[Long](speeds.length)(i => t0 + timeInitial + i )


    // Cumulative sum
    val distances = speeds.scanLeft(0.0)(_ + _).tail

    val (lats, lons, alts) =
      GeoMath.distance2Gps(distances,
                           raceCourse.pinDistances,
                           raceCourse.gpsRoute)
    val timerGeospatial2 = System.nanoTime

    /** --------------- Calculate the position of the sun --------------- **/

    val timerSunPosition1 = System.nanoTime
    val zeniths = Sun.findSolarZenithGrena3(times, lats, lons, alts)
    val timerSunPosition2 = System.nanoTime

    /** ---- Calculate the intensity of the sun and solar array power ---- **/
    val timerSolar1 = System.nanoTime
    val (directRadiation, diffuseRadiation) = Sun.findIrrandiance(zeniths)

    // Get the angles of the array with respect to the solar normal
    val sun2Arr = PV.tiltArray(zeniths, speeds)

    val solarPower =
      PV.findArrayPower(sun2Arr, directRadiation, diffuseRadiation)
    val timerSolar2 = System.nanoTime

    /** --------------- Calculate power usage by the motor --------------- **/
    val timerMotor1 = System.nanoTime
    val motorPower = EV.findMotorPower(speeds)
    val deltaPE = EV.findPotentialEnergy(alts)
    val timerMotor2 = System.nanoTime

    /** ------------ Calculate battery charging / discharging ------------ **/
    val timerBattery1 = System.nanoTime
    val netPower = Battery.findNetPower(motorPower, solarPower)
    val netEnergy = Battery.findNetEnergy(netPower, deltaPE)

    val soc = Battery.findStateOfCharge(netEnergy)

    val timerBattery2 = System.nanoTime

    val codeTimingFinish = System.nanoTime
    /** ------------------------ End of timed code ----------------------- **/

    /** ------------------------- Print race results --------------------- **/
    val raceDuration = times.length.toDouble / 3600.00

    val startDatetime = ZonedDateTime.ofInstant(
      Instant.ofEpochSecond(t0 + timeInitial),
      ZoneId.of("Australia/Darwin")
    )
    val finishDatetime = ZonedDateTime.ofInstant(
      Instant.ofEpochSecond(t0 + timeInitial + times.length),
      ZoneId.of("Australia/Darwin")
    )

    val formattedStart =
      startDatetime.format(DateTimeFormatter.RFC_1123_DATE_TIME)
    val formattedFinish =
      finishDatetime.format(DateTimeFormatter.RFC_1123_DATE_TIME)

    val duration = Duration.ofSeconds(times.length)
    val days = duration.toDays
    val hrs = duration.toHours % 24
    val mins = duration.toHours % 60
    val secs = duration.getSeconds % 60

    val peakSolar = solarPower.max
    val peakMotor = motorPower.max
    val minBatt = soc.min
    val finalBatt = soc.last

    // Time spent driving
    val driveTime = speeds.count( _ != 0 )
    val numCheckpoints = racePlan.count( _.stopType == "CHECKPOINT")

    // Total race time between morning start time and night stop time during
    // during the challenge
    val dayTime = driveTime + numCheckpoints * controlStopLength
    val averageSpeed = distances.last / dayTime * 3.6

    if (isLast) {
      println("----------------------------------------------------------------")
      println(s"Solar vehicle commenced race at: $formattedStart.")
      println(s"Solar vehicle completed race at: $formattedFinish.")
      println(s"Race duration: $days d $hrs h $mins m $secs s.")
      println(s"Final battery SOC: $finalBatt.")

      println(s"Peak solar input: $peakSolar Watts.")
      println(s"Peak power usage: $peakMotor Watts.")
      println(s"Average speed: $averageSpeed km/h.")

      if (minBatt == 0.0) println("WARNING: Battery SOC fell to 0% during the" +
        " race. Cannot complete WSC at this speed, slow down or increase the" +
        " battery pack size.")
      else println(s"Lowest battery capacity reached: $minBatt%. How much " +
        s"faster can we drive and stay above 0.0% ?\n\n")
    }

    if (bw != null) {
      val speed = speeds.head
      val timeLength = times.head
      val maxBatt = soc.max

      val output =s"$speed, $finalBatt, $timeLength, $minBatt, $maxBatt, $peakMotor, $peakSolar\n"
      bw.write(output)
    }

    new CodeTiming(
      t1 = codeTimingStart,
      t2 = codeTimingFinish,
      courseRules = timerRaceCourse,
      planRace = timerPlanRace2 - timerPlanRace1,
      geospatial = timerGeospatial2 - timerGeospatial1,
      sunPosition = timerSunPosition2 - timerSunPosition1,
      solarPower = timerSolar2- timerSolar1,
      motorPower = timerMotor2 - timerMotor1,
      batteryPower = timerBattery2 - timerBattery1)
  }
}
