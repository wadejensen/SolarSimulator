package au.com.wadejensen.solarcar.solarpositioning

import java.time.{Instant, ZoneId, ZonedDateTime}

//import java.lang.Math._

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.ops.transforms.Transforms._
import org.nd4s.Implicits._



/**
  * Calculate topocentric solar position, i.e. the location of the sun on the sky for a certain point in time on a
  * certain point of the Earth's surface.
  *
  * This follows the no. 3 algorithm described in Grena, 'Five new algorithms for the computation of sun position
  * from 2010 to 2110', Solar Energy 86 (2012) pp. 1323-1337.
  *
  * This is <i>not</i> a port of the C code, but a re-implementation based on the published procedure.
  *
  * @author Klaus Brunner
  */
object Grena3 {
  /**
    * Calculate solar zenith position, i.e. the elevation of the sun on the sky for a certain point in time on a
    * certain point of the Earth's surface.
    *
    * This follows the no. 3 algorithm described in Grena, 'Five new algorithms for the computation of sun position
    * from 2010 to 2110', Solar Energy 86 (2012) pp. 1323-1337.
    *
    * The algorithm is supposed to work for the years 2010 to 2110, with a maximum error of 0.01 degrees.
    *
    * This method does not perform refraction correction.
    *
    * @param date      Observer's local date and time.
    * @param latitude  Observer's latitude, in degrees (negative south of equator).
    * @param longitude Observer's longitude, in degrees (negative west of Greenwich).
    * @param deltaT    Difference between earth rotation time and terrestrial time (or Universal Time and Terrestrial Time),
    *                  in seconds. See
    *                  <a href ="http://asa.usno.navy.mil/SecK/DeltaT.html">http://asa.usno.navy.mil/SecK/DeltaT.html</a>.
    *                  For the year 2015, a reasonably accurate default would be 68.
    * @return Topocentric solar position (azimuth measured eastward from north)
    * @see AzimuthZenithAngle
    */
  def calculateSolarZenith(t: Long,
                           latitude: Double,
                           longitude: Double,
                           deltaT: Double): Double = {
    calculateSolarZenith(
      t,
      latitude,
      longitude,
      deltaT,
      Double.MinValue,
      Double.MinValue)
  }


  def calculateSolarZenith(t: Long,
                           latitude: Double,
                           longitude: Double,
                           deltaT: Double,
                           pressure: Double,
                           temperature: Double): Double = {
    val t2 = calcT(t)
    val tE = t2 + 1.1574e-5 * deltaT
    val omegaAtE = 0.0172019715 * tE
    val lambda = -1.388803 + 1.720279216e-2 * tE + 3.3366e-2 * math.sin(omegaAtE - 0.06172) + 3.53e-4 * math.sin(2.0 * omegaAtE - 0.1163)
    val epsilon = 4.089567e-1 - 6.19e-9 * tE
    val sLambda = math.sin(lambda)
    val cLambda = math.cos(lambda)
    val sEpsilon = math.sin(epsilon)
    val cEpsilon = math.sqrt(1 - sEpsilon * sEpsilon)
    var alpha = math.atan2(sLambda * cEpsilon, cLambda)
    if (alpha < 0) alpha += 2 * math.Pi
    val delta = math.asin(sLambda * sEpsilon)
    var H = 1.7528311 + 6.300388099 * t2 + math.toRadians(longitude) - alpha
    H = ((H + math.Pi) % (2 * math.Pi)) - math.Pi
    if (H < -math.Pi) H += 2 * math.Pi
    val sPhi = math.sin(math.toRadians(latitude))
    val cPhi = math.sqrt(1 - sPhi * sPhi)
    val sDelta = math.sin(delta)
    val cDelta = math.sqrt(1 - sDelta * sDelta)
    val cH = math.cos(H)
    val sEpsilon0 = sPhi * sDelta + cPhi * cDelta * cH
    val eP = math.asin(sEpsilon0) - 4.26e-5 * math.sqrt(1.0 - sEpsilon0 * sEpsilon0)
    val deltaRe = if (temperature < -(273) || temperature > 273 || pressure < 0 || pressure > 3000) 0.0
    else if ((eP > 0.0)) {
      (0.08422 * (pressure / 1000)) / ((273.0 + temperature) * math.tan(eP + 0.003138 / (eP + 0.08919)))
    }
    else {
      0.0
    }
    val z = math.Pi / 2 - eP - deltaRe
    math.toDegrees(z)
  }

  /**
    *
    * @param t unix timestamp, this version of calcT is only valid after unix
    *          epoch
    * @return
    */
  private def calcT(t: Long) = {
    val utc = ZonedDateTime.ofInstant(Instant.ofEpochSecond(t), ZoneId.of("Z"))
    var m = utc.getMonthValue
    var y = utc.getYear
    val d = utc.getDayOfMonth
    val h = utc.getHour + utc.getMinute / 60d + utc.getSecond / (60d * 60)
    if (m <= 2) {
      m += 12
      y -= 1
    }
    (365.25 * (y - 2000)).toInt + (30.6001 * (m + 1)).toInt - (0.01 * y).toInt + d + 0.0416667 * h - 21958
  }

  def calculateSolarZenithVectorised(
                           t: Array[Long],
                           latitude: INDArray,
                           longitude: INDArray,
                           deltaT: Double,
                           pressure: Double,
                           temperature: Double): Array[Double] = {

    val t2 = t.map(calcT(_)).toNDArray
    val tE = t2 + 1.1574e-5 * deltaT
    val omegaAtE = tE * 0.0172019715
    //val lambda = -1.388803 + 1.720279216e-2 * tE + 3.3366e-2 * math.sin(omegaAtE - 0.06172) + 3.53e-4 * math.sin(2.0 * omegaAtE - 0.1163)
    val lambda = tE * 1.720279216e-2 - 1.388803 + sin(omegaAtE - 0.06172) * 3.3366e-2 + sin(omegaAtE * 2.0 - 0.1163) * 3.53e-4
    val epsilon = - tE * 6.19e-9 + 4.089567e-1
    val sLambda = sin(lambda)
    val cLambda = cos(lambda)
    val sEpsilon = sin(epsilon)
    val cEpsilon = sqrt(-(sEpsilon * sEpsilon) + 1)
    var alpha = atan2(cLambda, sLambda * cEpsilon)

//    var alpha = math.atan2(sLambda * cEpsilon, cLambda)
//    if (alpha < 0) alpha += 2 * math.Pi

    // The following two lines are equivalent to:
    // if (alpha < 0) alpha += 2 * math.Pi
    val temp = alpha.lt(0.0) * (2 * math.Pi)
    alpha += temp

    val delta = asin(sLambda * sEpsilon)
    var H = t2 * 6.300388099 + 1.7528311 + longitude * math.Pi / 180 - alpha
        H = (H + math.Pi).fmod(2 * math.Pi) - math.Pi
        // The following two lines are equivalent to:
        // if (H < -math.Pi) H += 2 * math.Pi
        val temp2 = H.lt(-math.Pi) * (2 * math.Pi)
        H += temp2

    val sPhi = sin(latitude * math.Pi / 180)
    val cPhi = sqrt(-sPhi * sPhi + 1)
    val sDelta = sin(delta)
    val cDelta = sqrt(-sDelta * sDelta + 1.0)
    val cH = cos(H)
    val sEpsilon0 = sPhi * sDelta + cPhi * cDelta * cH
    val eP = asin(sEpsilon0) - sqrt(-sEpsilon0 * sEpsilon0 + 1) * 4.26e-5

    // The original calculation for deltaRe:
    //    val deltaRe =
    //      if (temperature < -(273) || temperature > 273 || pressure < 0 || pressure > 3000) 0.0
    //      else if ((eP > 0.0)) {
    //        (0.08422 * (pressure / 1000)) / ((273.0 + temperature) * tan(eP + (eP + 0.08919) \ 4.26e-5))
    //      }
    //      else {
    //        0.0
    //      }

    // But we always use a constant known temperature and pressure as an
    // approximation. So the expression simplifies to:
    val temp3 =
    sin(eP + (eP + 0.08919) \ 4.26e-5) /
      cos(eP + (eP + 0.08919) \ 4.26e-5) *
      (0.08422 * (pressure / 1000)) / (273.0 + temperature)

    val deltaRe = eP.gt(0.0) * temp3

    val z = - eP - deltaRe + (math.Pi / 2)
    val zenith = z * (180 / math.Pi)
    (for ( i <- 0 until zenith.length ) yield {zenith.getDouble(i)}).toArray
  }
}

final class Grena3 private() {
}

