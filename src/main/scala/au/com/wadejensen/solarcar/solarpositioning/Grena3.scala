package au.com.wadejensen.solarcar.solarpositioning

import java.util.Calendar
import java.util.GregorianCalendar
import java.lang.Math._


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
  def calculateSolarZenith(date: GregorianCalendar,
                           latitude: Double,
                           longitude: Double,
                           deltaT: Double): Double = {
    calculateSolarZenith(
      date,
      latitude,
      longitude,
      deltaT,
      Double.MinValue,
      Double.MinValue)
  }


  def calculateSolarZenith(date: GregorianCalendar,
                           latitude: Double,
                           longitude: Double,
                           deltaT: Double,
                           pressure: Double,
                           temperature: Double): Double = {
    val t = calcT(date)
    val tE = t + 1.1574e-5 * deltaT
    val omegaAtE = 0.0172019715 * tE
    val lambda = -1.388803 + 1.720279216e-2 * tE + 3.3366e-2 * sin(omegaAtE - 0.06172) + 3.53e-4 * sin(2.0 * omegaAtE - 0.1163)
    val epsilon = 4.089567e-1 - 6.19e-9 * tE
    val sLambda = sin(lambda)
    val cLambda = cos(lambda)
    val sEpsilon = sin(epsilon)
    val cEpsilon = sqrt(1 - sEpsilon * sEpsilon)
    var alpha = atan2(sLambda * cEpsilon, cLambda)
    if (alpha < 0) alpha += 2 * PI
    val delta = asin(sLambda * sEpsilon)
    var H = 1.7528311 + 6.300388099 * t + toRadians(longitude) - alpha
    H = ((H + PI) % (2 * PI)) - PI
    if (H < -PI) H += 2 * PI
    val sPhi = sin(toRadians(latitude))
    val cPhi = sqrt(1 - sPhi * sPhi)
    val sDelta = sin(delta)
    val cDelta = sqrt(1 - sDelta * sDelta)
    val cH = cos(H)
    val sEpsilon0 = sPhi * sDelta + cPhi * cDelta * cH
    val eP = asin(sEpsilon0) - 4.26e-5 * sqrt(1.0 - sEpsilon0 * sEpsilon0)
    val deltaRe = if (temperature < -(273) || temperature > 273 || pressure < 0 || pressure > 3000) 0.0
    else if ((eP > 0.0)) {
      (0.08422 * (pressure / 1000)) / ((273.0 + temperature) * tan(eP + 0.003138 / (eP + 0.08919)))
    }
    else {
      0.0
    }
    val z = PI / 2 - eP - deltaRe
    toDegrees(z)
  }

  private def calcT(date: GregorianCalendar) = {
    val utc = JulianDate.createUtcCalendar(date)
    var m = utc.get(Calendar.MONTH) + 1
    var y = utc.get(Calendar.YEAR)
    val d = utc.get(Calendar.DAY_OF_MONTH)
    val h = utc.get(Calendar.HOUR_OF_DAY) + utc.get(Calendar.MINUTE) / 60d + utc.get(Calendar.SECOND) / (60d * 60)
    if (m <= 2) {
      m += 12
      y -= 1
    }
    (365.25 * (y - 2000)).toInt + (30.6001 * (m + 1)).toInt - (0.01 * y).toInt + d + 0.0416667 * h - 21958
  }
}

final class Grena3 private() {
}

