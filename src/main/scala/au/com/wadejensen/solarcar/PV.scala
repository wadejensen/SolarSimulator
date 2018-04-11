package au.com.wadejensen.solarcar

import scala.math._

object PV {

  // Maximum angle the array can tilt by
  val maxTilt = 50.0
  val efficiencyPeak = 0.225
  val arrayEff = efficiencyPeak * 0.9
  val area = 6
  val tiltAccuracy = 4

  /**
    * Tilt the solar array towards the sun when the car is not moving.
    * @param zeniths The zenith angles of the sun (degrees)
    * @param speeds The velocity of the solar vehicle (m/s)
    * @return An array of angles between the solar array and the solar normal.
    *         the optimal angle is 0 degrees. We do not expect to normalise the
    *         array with the sun perfectly. Hence the bext we can achieve is
    *         tiltAccuracy.
    */
  def tiltArray( zeniths: Array[Double],
                 speeds: Array[Double]): Array[Double] = {

    (zeniths, speeds).zipped.map( (z,s) =>
      if ( s != 0 ) z
      else {
        if (z < maxTilt) tiltAccuracy
        else z - maxTilt
      }
    )
  }

  def findArrayPower( sun2Arr: Array[Double],
                      directRadiation: Array[Double],
                      diffuseRadiation: Array[Double] ): Array[Double] = {


    (sun2Arr, directRadiation, diffuseRadiation).zipped
      .map( calculateArrayPower(_,_,_) )
  }

  private def calculateArrayPower( theta: Double,
                           directRadiation: Double,
                           diffuseRadiation: Double): Double = {

    val scaleDirRad = directRadiation * cos(theta*Pi/180)
    val irradiance = scaleDirRad + diffuseRadiation
    (directRadiation * cos(theta*Pi/180) + diffuseRadiation) * arrayEff * area
  }
}
