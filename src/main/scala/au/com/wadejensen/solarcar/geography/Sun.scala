package au.com.wadejensen.solarcar.geography

import java.util.GregorianCalendar

import net.e175.klaus.solarpositioning._

object Sun {

  def findSunPositionsSPA(
      times: Array[Long],
      latitudes: Array[Double],
      longitudes: Array[Double],
      altitudes: Array[Double]): (Array[Double], Array[Double]) = {

    val pos =
      Array
        .range(0, times.length)
        .map(
          i =>
            calculatePositionSPA(times(i),
                                 latitudes(i),
                                 longitudes(i),
                                 altitudes(i)))

    val azimuths = pos.map(_.getAzimuth)
    val zeniths = pos.map(_.getZenithAngle)

    (azimuths, zeniths)
  }

  def findSunPositionsGrena3(
      times: Array[Long],
      latitudes: Array[Double],
      longitudes: Array[Double],
      altitudes: Array[Double]): (Array[Double], Array[Double]) = {

    val pos =
      Array
        .range(0, times.length)
        .map(
          i =>
            calculatePositionGrena3(times(i),
                                    latitudes(i),
                                    longitudes(i),
                                    altitudes(i)))

    val azimuths = pos.map(_.getAzimuth)
    val zeniths = pos.map(_.getZenithAngle)

    (azimuths, zeniths)
  }

  def calculatePositionSPA(time: Long,
                           latitude: Double,
                           longitude: Double,
                           altitude: Double): AzimuthZenithAngle = {

    var datetime = new GregorianCalendar
    datetime.setTimeInMillis(time * 1000)

    SPA.calculateSolarPosition(datetime,
                               latitude,
                               longitude,
                               altitude,
                               DeltaT.estimate(datetime),
                               1010.0,
                               30)
  }

  def calculatePositionGrena3(time: Long,
                              latitude: Double,
                              longitude: Double,
                              altitude: Double): AzimuthZenithAngle = {

    var datetime = new GregorianCalendar
    datetime.setTimeInMillis(time * 1000)

    Grena3.calculateSolarPosition(datetime,
                                  latitude,
                                  longitude,
                                  DeltaT.estimate(datetime),
                                  1010.0,
                                  30)
  }
}
