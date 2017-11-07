package au.com.wadejensen.solarcar

import java.util.GregorianCalendar

import au.com.wadejensen.solarcar.solarpositioning.{AzimuthZenithAngle, DeltaT, Grena3, SPA}
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4s.Implicits._

object Sun {
  val diffuseRadiationCoeff = 0.1
  val solarPowerExtraTerrestrial = 1367

  def findSunPositionsSPA(
      times: Array[Long],
      latitudes: Array[Double],
      longitudes: Array[Double],
      altitudes: Array[Double]): (Array[Double], Array[Double]) = {

    val pos =
      Array.range(0, times.length)
        .map( i =>
          calculatePositionSPA(times(i),
            latitudes(i),
            longitudes(i),
            altitudes(i)))

    val azimuths = pos.map(_.getAzimuth)
    val zeniths = pos.map(_.getZenithAngle)

    (azimuths, zeniths)
  }

//  def findSunPositionsGrena3(
//      times: Array[Long],
//      lats: Array[Double],
//      lons: Array[Double],
//      alts: Array[Double]): (Array[Double], Array[Double]) = {
//
//    val pos =
//      Array.range(0, times.length)
//        .map( i => calculatePositionGrena3(times(i),lats(i), lons(i), alts(i)))
//
//
//    val azimuths = pos.map(_.getAzimuth)
//    val zeniths = pos.map(_.getZenithAngle)
//
//    (azimuths, zeniths)
//  }

  def findSolarZenithGrena3Vectorised(times: Array[Long],
                      lats: Array[Double],
                      lons: Array[Double],
                      alts: Array[Double]): INDArray = {

    val gregorians =
      Array.tabulate(times.length){ i =>
        val time = new GregorianCalendar()
        time.setTimeInMillis(times(i) * 1000)
        time
      }

    // Estimate deltaT based on initial time
    val deltaT = DeltaT.estimate(gregorians(0))

    Grena3.calculateSolarZenithVectorised(
      gregorians,
      lats.toNDArray,
      lons.toNDArray,
      deltaT,
      1010.0,
      30.0
    )
  }

  def findSolarZenithGrena3(times: Array[Long],
                                      lats: Array[Double],
                                      lons: Array[Double],
                                      alts: Array[Double]): Array[Double] = {

    Array.range(0, times.length)
         .map( i => calculateZenithGrena3(times(i),lats(i), lons(i), lats(i)))
  }

  private def calculateZenithGrena3( time: Long,
                                     latitude: Double,
                                     longitude: Double,
                                     altitude: Double) = {

    var datetime = new GregorianCalendar
    datetime.setTimeInMillis(time * 1000)

    Grena3.calculateSolarZenith(
      datetime,
      latitude,
      longitude,
      DeltaT.estimate(datetime) )
  }

  private def calculatePositionSPA(time: Long,
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

//  private def calculatePositionGrena3(time: Long,
//                              latitude: Double,
//                              longitude: Double,
//                              altitude: Double): AzimuthZenithAngle = {
//
//    var datetime = new GregorianCalendar
//    datetime.setTimeInMillis(time * 1000)
//
//    Grena3.calculateSolarPosition(datetime,
//                                  latitude,
//                                  longitude,
//                                  DeltaT.estimate(datetime),
//                                  1010.0,
//                                  30)
//  }

  def findIrrandiance(zeniths: Array[Double]):
                          (Array[Double], Array[Double]) = {

    val directRadiations = zeniths.map(calculateDirectRadiance(_))
    val diffuseRadiations = directRadiations.map( _ * diffuseRadiationCoeff)

    (directRadiations, diffuseRadiations)

  }

  private def calculateDirectRadiance(zenith: Double): Double = {
    if (zenith > 90.0) 0.0
    else {
      val z = zenith*math.Pi/180
      val AM = 1 / ( math.cos(z) + 0.50572 * math.pow(96.07995 - z, -1.6364) )
      // Direct radiation
      solarPowerExtraTerrestrial * math.pow(0.7, math.pow(AM,0.678) )
    }
  }
}
