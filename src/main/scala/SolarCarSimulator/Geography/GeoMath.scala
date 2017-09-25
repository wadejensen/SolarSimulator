package SolarCarSimulator.Geography

import scala.math._


/**
  * Static object for geographic distance calculations
  */
object GeoMath {

  // https://github.com/acmeism/RosettaCodeData/blob/master/Task/Haversine-formula/Scala/haversine-formula.scala
  val R = 6367.4447 // Earth's radius in km
  val KM_TO_M = 1000 // Conversion rate between kilometers and meters

  /**
    * Calculates the distance travelled along the race in order to reach each
    * GPS marker on the route
    * @param gpsRoute The list of GPS locations of the race route
    * @return The distance travelled
    */
  def cumulativeDistance(gpsRoute: Array[Pin]): Array[Double] = {
    val locations1: Array[Pin] = gpsRoute.slice(0, gpsRoute.length - 1)
    val locations2: Array[Pin] = gpsRoute.slice(1, gpsRoute.length)

    // Calculate distance between each consecutive pin
    val lengths = (locations1, locations2).zipped
                                          .map( pinHaversine )

    // Cumulative sum of lengths, the distance travelled for each pin
    // from race start
    val distancesTravelled = lengths.scanLeft(0.0)(_ + _)
    distancesTravelled
  }

  /**
    * A wrapper for the haversine function to allow cleaner use with Pin
    * objects in lambdas
    * @param pin1
    * @param pin2
    * @return Great circle distance between two Pins
    */
  private def pinHaversine(pin1: Pin, pin2: Pin): Double = {
    haversine(pin1.lat, pin1.long, pin2.lat, pin2.long) * KM_TO_M
  }

  /**
    * Haversine computation of great circle distance
    * @param lat1 Latitude in degrees of point 1
    * @param lon1 Longitude in degrees of point 1
    * @param lat2 Latitude in degrees of point 2
    * @param lon2 Longitude in degrees of point 2
    * @return Great circle distance between two GPS coordinates
    */
  private def haversine(lat1: Double, lon1: Double, lat2: Double, lon2: Double) = {
    val dLat = (lat2 - lat1).toRadians
    val dLon = (lon2 - lon1).toRadians

    val a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1.toRadians) * cos(
      lat2.toRadians)
    val c = 2 * asin(sqrt(a))
    R * c
  }

  /**
    * Calaculates an estimated gradient of each pin on the route based on the
    * elevation of the following route marker pin
    * @param gpsRoute
    * @return Road gradients (radians)
    */
  def findGradients(gpsRoute: Array[Pin]): Array[Double] = {
    val locations1: Array[Pin] = gpsRoute.slice(0, gpsRoute.length - 1)
    val locations2: Array[Pin] = gpsRoute.slice(1, gpsRoute.length)

    val altitudes: Array[Double] = gpsRoute.map(_.alt)

    val alt1: Array[Double] = altitudes.slice(0, altitudes.length - 1)
    val alt2: Array[Double] = altitudes.slice(1, altitudes.length)

    val dx: Array[Double] = (locations1, locations2).zipped.map( pinHaversine )

    (alt2, alt1, dx).zipped.map(gradient)
  }

  /**
    *
    * @param y2
    * @param y1
    * @param dx
    * @return
    */
  def gradient(y2: Double, y1: Double, dx: Double): Double = {
    val dy = y2 - y1
    Math.atan2( dy, dx)
  }

  /**
    * Calculates an estimated bearing for each pin on the route based on the
    * position of the following route marker pin
    * @param gpsRoute
    * @return Route bearing (radians)
    */
  def findBearings(gpsRoute: Array[Pin]): Array[Double] = {
    val locations1: Array[Pin] = gpsRoute.slice(0, gpsRoute.length - 1)
    val locations2: Array[Pin] = gpsRoute.slice(1, gpsRoute.length)

    val latitudes: Array[Double] = gpsRoute.map(_.lat.toRadians)
    val longitudes: Array[Double] = gpsRoute.map(_.long.toRadians)

    val lat1: Array[Double] = latitudes.slice(0, latitudes.length - 1)
    val lat2: Array[Double] = latitudes.slice(1, latitudes.length)

    val lon1: Array[Double] = longitudes.slice(0, longitudes.length - 1)
    val lon2: Array[Double] = longitudes.slice(1, longitudes.length)

    val bearings = lat1 zip lon1 zip lat2 zip lon2 map {
      case (((lat1,lon1),lat2),lon2)=> bearing(lat1,lon1,lat2,lon2)
    }

    bearings
  }

  /**
    *
    * @param lat1 Latitude1 (radians)
    * @param lon1 Longitude1 (radians)
    * @param lat2 Latitude2 (radians)
    * @param lon2 Longitude2 (radians)
    * @return
    */
  def bearing(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val dLon = (lon2 - lon1)

    val bearing = Math.atan2(
      Math.sin(dLon) * Math.cos(lat2),
      Math.cos(lat1) * Math.sin(lat2) - Math.sin(lat1) * Math.cos(lat2) * Math.cos(dLon)
    )
    bearing
  }
}

