package SolarCarSimulator.Geography

import scala.math._


/**
  * Static object for geographic distance calculations
  */
object GeoMath {
  // https://github.com/acmeism/RosettaCodeData/blob/master/Task/Haversine-formula/Scala/haversine-formula.scala
  val R = 6367.4447 // Earth's radius in km

  def cumulativeDistance(gpsRoute: Array[Pin]): Array[Double] = {
    val locations1: Array[Pin] = gpsRoute.slice(0, gpsRoute.length - 1)
    val locations2: Array[Pin] = gpsRoute.slice(1, gpsRoute.length)

    // Calculate cumulative distance travelled for each pin from race start
    val distancesTravelled =
      (locations1, locations2).zipped
                              .map( pinHaversine )
                              .scanLeft(0.0)(_ + _)
    distancesTravelled
  }

  /**
    * A wrapper for the haversine function to allow cleaner use with Pin
    * objects in lambdas
    * @param pin1
    * @param pin2
    * @return Great circle distance between two Pins
    */
  def pinHaversine(pin1: Pin, pin2: Pin): Double = {
    haversine(pin1.lat, pin1.long, pin2.lat, pin2.long)
  }

  /**
    * Haversine computation of great circle distance
    * @param lat1 Latitude in degrees of point 1
    * @param lon1 Longitude in degrees of point 1
    * @param lat2 Latitude in degrees of point 2
    * @param lon2 Longitude in degrees of point 2
    * @return Great circle distance between two GPS coordinates
    */
  def haversine(lat1: Double, lon1: Double, lat2: Double, lon2: Double) = {
    val dLat = (lat2 - lat1).toRadians
    val dLon = (lon2 - lon1).toRadians

    val a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1.toRadians) * cos(
      lat2.toRadians)
    val c = 2 * asin(sqrt(a))
    R * c
  }

  def findGradients(gpsRoute: Array[Pin], distanceRoute: Array[Double]) = {
    val altitudes: Array[Double] = gpsRoute.map(_.alt)

    val alt1: Array[Double] = altitudes.slice(0, altitudes.length - 1)
    val alt2: Array[Double] = altitudes.slice(1, altitudes.length)

    val dy: Array[Double] =
      (locations1, locations2).zipped
                              .map( pinGradient )

  }
  
  def pinGradient(pin1: Pin, pin2: Pin): Double = {
    gradient(pin1.lat, pin1.long, pin2.lat, pin2.long)
  }

  def gradient()

  def bearing(gpsRoute: Array[Pin]): Unit = {
    bearing()

    val dLat = (lat2 - lat1).toRadians
    val dLon = (lon2 - lon1).toRadians

    bearing = atan2d( ( sin(dLon).*cos(lat2) ),( cos(lat1).*sin(lat2) - sin(lat1).*cos(lat2).*cos(dLon) ));
  }



  def bearing(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Unit = {
    val dLat = (lat2 - lat1).toRadians
    val dLon = (lon2 - lon1).toRadians


  }
}