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
    val locations1: Array[Pin] = gpsRoute.slice(0, gpsRoute.length - 2)
    val locations2: Array[Pin] = gpsRoute.slice(1, gpsRoute.length - 1)

    // Calculate distance between each consecutive pin
    val lengths = (locations1, locations2).zipped.map( pinHaversine )

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
    * @return Great circle distance between two GPS coordinates in km
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
    val locations1: Array[Pin] = gpsRoute.slice(0, gpsRoute.length - 2)
    val locations2: Array[Pin] = gpsRoute.slice(1, gpsRoute.length - 1)

    val altitudes: Array[Double] = gpsRoute.map(_.alt)

    val alt1: Array[Double] = altitudes.slice(0, altitudes.length - 2)
    val alt2: Array[Double] = altitudes.slice(1, altitudes.length - 1)

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
    math.atan2( dy, dx)
  }

  /**
    * Calculates an estimated bearing for each pin on the route based on the
    * position of the following route marker pin
    * @param gpsRoute
    * @return Route bearing (radians)
    */
  def findBearings(gpsRoute: Array[Pin]): Array[Double] = {
    val locations1: Array[Pin] = gpsRoute.slice(0, gpsRoute.length - 2)
    val locations2: Array[Pin] = gpsRoute.slice(1, gpsRoute.length - 1)

    val latitudes: Array[Double] = gpsRoute.map(_.lat.toRadians)
    val longitudes: Array[Double] = gpsRoute.map(_.long.toRadians)

    val lat1: Array[Double] = latitudes.slice(0, latitudes.length - 2)
    val lat2: Array[Double] = latitudes.slice(1, latitudes.length - 1)

    val lon1: Array[Double] = longitudes.slice(0, longitudes.length - 2)
    val lon2: Array[Double] = longitudes.slice(1, longitudes.length - 1)

    val bearings: Array[Double] = for (i <- lat1.indices.toArray) yield {
      bearing(lat1(i), lon1(i), lat2(i), lon2(i))
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
    val dLon = lon2 - lon1

    val bearing = math.atan2(
      math.sin(dLon) * math.cos(lat2),
      math.cos(lat1) * math.sin(lat2) - math.sin(lat1) * math.cos(lat2) * math.cos(dLon)
    )
    bearing
  }

  /**
    * Find closest map marker to a checkpoint and use the marker's distance
    * from the starting line to approximate checkpoint distance from start
    * @param checkpoints
    * @param gpsRoute
    * @param distances
    * @return Distances of each checkpoint from starting line (meters)
    */
  def findCheckpointDistances(checkpoints: Array[Poi],
                              gpsRoute: Array[Pin],
                              distances: Array[Double]): List[Double] = {

    val targetLats = checkpoints.map( pin => pin.lat)
    val targetLons = checkpoints.map( pin => pin.long)

    val latitudes = gpsRoute.map( pin => pin.lat)
    val longitudes = gpsRoute.map( pin => pin.long)

    val checkpointNearestIndices =
      (targetLats, targetLons).zipped.map(
        (lat, lon) => fuzzyGpsBinarySearch(lat, lon, latitudes, longitudes)
      )
    val checkpointDistances = checkpointNearestIndices.map( i => distances(i) )
                                                      .toList
    checkpointDistances
  }

  def findDistanceFromStart( lat: Double,
                             lon: Double,
                             gpsRoute: Array[Pin],
                             distances: Array[Double]): Double = {

    val latitudes = gpsRoute.map( pin => pin.lat)
    val longitudes = gpsRoute.map( pin => pin.long)

    // Index of distance from start
    val i = fuzzyGpsBinarySearch(lat, lon, latitudes, longitudes)
    val distanceFromStart = distances(i)
    distanceFromStart
  }

  /**
    * A fuzzy search method based on binary search which takes advantage of the
    * fact that the GPS route is largely north to south. This means we can
    * assume it is ordered almost perfectly. First we find the closest latitude
    * coordinate in the list to the desired lat. Then we broaden the search by
    * a magic number (default =/- 10) and use haversine to find the closest point
    * within that window to our desired.
    * @param lat Latitude of the point we with to match to the route
    * @param lon Longitude of the point we wish to match to the route
    * @param lats Route latitudes of map markers in race order
    * @param lons Route longitudes of map markers in race order
    * @param tol The number of markers to test either side the closest latitude
    *            to find overall closest GPS marker should be high enough the
    *            span a distance of 5km either side to be safe
    * @return
    */
  def fuzzyGpsBinarySearch(lat: Double,
                           lon: Double,
                           lats: Array[Double],
                           lons: Array[Double],
                           tol: Int = 300): Int = {

    // Approximate the index of the nearest GPS marker in list to desired
    // just by comparing latitude
    val i = reverseBinarySearch(0, lats.length-2, lat, lats)

    // Create a window of indices near the best guess, calculate each distance
    val lower = max(i - tol, 0)
    val upper = min(i + tol, lats.length-2 )

    val windowIndices: Array[Int] = Array.range( lower, upper )
    val distances: Array[Double] = windowIndices.map(
      j => haversine( lat, lon, lats(j), lons(j) )
    )

    // Find the closest result within the window to the target
    val (minValue, minIndex) = distances.zipWithIndex.min

    val nearestIndex = min(minIndex + lower, lats.length-1)
    nearestIndex
  }

  /**
    * A functional binary tree search for floating point numbers which finds
    * a neighbour to the search target in the given list of doubles which
    * are sorted in descending order
    * @param start Lower index of the array to search
    * @param end Upper index of the array to search
    * @param target Value being searched for in list
    * @param list List of values to search sorted in *descending order*
    * @return Index of a neighbour to the target within list
    *         (not guaranteed to be nearest neighbour)
    */
  def reverseBinarySearch(start: Int = 0,
                   end: Int,
                   target:Double,
                   list: Array[Double]): Int = {

    // midpoint with overflow protection
    val mid = start + (end-start+1)/2
    // if mid is the same as start or finish then we have found a neighbour
    if ( mid == start || mid == end) mid
    else if (target < list(mid) ) reverseBinarySearch(mid, end, target, list)
    else if (target > list(mid) ) reverseBinarySearch(start, mid, target, list)
    else if (target == list(mid) ) mid
    else -1 // You have NaNs in your list and therefore have bigger problems
  }
}

