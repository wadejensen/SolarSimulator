package au.com.wadejensen.solarcar

import au.com.wadejensen.solarcar.model.{Pin, Poi}

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
    val lengths = (locations1, locations2).zipped.map(pinHaversine)

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
  private def haversine(lat1: Double,
                        lon1: Double,
                        lat2: Double,
                        lon2: Double) = {
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

    val dx: Array[Double] = (locations1, locations2).zipped.map(pinHaversine)

    (alt2, alt1, dx).zipped.map(gradient)
  }

  /**
    *
    * @param y2
    * @param y1
    * @param dx
    * @return
    */
  private def gradient(y2: Double, y1: Double, dx: Double): Double = {
    val dy = y2 - y1
    math.atan2(dy, dx)
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
  private def bearing(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val dLon = lon2 - lon1

    val bearing = math.atan2(
      math.sin(dLon) * math.cos(lat2),
      math.cos(lat1) * math.sin(lat2) - math.sin(lat1) * math.cos(lat2) * math
        .cos(dLon)
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

    val targetLats = checkpoints.map(pin => pin.lat)
    val targetLons = checkpoints.map(pin => pin.long)

    val latitudes = gpsRoute.map(pin => pin.lat)
    val longitudes = gpsRoute.map(pin => pin.long)

    val checkpointNearestIndices =
      (targetLats, targetLons).zipped.map(
        (lat, lon) => fuzzyGpsBinarySearch(lat, lon, latitudes, longitudes)
      )
    val checkpointDistances =
      checkpointNearestIndices.map(i => distances(i)).toList
    checkpointDistances
  }

  def findDistanceFromStart(lat: Double,
                            lon: Double,
                            gpsRoute: Array[Pin],
                            distances: Array[Double]): Double = {

    val latitudes = gpsRoute.map(pin => pin.lat)
    val longitudes = gpsRoute.map(pin => pin.long)

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
  private def fuzzyGpsBinarySearch(lat: Double,
                           lon: Double,
                           lats: Array[Double],
                           lons: Array[Double],
                           tol: Int = 300): Int = {

    // Approximate the index of the nearest GPS marker in list to desired
    // just by comparing latitude
    val i = reverseBinarySearch(0, lats.length - 2, lat, lats)

    // Create a window of indices near the best guess, calculate each distance
    val lower = max(i - tol, 0)
    val upper = min(i + tol, lats.length - 2)

    val windowIndices: Array[Int] = Array.range(lower, upper)
    val distances: Array[Double] = windowIndices.map(
      j => haversine(lat, lon, lats(j), lons(j))
    )

    // Find the closest result within the window to the target
    val (minValue, minIndex) = distances.zipWithIndex.min

    val nearestIndex = min(minIndex + lower, lats.length - 1)
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
  private def reverseBinarySearch(start: Int = 0,
                          end: Int,
                          target: Double,
                          list: Array[Double]): Int = {

    // midpoint with overflow protection
    val mid = start + (end - start + 1) / 2
    // if mid is the same as start or finish then we have found a neighbour
    if (mid == start || mid == end) mid
    else if (target < list(mid)) reverseBinarySearch(mid, end, target, list)
    else if (target > list(mid)) reverseBinarySearch(start, mid, target, list)
    else if (target == list(mid)) mid
    else -1 // You have NaNs in your list and therefore have bigger problems
  }

  def distance2Gps(
      distances: Array[Double],
      lookupDistances: Array[Double],
      gpsRoute: Array[Pin]): (Array[Double], Array[Double], Array[Double]) = {

    val numDist = distances.length
    val lookupSize = lookupDistances.length

    val nearestIndices: Array[Int] =
      if (numDist < lookupSize) lookupNearests(distances, lookupDistances)
      else {
        // We need to compress the distance to be less granular
        val sampleRate = ceil(numDist / lookupSize).toInt * 2
        val sampledDistances =
          for (i <- 0 until numDist by sampleRate) yield distances(i)

        val compressedIndices =
          lookupNearests(sampledDistances.toArray, lookupDistances)

        val decompressedIndices =
          for (i <- 0 until numDist by sampleRate) yield {
            val batchSize = min(sampleRate, numDist - i)
            val batchValue =
              min(compressedIndices(i / sampleRate), lookupSize - 1)
            Array.fill(batchSize)(batchValue)
          }
        decompressedIndices.flatten.toArray
      }

    val lats: Array[Double] = gpsRoute.map(_.lat)
    val lons: Array[Double] = gpsRoute.map(_.long)
    val alts: Array[Double] = gpsRoute.map(_.alt)

    val lat_bar = nearestIndices.map(i => lats(i))
    val lon_bar = nearestIndices.map(i => lons(i))
    val alt_bar = nearestIndices.map(i => alts(i))

    (lat_bar, lon_bar, alt_bar)
  }

  /**
    * Takes an array of values x for which to find the nearest value in the
    * lookup array y. Works only when x.length > y.length
    * @param x
    * @param y Lookup array to search for nearest matches
    * @return An array of indices in y pointing to nearest value in x
    */
  private def lookupNearests(x: Array[Double], y: Array[Double]): Array[Int] = {
    implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

    val m: Int = x.length
    val n: Int = y.length

    if (m > n)
      throw new Exception("Search array must be smaller than lookup array.")

    // Prebuild arrays of indices for x, y and concat(x,y)
    val ix = Array.range(0, m)
    val iy = Array.range(0, n)
    val ixy = Array.range(0, m + n)

    val xy: Array[Double] = x ++ y
    // Array of indexes the same length as xy

    // The indexes into array xy, sorted by the value of their corresponding
    // array element
    val p: Array[Int] = ixy.sortWith(xy(_) < xy(_))

    var q: Array[Int] = ixy.clone
    for (i <- xy.indices ) q(p(i)) = i

    // q contains the indices of corresponding values in xy, if xy were to be
    // sorted. Eg. If q(10) is 15, then xy(10) is the 15th lowest number in xy

    // Continuing from here defies explanation in my brain
    // We have t2 = cumulative sum of whether p values are greater than m with
    // implicit Boolean => Int conversion
    val t = p.map { _ > m }.scanLeft(0)(_ + _).tail

    val r = iy.clone
    for (i <- 0 until n) r(t(q(i + m))) = r(i)

    val s = ix.map(i => t(q(i)))

    val id = ix.map(i => r(max(s(i), 0)))
    val iu = ix.map(i => r(min(s(i) + 1, n - 1)))

    val ix_bar = ix.map(i => abs(x(i) - y(id(i))))
    val iy_bar = ix.map(i => abs(y(iu(i)) - x(i)))

    val it = (ix_bar, iy_bar).zipped.map((x, y) => round(min(x, y)).toInt)
    val ib = (it, id, iu).zipped.map((t, d, u) => d + t * (u - d))
    ib
  }
}
