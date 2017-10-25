package au.com.wadejensen.solarcar.geography

trait Gps {
  val lat: Double // latitude
  val long: Double // longitude
}

/**
  * Model object to represent one row of a csv file containing the WSC route.
  * @param lat Latitude coord (degrees)
  * @param long Longitude coord (degrees)
  * @param alt Altitude above sea level (meters)
  */
case class Pin(lat: Double, long: Double, alt: Double) extends Gps

/**
  * Model object to represent one row of a csv file containing a list of WSC
  * checkpoint locations as a Point of Interest.
  * @param lat lat Latitude coord (degrees)
  * @param long Longitude coord (degrees)
  * @param name Town / city name of the checkpoint location, eg. Darwin
  */
case class Poi(lat: Double, long: Double, name: String) extends Gps

/**
  * Model object which represents a single map marker along the WSC route.
  * @param lat Latitude coord (degrees)
  * @param long Longitude coord (degrees)
  * @param alt Altitude above sea level (meters)
  * @param dist Distance from starting line along the race route (meters)
  */
case class Location(lat: Double,
                    long: Double,
                    alt: Double = 0.0f,
                    dist: Double,
                    gradient: Double,
                    bearing: Double)
    extends Gps
