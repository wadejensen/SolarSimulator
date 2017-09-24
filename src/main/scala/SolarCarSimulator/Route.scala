package SolarCarSimulator

import java.net.URI

import scala.io.Source

/**
  * Created by WadeJensen on 11/09/2017.
  */
case class Location2(lat: Array[Double],
                     long: Array[Double],
                     alt: Array[Double])

object Route {

  def loadRouteFromCsv(routeFile: URI): Unit = {
    // https://stackoverflow.com/questions/32055496/how-to-read-csv-file-into-an-array-of-arrays-in-scala
    val route = io.Source
      .fromFile(routeFile)
      .getLines()
      .map(_.split(",").map(_.trim.toDouble))
      .toArray

    Location2(route(0), route(1), route(2))
  }

}
