package SolarCarSimulator.StrategyEngine

/**
  * Created by WadeJensen on 11/09/2017.
  */

import SolarCarSimulator.Geography.{GeoMath, Location, Pin, Poi}
import org.joda.time.DateTime
import org.rogach.scallop._
import purecsv.unsafe._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  // Fancy reflection is used to infer option name from variable name

  val checkpointFile = opt[String](required = true)
  val routeFile = opt[String](required = true)

  verify()


}
object StrategyEngine extends App {
  val conf = new Conf(args) // Note: This line also works for "object Main extends App"
  //println("routeFile is: " + conf.routeFile())



  // Race rules

  // Start date in seconds from epoch
  val raceStartDate = new DateTime(2017, 10, 12, 0, 0, 0).getMillis / 1000
  // Race start time (seconds after midnight)
  val raceStartTime = (8 * 60 + 30) * 60
  // Time the race starts and finishes each day
  val morningStartTime = 8 * 60 * 60 // seconds after midnight
  val eveningStopTime = 17 * 60 * 60 // seconds after midnight
  val nightStopDuration = 24 * 60 * 60 + morningStartTime - eveningStopTime

  val initialBattery = 1 // Fraction of total capacity
  val initialLocation = Pin(-12.46284, 130.84179, 10.555)  // Darwin


  // Race route
  // Read latitude, longitude and altitude data from csv file
  val gpsRoute: Array[Pin] =
    CSVReader[Pin].readCSVFromFileName(conf.routeFile()).toArray

  val distanceRoute = GeoMath.cumulativeDistance(gpsRoute)


//  lat1 = pi/180*lat1;
//  lon1 = pi/180*lon1;
//  lat2 = pi/180*lat2;
//  lon2 = pi/180*lon2;
//
//  %dLat = lat2-lat1; %phi
//    dLon = lon2-lon1; %lambda
//    dAlt = diff(alt);
//
//  gradient = atan2d(dAlt,1000*dist); % multiply dist by 1000 to convert km to m
//    bearing = atan2d( ( sin(dLon).*cos(lat2) ),( cos(lat1).*sin(lat2) - sin(lat1).*cos(lat2).*cos(dLon) ));

  //new Location( _.lat, 0.0, 0.0, 0.0, 0.0, 0.0)
  //)

  // Mandatory control stop locations
  val checkpoints: Array[Poi] =
    CSVReader[Poi].readCSVFromFileName(conf.checkpointFile()).toArray


//  val metresFromFinish =
//
//
//
//  val diffDistances = (loc1, loc2).zipped.map((loc1, loc2) =>
//    GeoMath.haversine(loc1.lat, loc1.long, loc2.lat, loc2.long))
//  //route.slice(1,route.length)


//  val controlStops = Array(
//    Location(-12.46284, 130.84179, 0),    // Darwin
//    Location(-14.464216, 132.261955, 0),  // Katherine
//    Location(-16.679497, 133.411811, 0),  // Dunmarra
//    Location(-19.657682, 134.188508, 0),  // Tennant Creek
//    Location(-21.531326, 133.887739, 0),  // Barrow Creek
//    Location(-23.70861, 133.8756, 0),     // Alice Springs
//    Location(-25.83911, 133.315722, 0),   // Kulgera
//    Location(-29.011056, 134.75467, 0),   // Coober Pedy
//    Location(-30.9698611, 135.74900, 0),  // Glendambo
//    Location(-32.5091944, 137.796722, 0), // Port Augusta
//    Location(-34.724838, 138.579624, 0)   // EOT
//  )

  val speeds = Array[Double](60, 60, 60, 60, 60, 60, 60, 60, 60)

  checkpoints.foreach( println(_) )
  //val plan = new RacePlan(controlStopGps, speeds)






  //
//  if (speeds.length != controlStopGps.length - 2 )
//    throw new Exception("Incorrect number of race leg speeds for given control stops.")


//  val initialTime = new Calendar.set(year + 1900, month, date, hrs, min, sec)
//  val distanceRemaining = diffDistances.scanLeft(3005.0)( (x,y) => x-y )
//  //val distances = route.sliding(2).sum
//
//  val end = distanceRemaining(distanceRemaining.length-1)

//  println(plan)
//  println(plan.speeds(0))
}
// https://stackoverflow.com/questions/32055496/how-to-read-csv-file-into-an-array-of-arrays-in-scala
// Read the route csv file into 3 arrays of doubles: lat, long and altitude
//  val route: Array[Array[Double]] = io.Source.fromFile(conf.routeFile())
//                      .getLines()
//                      .map( _.split(",").map(_.trim.toDouble) )
//                      .to[Location]()  //.toArray
