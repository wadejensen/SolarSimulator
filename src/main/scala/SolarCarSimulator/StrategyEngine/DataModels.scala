package SolarCarSimulator.StrategyEngine

import SolarCarSimulator.Geography.Pin
import SolarCarSimulator.RaceLeg

/**
  * Created by WadeJensen on 22/10/2017.
  */

case class Race(
  val startTime: Long,
  val racePlan: List[RaceLeg],
  val gpsRoute: Array[Pin],
  val pinDistances: Array[Double],
  val bearings: Array[Double],
  val gradients: Array[Double]
)

