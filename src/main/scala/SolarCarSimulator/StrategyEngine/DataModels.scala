package SolarCarSimulator.StrategyEngine

import SolarCarSimulator.StrategyEngine.geography.Pin
import SolarCarSimulator.RaceLeg

/**
  * Created by WadeJensen on 22/10/2017.
  */
case class RaceCourse(
    val gpsRoute: Array[Pin],
    val pinDistances: Array[Double],
    val bearings: Array[Double],
    val gradients: Array[Double],
    val checkpointDistances: List[Double]
)
