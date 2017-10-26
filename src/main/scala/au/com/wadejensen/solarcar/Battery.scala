package au.com.wadejensen.solarcar

object Battery {

  val electricalEff = 0.97

  val numSeriesCells = 37
  val numParallelStrings = 11
  val nominalCellCapacity = 3.2
  val nominalCellVoltage = 3.6

  val wattHoursPerCell = nominalCellCapacity * nominalCellVoltage
  val numCells = numSeriesCells * numParallelStrings

  val capacityWh = wattHoursPerCell * numCells
  val capacityJoules = capacityWh * 3600

  def findNetPower(motorPower: Array[Double],
               solarPower: Array[Double]): Array[Double] = {
    (motorPower, solarPower).zipped.map( (m,sol) => sol - m/electricalEff)
  }

  def findNetEnergy(netPower: Array[Double],
                potentialEnergy: Array[Double]): Array[Double] = {
    // Change in energy
    (netPower, potentialEnergy).zipped.map( (p,PE) => p + PE*electricalEff )
  }

  def findStateOfCharge( netEnergy: Array[Double] ): Array[Double] = {
    // SOC = State of charge
    val socJoules = netEnergy.scanLeft(capacityJoules)( (soc, netEnergy ) => {
      val newTotalEnergy = soc + netEnergy
      if ( newTotalEnergy > capacityJoules ) capacityJoules
      else if ( newTotalEnergy < 0) 0
      else newTotalEnergy
    }).tail

    // SOC as a percentage
    socJoules.map( _ / capacityJoules * 100)
  }


}
