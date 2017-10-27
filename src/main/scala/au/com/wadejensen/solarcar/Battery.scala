package au.com.wadejensen.solarcar

object Battery {

  private val electricalEff = 0.97

  private val numSeriesCells = 37
  private val numParallelStrings = 11
  private val nominalCellCapacity = 3.2
  private val nominalCellVoltage = 3.6

  private val wattHoursPerCell = nominalCellCapacity * nominalCellVoltage
  private val numCells = numSeriesCells * numParallelStrings

  private val capacityWh = wattHoursPerCell * numCells
  private val capacityJoules = capacityWh * 3600

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
