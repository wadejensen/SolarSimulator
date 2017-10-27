package au.com.wadejensen.solarcar

import scala.tools.nsc.interactive.Pickler.~

object EV {

  // Drag coefficient (dimensionless)
  val Cd = 0.12
  // Cross-sectional area ( m^2 )
  val A = 0.91
  // car mass including driver (kg)
  val mass = 310
  // Tyre rolling resistance
  val Crr = 0.008
  // Air density kg/m^3
  val airDensity = 1.225
  // gravity
  val g = 9.81

  def findMotorPower(speeds: Array[Double]): Array[Double] = {

    val powerAero = speeds.map( s => 0.5 * A * Cd * airDensity *  s*s*s )
    val powerRollingResistance = speeds.map( s => Crr * g * mass * s )

    (powerAero, powerRollingResistance).zipped.map(_+_)
  }

  def findPotentialEnergy(alts: Array[Double]): Array[Double] = {
    val altsOffset = alts.slice(1,alts.length-1)

    val deltaY =
      (alts, altsOffset).zipped.map( (alt, altOffset) => altOffset - alt)
    deltaY.map( _ * mass * g)
  }
}
