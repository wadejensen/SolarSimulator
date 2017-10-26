package au.com.wadejensen.solarcar

object PV {

  // Maximum angle the array can tilt by
  val maxTilt = 50.0
  val efficiencyPeak = 0.225
  val arrayEff = efficiencyPeak * 0.9
  val area = 6
  val tiltAccuracy = 4

  /**
    * Tilt the solar array towards the sun when the car is not moving.
    * @param zeniths The zenith angles of the sun (degrees)
    * @param speeds The velocity of the solar vehicle (m/s)
    * @return An array of angles between the solar array and the solar normal.
    *         the optimal angle is 0 degrees. We do not expect to normalise the
    *         array with the sun perfectly. Hence the bext we can achieve is
    *         tiltAccuracy.
    */
  def tiltArray( zeniths: Array[Double],
                 speeds: Array[Double]): Array[Double] = {

    (zeniths, speeds).zipped.map( (z,s) =>
      if ( s != 0 ) z
      else {
        if (z < maxTilt) tiltAccuracy
        else z - maxTilt
      }
    )
  }

//  function tilt(Array, vBlockTimeIDX,zenith)
//  %% Retrieve data from other objects
//    Array.sun2Arr = zenith;
//
//  Array.sun2Arr( (vBlockTimeIDX==0) & (zenith < Array.maxTilt) ) = Array.tiltAccuracy;
//  % when it is possible to tilt the array perfectly, tilt it
//    % almost perfectly
//
//  indexQuery = (vBlockTimeIDX==0) & (zenith >= Array.maxTilt);
//  Array.sun2Arr( indexQuery ) = ...
//  Array.sun2Arr (indexQuery)- Array.maxTilt ;
//  % when it is not possible to tilt perfectly, tilt to the
//  % maximum angle
//
//  end
//  function findOptimalTilt(Array)
//  zenith = getZenith();
//  Array.optimalTilt = zenith;
//  Array.optimalTilt(Array.optimalTilt > Array.maxTilt)...
//  = Array.maxTilt;
//  end
//



}
