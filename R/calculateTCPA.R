#' Calculate the time to closest point of approach (TCPA) between two aircraft 
#' at each time point of their their trajectories.
#' 
#' @param trajectory1 A \code{flighttrajectory} object corresponding to the 
#'   first aircraft.
#' @param trajectory2 A \code{flighttrajectory} object corresponding to the 
#'   second aircraft.
#' @return The numeric vector giving the TCPA. Values in seconds.
#'   
#' @details This code is based on the SLoWC formulation by Ethan Pratt and Jacob
#'   Kay as implemented in a MATLAB script by Ethan Pratt dated 2016-04-18. This
#'   code calculates horizontal CPA only.
#'   
#' @export
calculateTCPA <- function(trajectory1, trajectory2) {
  if (!is.flighttrajectory(trajectory1) || !is.flighttrajectory(trajectory2)) {
    stop("Both arguments must be instances of flighttrajectory")
  }
  if (!isTRUE(all.equal(trajectory1$timestamp, trajectory2$timestamp))) {
    stop("Trajectories must have matching time stamps")
  }
  
  # Find the "origin" lon/lat for the encounter. Distances will be represented
  # in feet north/east from this point. Use the centroid of the trajectories.
  lon0 <- mean(c(trajectory1$longitude, trajectory2$longitude))
  lat0 <- mean(c(trajectory1$latitude, trajectory2$latitude))
  
  # Flat Earth approximation of aircraft position and velocity
  ac1XYZ <- cbind(lonlatToXY(trajectory1$longitude, trajectory1$latitude, 
                             lon0, lat0),
                  trajectory1$altitude)
  ac2XYZ <- cbind(lonlatToXY(trajectory2$longitude, trajectory2$latitude, 
                             lon0, lat0),
                  trajectory2$altitude)
  
  # Convert bearing/speed to velocity vector.
  ac1Velocity <- bearingToXY(trajectory1$bearing, trajectory1$groundspeed)
  ac2Velocity <- bearingToXY(trajectory2$bearing, trajectory2$groundspeed)
  
  # Distance between aircraft
  dXYZ <- ac2XYZ - ac1XYZ
  dXYZ[, 3] <- abs(dXYZ[, 3])
  relativeVelocity <- ac2Velocity - ac1Velocity
  
  # Calculate time to CPA and projected HMD
  tCPADividend <- -apply(dXYZ[, 1:2, drop = FALSE] * relativeVelocity, 1, sum)
  tCPADivisor <- apply(relativeVelocity^2, 1, sum)
  tCPA <-  tCPADividend / tCPADivisor
  # Safegaurd against singularity
  tCPA[tCPADivisor == 0 | tCPADividend < 0] <- 0
  
  return(tCPA)
}
