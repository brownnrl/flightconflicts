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
  checkTrajectories(trajectory1, trajectory2)
  
  if (!is.flattrajectory(trajectory1)) {
    lon0 <- mean(c(trajectory1$longitude, trajectory2$longitude))
    lat0 <- mean(c(trajectory1$latitude, trajectory2$latitude))
    
    trajectory1 <- trajectoryToXYZ(trajectory1, c(lon0, lat0))
    trajectory2 <- trajectoryToXYZ(trajectory2, c(lon0, lat0))
  }
  
  # Distance between aircraft
  dXYZ <- trajectory2$position - trajectory1$position
  dXYZ[, 3] <- abs(dXYZ[, 3])
  relativeVelocity <- trajectory2$velocity - trajectory1$velocity
  
  # Calculate time to CPA and projected HMD
  tCPADividend <- -apply(dXYZ[, 1:2, drop = FALSE] * relativeVelocity, 1, sum)
  tCPADivisor <- apply(relativeVelocity^2, 1, sum)
  tCPA <-  tCPADividend / tCPADivisor
  # Safegaurd against singularity
  tCPA[tCPADivisor == 0 | tCPADividend < 0] <- 0
  
  return(tCPA)
}
