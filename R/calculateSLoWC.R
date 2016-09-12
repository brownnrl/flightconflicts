#' Calculate the "severity loss of Well Clear" (SLoWC) metric, described in RTCA
#' SC-228 Closed-Loop Metrics White Paper.
#' 
#' @param trajectory1 A \code{flighttrajectory} object corresponding to the
#'   first aircraft.
#' @param trajectory2 A \code{flighttrajectory} object corresponding to the
#'   second aircraft.
#' @return The numeric vector giving the SLoWC metric. Values lie in the range 
#'   [0, 100]. A SLoWC of 0 indicates Well Clear, while a value of 100 
#'   corresponds to "full penetration" (i.e., a collision).
#'   
#' @details Note that the RTCA definition of Well Clear is undergoing revision. 
#'   This code is based on the SLoWC formulation by Ethan Pratt and Jacob Kay as
#'   implemented in a MATLAB script by Ethan Pratt dated 2016-04-18.
#'   
#' @export
calculateSLoWC <- function(trajectory1, trajectory2) {
  checkTrajectories(trajectory1, trajectory2)
  
  # DAA Well Clear thresholds
  DMOD       <- 4000 # ft
  DH_thr     <- 450  # ft
  TauMod_thr <- 35   # s
  
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
  
  # Calculate the range
  R <- sqrt(apply(dXYZ[, 1:2, drop = FALSE]^2, 1, sum))
  
  # Time to the closest point of approach (s)
  tCPA <- calculateTCPA(trajectory1, trajectory2)
  
  # Horizontal missed distance
  HMD <- sqrt((dXYZ[, 1] + relativeVelocity[, 1] * tCPA)^2 + 
                (dXYZ[, 2] + relativeVelocity[, 2] * tCPA)^2 )
  
  # Note: the code below here is very close to a direct translation of the 
  # MATLAB script to R (with a few modifications to permit parallelization). 
  # Additional optimizations are possible.

  # Horizontal size of the Hazard Zone (TauMod_thr boundary)
  Rdot <- apply(dXYZ[, 1:2, drop = FALSE] * relativeVelocity, 1, sum) / R
  S <- pmax(DMOD, .5 * (sqrt( (Rdot * TauMod_thr)^2 + 4 * DMOD^2) - Rdot * TauMod_thr))
  # Safeguard against x/0
  S[R < 1e-4] <- DMOD

  # Three penetration components (Range, HMD, vertical separation)
  RangePen <- pmin((R / S), 1)
  HMDPen   <- pmin((HMD / DMOD), 1)
  DHPen    <- pmin((dXYZ[, 3] / DH_thr), 1)

  hpen <- FGnorm(RangePen, HMDPen)
  vSLoWC <- 100 * (1 - FGnorm(hpen, DHPen))

  return(vSLoWC)
}

# The Fernandez-Gausti squircular operator.
FGnorm <- function(x, y) {
  return(sqrt(x^2 + (1 - x^2) * y^2))
}
