#' Convert trajectories to Easting/Northing representation.
#' 
#' @param trajectory A flighttrajectory object.
#' @param origin A length 2 numeric vector giving the origin lon/lat for the
#'   conversion.
#' @return A flattrajectory object.
trajectoryToXYZ <- function(trajectory, origin) {
  if (!is.flighttrajectory(trajectory)) {
    stop("'trajectory' must be an instance of flighttrajectory")
  }
  if (!is.numeric(origin) || length(origin) != 2) {
    stop("'origin' must be a numeric vector of length 2")
  }
  
  trajectoryXY <- lonlatToXY(trajectory$longitude, trajectory$latitude, 
                             origin[1], origin[2])
  
  velocityXY <- bearingToXY(trajectory$bearing, trajectory$groundspeed)
  
  flattrajectory <- list(origin = origin,
                         position = cbind(trajectoryXY, trajectory$altitude),
                         velocity = velocityXY)
  class(flattrajectory) <- "flattrajectory"
  
  return(flattrajectory)
}
