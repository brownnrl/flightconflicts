#' Interpolate a trajectory (in time)
#' 
#' @param trajectory A \code{flighttrajectory} object.
#' @param timestamp The new timestamp along which the data should be 
#'   interpolated.
#' @return A new \code{flighttrajectory} with the given \code{timestamp}
#'   
#' @details This just performs linear interpolation for all of the values in the
#'   trajectory. A better approach would make use of the bearing and velocity
#'   information to smoothly interpolate the coordinates.
#'
#' @export
interpolateTrajectory <- function(trajectory, timestamp) {
  # We'll interpolate all of the features (except the timestamp, which is
  # specified).
  trajectoryFeatures <- names(trajectory)
  trajectoryFeatures <- trajectoryFeatures[trajectoryFeatures != "timestamp"]
  
  # Create a new trajectory with 
  newTrajectory <- list()
  for (trajectoryFeature in trajectoryFeatures) {
    newTrajectory[[trajectoryFeature]] <- approx(x = trajectory$timestamp,
                                                 y = trajectory[[trajectoryFeature]],
                                                 xout = timestamp,
                                                 method = "linear",
                                                 rule = 2)$y
  }
  newTrajectory[["timestamp"]] <- timestamp
  
  return(do.call(createTrajectory, newTrajectory))
}
