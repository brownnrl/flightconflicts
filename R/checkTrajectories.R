#' Check that both arguments are trajectories with the same timecourse. 
#' 
#' @param trajectory1 An object to test.
#' @param trajectory2 Another object to test.
#' @return TRUE if everything works. Raises an error otherwise.
checkTrajectories <- function(trajectory1, trajectory2) {
  if (!is.flighttrajectory(trajectory1) || !is.flighttrajectory(trajectory2)) {
    stop("Both arguments must be instances of flighttrajectory")
  }
  if (!isTRUE(all.equal(trajectory1$timestamp, trajectory2$timestamp))) {
    stop("Trajectories must have matching time stamps")
  }
  return(TRUE)
}
