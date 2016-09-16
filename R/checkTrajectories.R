#' Check that both arguments are trajectories with the same timecourse.
#' 
#' @param trajectory1 An object to test.
#' @param trajectory2 Another object to test.
#' @return TRUE if everything works. Raises an error otherwise.
#'   
#' @details There are two options for passing this test. Either both objects can
#'   be flighttrajectories, in which case the time stamps must match, or they
#'   can both be flattrajectories, in which case they must have the same number
#'   of samples and origin.
checkTrajectories <- function(trajectory1, trajectory2) {
  # Two flight trajectories
  if (flightpathr::is.flighttrajectory(trajectory1)) {
    if (!flightpathr::is.flighttrajectory(trajectory2)) {
      stop("'trajectory1' is a flighttrajectory; 'trajectory2' must match this type")
    }
    if (!isTRUE(all.equal(trajectory1$timestamp, trajectory2$timestamp))) {
      stop("flighttrajectories must have matching time stamps")
    }
  } else if (is.flattrajectory(trajectory1)) {
    if (!is.flattrajectory(trajectory2)) {
      stop("'trajectory1' is a flattrajectory; 'trajectory2' must match this type")
    }
    if (nrow(trajectory1$position) != nrow(trajectory2$position)) {
      stop("flattrajectories must have the same number of samples")
    }
    if (!isTRUE(all.equal(trajectory1$origin, trajectory2$origin))) {
      stop("flattrajectories must have the same origin")
    }
  }
  return(TRUE)
}
