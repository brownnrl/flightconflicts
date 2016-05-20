#' Determine whether an NMAC (near mid air collision) has occurred at each time 
#' point.
#' 
#' @param trajectory1 A \code{flighttrajectory} object corresponding to the
#'   first aircraft.
#' @param trajectory2 A \code{flighttrajectory} object corresponding to the
#'   second aircraft.
#' @return The logical vector indicating whether NMAC criteria are met at each 
#'   time point.
#'   
#' @details NMAC is a cylinder +/- 100 ft above and below the ownship with a 
#'   radius of 500 ft.
#'   
#' @export
identifyNMAC <- function(trajectory1, trajectory2) {
  if (!is.flighttrajectory(trajectory1) || !is.flighttrajectory(trajectory2)) {
    stop("Both arguments must be instances of flighttrajectory")
  }
  if (!isTRUE(all.equal(trajectory1$timestamp, trajectory2$timestamp))) {
    stop("Trajectories must have matching time stamps")
  }

  horizontalDistanceFt <- geosphere::distHaversine(cbind(trajectory1$longitude,
                                                         trajectory1$latitude),
                                                   cbind(trajectory2$longitude,
                                                         trajectory2$latitude),
                                                   r = 20925646)
  verticalDistanceFt <- abs(trajectory1$altitude - trajectory2$altitude)

  isNMAC <- (horizontalDistanceFt < 500) | (verticalDistanceFt < 100)

  return(isNMAC)
}



#' Determine whether loss of Well Clear (LoWC) has occurred at each time point.
#' 
#' @param trajectory1 A \code{flighttrajectory} object corresponding to the 
#'   first aircraft.
#' @param trajectory2 A \code{flighttrajectory} object corresponding to the 
#'   second aircraft.
#' @return The logical vector indicating whether Well Clear has been violated at
#'   each time point.
#'   
#' @details This code relies on \code{\link{calculateSLoWC}}, see its
#'   documentation for details.
#'   
#' @export
identifyLoWC <- function(trajectory1, trajectory2) {
  return(calculateSLoWC(trajectory1, trajectory2) > 0)
}
