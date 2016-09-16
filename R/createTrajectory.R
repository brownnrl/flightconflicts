#' Create a flighttrajectory object from the flight info.
#' 
#' @param longitude Required; numeric vector giving aircraft longitude in 
#'   degrees.
#' @param latitude Required; numeric vector giving aircraft latitude in degrees.
#' @param altitude Optional; numeric vector giving aircraft altitude (AGL) in 
#'   feet. If missing, it will be set to 0.
#' @param timestamp Optional; numeric vector giving the time of each observation
#'   in seconds. If missing, the observation period is assumed to be 1 s.
#' @param bearing Optional; numeric vector giving the current bearing in 
#'   degrees. If missing, it is estimated using pairs of successive lon/lat 
#'   observations.
#' @param groundspeed Optional; numeric vector giving the current ground speed 
#'   of the aircraft in knots. If missing, it is estimated using pairs of 
#'   successive lon/lat observations.
#' @return A flighttrajectory object encapsulating these parameters (with
#'   default values substituded as necessary).
#'   
#' @details \code{longitude} and \code{latitude} must be the same length. 
#'   \code{timestamp}, \code{bearing}, and \code{groundspeed}, if present, must 
#'   also match this length. \code{altitude} must also have a length equal to 
#'   these parameters or be scalar.
#'   
#' @export
createTrajectory <- function(longitude, latitude, altitude = 0, timestamp = NULL, 
                             bearing = NULL, groundspeed = NULL) {
  if (!is.numeric(longitude)) stop("\"longitude\" must be a numeric vector")
  nCoord <- length(longitude)

  # Helper function to throw an error if the length of a vector is incorrect.
  checkLength <- function(x) {
    if (!is.numeric(x)) {
      stop("\"", deparse(substitute(x)), "\" must be a numeric vector")
    } else if (length(x) != nCoord) {
      stop("Vector \"", deparse(substitute(x)), "\" has length = ", length(x), 
           ", expected length = ", nCoord)
    }
    return(TRUE)
  }

  checkLength(latitude)
  coords <- cbind(longitude, latitude)

  if (length(altitude) == 1) {
    altitude <- rep(altitude, nCoord)
  } else {
    checkLength(altitude)
  }
  
  if (is.null(timestamp)) {
    timestamp <- seq(1, nCoord)
  } else {
    checkLength(timestamp)
  }
  
  # Use flightpathr to calculate bearing between successive points if not 
  # specified.
  if (is.null(bearing)) {
    bearing <- flightpathr::coordsToBearing(cbind(coords, altitude))
    bearing[nCoord] <- bearing[nCoord-1]
  } else {
    checkLength(bearing)
  }
  
  # Use geosphere to find the distance between points and use the timestamps to
  # calculate groundspeed if not specified.
  if (is.null(groundspeed)) {
    distNM <- geosphere::distCosine(coords[1:(nCoord-1), ],
                                    coords[2:nCoord, ],
                                    r = 3444)
    groundspeed <- distNM / diff(timestamp) * 3600
    groundspeed <- c(groundspeed, groundspeed[nCoord-1])
  } else{
    checkLength(groundspeed)
  }
  
  flighttrajectory <- list(longitude = longitude,
                           latitude = latitude,
                           altitude = altitude,
                           timestamp = timestamp,
                           bearing = bearing,
                           groundspeed = groundspeed)
  class(flighttrajectory) <- "flighttrajectory"
  
  return(flighttrajectory)
}

#' Check if an object is a flighttrajectory
#' @export
is.flighttrajectory <- function(x) inherits(x, "flighttrajectory")
