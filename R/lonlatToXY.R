#' Convert lon/lat coordinates to north/east coordinates (in feet) using a 
#' flat-Earth approximation.
#' 
#' @param longitude A numeric vector.
#' @param latitude A numeric vector.
#' @param originLongintude The logntidue for which x = 0 feet.
#' @param originLatitude The latitude for which x = 0 feet.
#' @return A n x 2 matrix giving the x and y distance (respectively), in feet,
#'   of each longitude/latitude pair from the origin longitude/latitude.
lonlatToXY <- function(longitude, latitude, originLongitude, originLatitude) {
  # This implementation is based on code from:
  # http://williams.best.vwh.net/avform.htm#flat
  lon0 <- originLongitude * pi / 180.0
  lat0 <- originLatitude * pi / 180.0
  
  dlon <- longitude * pi / 180.0 - lon0
  dlat <- latitude * pi / 180.0 - lat0
  
  # Radius in feet and flattening based on WGS84
  a <- 20925646
  #a <- 6378137
  f <- 1/298.257223563
  e2 <- f*(2-f)
  
  R1 <- a*(1-e2)/(1-e2*(sin(lat0))^2)^(3/2)
  R2 <- a/sqrt(1-e2*(sin(lat0))^2)
  
  xyFeet <- cbind(R2 * cos(lat0) * dlon, 
                  R1 * dlat)
  
  return(xyFeet)
}

#' Convert a bearing (in degrees) and velocity (in knots) to north and east 
#' velocity (in ft / s).
#' 
#' @param bearing A numeric vector giving the instantaneous direction of the 
#'   velocity in degrees.
#' @param speed A numeric vector giving the instantaneous magnitude of the 
#'   velocity in knots.
#' @return A n x 2 matrix giving the north/south and east/west components of the
#'   velocity, in feet/s.
bearingToXY <- function(bearing, speed) {
  # Velocity should be in knots. Convert to ft / s
  fps <- speed * 1.68781
  # Bearing should be degrees from north. Convert to radians.
  theta <- bearing * pi / 180
  # Return x and y components of the velocity in ft / s
  return(cbind(fps * sin(theta),
               fps * cos(theta)))
}
