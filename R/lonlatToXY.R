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
  dlon <- longitude - originLongitude
  dlat <- latitude - originLatitude
  
  # Radius in feet and flattening based on WGS84
  a <- 20925646
  f <- 1/298.257223563
  e2 <- f*(2-f)
  
  R1 <- a*(1-e2)/(1-e2*(sin(originLatitude))^2)^(3/2)
  R2 <- a/sqrt(1-e2*(sin(originLatitude))^2)
  
  xDistance <- R2 * cos(originLatitude) * dlon
  yDistance <- R1 * dlat
  
  return(cbind(xDistance, yDistance))
}
