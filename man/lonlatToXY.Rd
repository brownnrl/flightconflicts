% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lonlatToXY.R
\name{lonlatToXY}
\alias{lonlatToXY}
\title{Convert lon/lat coordinates to north/east coordinates (in feet) using a 
flat-Earth approximation.}
\usage{
lonlatToXY(longitude, latitude, originLongitude, originLatitude)
}
\arguments{
\item{longitude}{A numeric vector.}

\item{latitude}{A numeric vector.}

\item{originLatitude}{The latitude for which x = 0 feet.}

\item{originLongintude}{The logntidue for which x = 0 feet.}
}
\value{
A n x 2 matrix giving the x and y distance (respectively), in feet,
  of each longitude/latitude pair from the origin longitude/latitude.
}
\description{
Convert lon/lat coordinates to north/east coordinates (in feet) using a 
flat-Earth approximation.
}

