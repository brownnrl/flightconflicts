% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateTCPA.R
\name{calculateTCPA}
\alias{calculateTCPA}
\title{Calculate the time to closest point of approach (TCPA) between two aircraft 
at each time point of their their trajectories.}
\usage{
calculateTCPA(trajectory1, trajectory2)
}
\arguments{
\item{trajectory1}{A \code{flighttrajectory} object corresponding to the 
first aircraft.}

\item{trajectory2}{A \code{flighttrajectory} object corresponding to the 
second aircraft.}
}
\value{
The numeric vector giving the TCPA. Values in seconds.
}
\description{
Calculate the time to closest point of approach (TCPA) between two aircraft 
at each time point of their their trajectories.
}
\details{
This code is based on the SLoWC formulation by Ethan Pratt and Jacob
  Kay as implemented in a MATLAB script by Ethan Pratt dated 2016-04-18. This
  code calculates horizontal CPA only.
}

