library(flightconflicts)
context("calculateTCPA")

library(geosphere)
library(flightpathr)

kacy <- c(-74.5771667, 39.4575833)
kphl <- c(-75.2408658, 39.8722494)

meterPer200Knots <- 0.009719

test_that("correct tCPA for head-on collision", {
  trajectory1 <- createTrajectory(kacy[1], kacy[2], 
                                  bearing = bearingRhumb(kacy, kphl),
                                  groundspeed = 200)
  trajectory2 <- createTrajectory(kphl[1], kphl[2], 
                                  bearing = bearingRhumb(kphl, kacy),
                                  groundspeed = 200)
  
  expect_equal(calculateTCPA(trajectory1, trajectory2),
               distGeo(kacy, kphl)/2 * meterPer200Knots,
               tolerance = 1)
})

test_that("TCPA for actual trajectories", {
  numPoints <- round(distGeo(kacy, kphl) * meterPer200Knots)
  coords <- gcIntermediate(kacy, kphl,
                           n = numPoints)
  trajectory1 <- createTrajectory(coords[, "lon"], coords[, "lat"])
  trajectory2 <- createTrajectory(rev(coords[, "lon"]), rev(coords[, "lat"]))
  tcpaVector <- calculateTCPA(trajectory1, trajectory2)
  
  expect_equal(tcpaVector[seq(ceiling(numPoints/2)+1, numPoints)],
               0 * seq(ceiling(numPoints/2)+1, numPoints))
  
  expect_equal(diff(tcpaVector[seq(1, floor(numPoints/2))]), 
               -1*diff(seq(1, floor(numPoints/2))),
               tolerance = 1e-3)
})
