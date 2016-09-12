library(flightconflicts)
context("calculateTCPA")

library(geosphere)

kacy <- c(-74.5771667, 39.4575833)
kphl <- c(-75.2408658, 39.8722494)

test_that("correct tCPA for head-on collision", {
  trajectory1 <- createTrajectory(kacy[1], kacy[2], 
                                  bearing = bearingRhumb(kacy, kphl),
                                  groundspeed = 200)
  trajectory2 <- createTrajectory(kphl[1], kphl[2], 
                                  bearing = bearingRhumb(kphl, kacy),
                                  groundspeed = 200)
  
  expect_equal(calculateTCPA(trajectory1, trajectory2),
               distGeo(kacy, kphl)/2 * 0.009719,
               tolerance = 1)
})
