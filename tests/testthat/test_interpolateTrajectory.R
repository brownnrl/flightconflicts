library(flightconflicts)
context("interpolateTrajectory")

library(geosphere)

kacy <- c(-74.5771667, 39.4575833)
k17n <- c(-75.0330031, 39.7054758)

# Two identical trajectories with different sampling rates
coords1 <-  gcIntermediate(kacy, k17n, n = 61)
trajectory1 <-createTrajectory(coords1[, 1], coords1[, 2], altitude = 3500,
                               timestamp = seq(0, 800, length.out = 61))
coords2 <- gcIntermediate(kacy, k17n, n = 229)
trajectory2 <-createTrajectory(coords2[, 1], coords2[, 2], altitude = 3500,
                               timestamp = seq(0, 800, length.out = 229))

test_that("Interpolated trajectory is close to correct", {
  trajectoryInterpolated <- interpolateTrajectory(trajectory1,
                                                  trajectory2$timestamp)
  
  for (tf in names(trajectoryInterpolated)) {
    expect_equal(trajectoryInterpolated[[tf]], trajectory2[[tf]], tolerance = 1)
  }
})

test_that("Correctly handling times outside original range", {
  trajectoryInterpolated <- interpolateTrajectory(trajectory1,
                                                  c(-100, 0, 800, 900))
  
  for (tf in names(trajectoryInterpolated)[names(trajectoryInterpolated) != "timestamp"]) {
    expect_equal(trajectoryInterpolated[[tf]][1], trajectoryInterpolated[[tf]][2])
    expect_equal(trajectoryInterpolated[[tf]][3], trajectoryInterpolated[[tf]][4])
  }
})
