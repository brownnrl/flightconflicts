library(flightconflicts)
context("calculateSLoWC")

library(geosphere)

# I'll attempt to replicate the values in Appendix A of SC-228.

test_that("co-altitude head-on examples match", {
  kacy <- c(-74.5771667, 39.4575833)
  # Traveling at 100 kt for 3 nm.
  ownshipCoords <-  destPoint(kacy, 90, seq(0, 5556, length.out = 108))
  intruderCoords <- apply(ownshipCoords, 2, rev)
  
  ownshipTrajectory <- createTrajectory(ownshipCoords[, 1], ownshipCoords[, 2])
  intruderTrajectory <- list()
  hmd <- c(0, 500, 1000, 2000, 3000, 3999, 5000)

  for (i in seq_along(hmd)) {
    intruderCoordsHMD <- destPoint(intruderCoords, 0, hmd[i]*0.3048)
    intruderTrajectory[[i]] <- createTrajectory(intruderCoordsHMD[, 1], intruderCoordsHMD[, 2])
  }
  
  # High tolerances to deal with different sampling rates
  expect_equal(max(calculateSLoWC(ownshipTrajectory, intruderTrajectory[[1]])), 100.00, tolerance = 5)
  expect_equal(max(calculateSLoWC(ownshipTrajectory, intruderTrajectory[[2]])), 85.65, tolerance = 5)
  expect_equal(max(calculateSLoWC(ownshipTrajectory, intruderTrajectory[[3]])), 71.47, tolerance = 5)
  expect_equal(max(calculateSLoWC(ownshipTrajectory, intruderTrajectory[[4]])), 44.42, tolerance = 5)
  expect_equal(max(calculateSLoWC(ownshipTrajectory, intruderTrajectory[[5]])), 20.00, tolerance = 5)
  expect_equal(max(calculateSLoWC(ownshipTrajectory, intruderTrajectory[[6]])), 0.02, tolerance = 5)
  expect_equal(max(calculateSLoWC(ownshipTrajectory, intruderTrajectory[[7]])), 0)
})
