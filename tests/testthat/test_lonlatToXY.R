library(flightconflicts)
context("lonlatToXY")

test_that("Returns sensible values", {
  kacyLon <- -74.5771667
  kacyLat <- 39.4575833
  # Find points 500 feet (152.4 m) from the origin
  compassPoints <- geosphere::destPoint(c(kacyLon, kacyLat),
                                        b = seq(0, 359, by = 45),
                                        d = 152.4)
  
  xyFeet <- lonlatToXY(compassPoints[, 1], compassPoints[, 2], kacyLon, kacyLat)
  
  expect_equal(xyFeet[1, ], c(0, 500), tolerance = 1e-4)
  expect_equal(xyFeet[3, ], c(500, 0), tolerance = 1e-4)
  expect_equal(sqrt(apply(xyFeet^2, 1, sum)), rep(500, 8), tolerance = 1e-4)
})
