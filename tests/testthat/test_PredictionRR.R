
test_that("calc_RF_response_from_probs", {
  x <- rbind(
    c(0.1, 0.4, 0.4, 0.1),
    c(0.7, 0.1, 0.1, 0.1),
    c(NA, 0.1, 0.1, 0.1)
  )

  res <- newRR3::calc_RF_response_from_probs(x)

  ## expected outcome: c(2 or 3, 1, NA)
  expect_true(res[[1L]] %in% c(2L, 3L))
  expect_identical(res[-1L], c(1L, NA_integer_))
})


test_that("calc_RR_index", {
  x <- rbind(
    c(0.1, 0.4, 0.4, 0.1),
    c(0.7, 0.1, 0.1, 0.1),
    c(NA, 0.1, 0.1, 0.1),
    c(1, 0, 0, 0),
    c(0, 1, 0, 0),
    c(0, 0, 0, 1),
    c(0.5, 0, 0, 0.5),
    c(0, 0.5, 0.5, 0)
  )

  expect_identical(
    newRR3::calc_RR_index(x),
    c(0, -0.6, NA, -1, -1 / 3, 1, 0, 0)
  )
})


test_that("RF certainty", {
  x <- rbind(
    c(0, 1, 0, 0),
    c(0.5, 0, 0, 0.5),
    c(0.4, 0, 0.1, 0.4),
    c(0.5, 0.4, 0.1, 0),
    c(0.25, 0.25, 0.25, 0.25)
  )

  expect_equal(
    newRR3::calc_excessp(x),
    c(1, 0, 0, 0.1, 0),
    tolerance = sqrt(.Machine[["double.eps"]])
  )

  expect_identical(
    newRR3::calc_pfirst(x),
    c(1, 0.5, 0.4, 0.5, 0.25)
  )
})


test_that("calc_MIRRP_from_dists", {
  x <- rbind(
    1L:5L,
    5L:1L,
    rep(4L, 5L),
    c(4L, 4L, 1L, 1L, 1L),
    rep(-1L, 5L)
  )

  expect_identical(
    newRR3::calc_MIRRP_from_dists(x, seed = 123L),
    c(5L, 1L, NA, 1L, NA)
  )
})



test_that("calc_nearest_column", {
  loc <- c(1.5, 2L, 4L, 2L)
  mat <- rbind(
    1L:5L,
    5L:1L,
    rep(4L, 5L),
    c(4L, 4L, 2L, 1L, 1L)
  )

  res <- newRR3::calc_nearest_column(loc, mat)

  ## expected outcome: c(1 or 2, 4, any of 1:5, 3)

  expect_true(res[[1L]] %in% c(1L, 2L))
  expect_true(res[[3L]] %in% 1L:5L)
  expect_identical(res[-c(1L, 3L)], c(4L, 3L))
})
