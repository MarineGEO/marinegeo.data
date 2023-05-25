# example input
# List of 1
# $ values: chr [1:6] "sample_event_id" "observatory_code" "location_name" "transect" ...

# Example output
# List of 1
# $ scientific_name: num [1:262] 0 0 0 0 0 0 0 0 0 0 ...

test_that("unique observation test, all pass", {

  df <- tibble::tibble(
    "sample_event_id" = c("x", "x", "x"),
    "transect" = c("a", "a", "a"),
    "scientific_name" = c("x", "y", "z"),
    "values" = c(1,2,3)
  )

  p <- list("values" = c("sample_event_id", "transect", "scientific_name"))

  expect_equal(
    unique_observation(df, p),
    list("scientific_name" = c(0,0,0))
  )

})

test_that("unique observation test, one fails", {

  df <- tibble::tibble(
    "sample_event_id" = c("x", "x", "x"),
    "transect" = c("a", "a", "a"),
    "scientific_name" = c("x", "x", "z"),
    "values" = c(1,2,3)
  )

  p <- list("values" = c("sample_event_id", "transect", "scientific_name"))

  expect_equal(
    unique_observation(df, p),
    list("scientific_name" = c(-3,-3,0))
  )

})

test_that("unique observation test with some NAs, some pass", {

  df <- tibble::tibble(
    "sample_event_id" = c("x", "x", NA),
    "transect" = c("a", "a", "a"),
    "scientific_name" = c("x", NA, "z"),
    "values" = c(1,2,3)
  )

  p <- list("values" = c("sample_event_id", "transect", "scientific_name"))

  expect_equal(
    unique_observation(df, p),
    list("scientific_name" = c(0,0,0))
  )

})

test_that("unique observation test all NAs, all fail", {

  df <- tibble::tibble(
    "sample_event_id" = c(NA, NA, NA),
    "transect" = c(NA, NA, NA),
    "scientific_name" = c(NA, NA, NA),
    "values" = c(1,2,3)
  )

  p <- list("values" = c("sample_event_id", "transect", "scientific_name"))

  expect_equal(
    unique_observation(df, p),
    list("scientific_name" = c(-3,-3,-3))
  )

})
