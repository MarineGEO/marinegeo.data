# Input example
# List of 1
# $ values: chr [1:11] "sample_event_id" "observatory_code" "location_name" "transect" ...

test_that("column evaluation test, all pass", {

  df <- tibble::tibble(
    "cover_code" = c(NA, "B", "A"),
    "sample_event_id" = c("x", "y", "z")
  )

  p <- list("values" = c("cover_code", "sample_event_id"))

  expect_equal(
    column_evaluation(df, p),
    list()
  )

})

test_that("column evaluation test, one fails (missing)", {

  df <- tibble::tibble(
    "cover_code" = c(NA, "B", "A")
  )

  p <- list("values" = c("cover_code", "sample_event_id"))

  expect_equal(
    column_evaluation(df, p),
    list("sample_event_id" = c(-2,-2,-2))
  )

})

test_that("column evaluation test, one fails (invalid)", {

  df <- tibble::tibble(
    "cover_code" = c(NA, "B", "A"),
    "sample_event_id" = c("x", "y", "z"),
    "transect" = c(1,2,3)
  )

  p <- list("values" = c("cover_code", "sample_event_id"))

  expect_equal(
    column_evaluation(df, p),
    list("transect" = c(-3,-3,-3))
  )

})

test_that("column evaluation test, all fail (invalid)", {

  df <- tibble::tibble(
    "cover_code1" = c(NA, "B", "A"),
    "sample_event_id2" = c("x", "y", "z"),
    "transect3" = c(1,2,3)
  )

  p <- list("values" = c("cover_code", "sample_event_id"))

  expect_equal(
    column_evaluation(df, p),
    list("cover_code1" = c(-3,-3,-3),
         "sample_event_id2" = c(-3,-3,-3),
         "transect3" = c(-3,-3,-3),
         "cover_code" = c(-2,-2,-2),
         "sample_event_id" = c(-2,-2,-2))
  )

})
