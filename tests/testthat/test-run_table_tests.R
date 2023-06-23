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
