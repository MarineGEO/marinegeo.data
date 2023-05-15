# Input Example (p)
# List of 6
# $ sample_event_id       :List of 1
# ..$ value_required: logi TRUE
# $ transect              :List of 1
# ..$ value_required: logi TRUE
# $ quadrat               :List of 1
# ..$ value_required: logi TRUE
# $ sample_collection_date:List of 1
# ..$ value_required: logi TRUE
# $ cover_code            :List of 1
# ..$ value_required: logi TRUE
# $ sediment_type         :List of 1
# ..$ value_required: logi TRUE

# Output Example (actual output is unnamed list)
# $ missing_values    :List of 6
# ..$ sample_event_id       : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ transect              : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ quadrat               : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ sample_collection_date: num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ cover_code            : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ sediment_type         : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...


test_that("test works, all pass", {

  df <- tibble::tibble(
    "sample_event_id" = c(1, 2, 3)
  )

  p <- setNames(
    list(
      list("value_required" = T)
    ), "sample_event_id"
  )

  expect_equal(
    missing_values(df, p),
    list("sample_event_id" = c(0,0,0))
  )

})

test_that("test works, 1 missing value", {

  df <- tibble::tibble(
    "sample_event_id" = c(1, NA, 3)
  )

  p <- setNames(
    list(
      list("value_required" = T)
    ), "sample_event_id"
  )

  expect_equal(
    missing_values(df, p),
    list("sample_event_id" = c(0,-2,0))
  )

})

test_that("test works, all values fail", {

  df <- tibble::tibble(
    "sample_event_id" = c(NA, NA, NA)
  )

  p <- setNames(
    list(
      list("value_required" = T)
    ), "sample_event_id"
  )

  expect_equal(
    missing_values(df, p),
    list("sample_event_id" = c(-2,-2,-2))
  )

})

