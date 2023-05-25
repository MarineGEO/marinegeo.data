# List of 2
# $ transect:List of 1
# ..$ fail_range: num [1:2] 1 3
# $ quadrat :List of 1
# ..$ fail_range: num [1:2] 1 50

# Output Example (actual output is unnamed list)
# $ integer_range     :List of 2
# ..$ transect: num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ quadrat : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...

test_that("test works, all pass", {

  df <- tibble::tibble(
    "transect" = c(1, 2, NA)
  )

  p <- setNames(
    list(
      list("fail_range" = c(1, 3))
    ), "transect"
  )

  expect_equal(
    integer_range(df, p),
    list("transect" = c(0,0,0))
  )

})

test_that("test works, 2 failures", {

  df <- tibble::tibble(
    "transect" = c(1, 2, 4, 0)
  )

  p <- setNames(
    list(
      list("fail_range" = c(1, 3))
    ), "transect"
  )

  expect_equal(
    integer_range(df, p),
    list("transect" = c(0,0,-5,-4))
  )

})

test_that("test works, all values fail", {

  df <- tibble::tibble(
    "transect" = c(4, 5, 6)
  )

  p <- setNames(
    list(
      list("fail_range" = c(1, 3))
    ), "transect"
  )

  expect_equal(
    integer_range(df, p),
    list("transect" = c(-5,-5,-5))
  )

})
