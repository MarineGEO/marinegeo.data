
# Input example
# List of 1
# $ sample_dry_mass_g:List of 2
# ..$ fail_range: num [1:2] 0 5000
# ..$ type      : chr "exclusive"

# Output example
# List of 1
#$ sample_dry_mass_g: num [1:159] 0 0 0 0 0 0 0 0 0 0 ...

test_that("numeric range (exclusive): all QC pass", {

  df <- tibble::tibble(
    "sample_dry_mass_g" = c(1, 2, NA)
  )

  p <- setNames(
    list(
      list("fail_range" = c(0, 100),
           "type" = "exclusive")
    ), "sample_dry_mass_g"
  )

  expect_equal(
    numeric_range(df, p),
    list("sample_dry_mass_g" = c(0,0,0))
  )

})

test_that("numeric range (exclusive): 2 QC fail", {

  df <- tibble::tibble(
    "sample_dry_mass_g" = c(0, 101, NA)
  )

  p <- setNames(
    list(
      list("fail_range" = c(0, 100),
           "type" = "exclusive")
    ), "sample_dry_mass_g"
  )

  expect_equal(
    numeric_range(df, p),
    list("sample_dry_mass_g" = c(-4, -5, 0))
  )

})

test_that("numeric range (inclusive): all QC pass", {

  df <- tibble::tibble(
    "sample_dry_mass_g" = c(0, 100, 5, NA)
  )

  p <- setNames(
    list(
      list("fail_range" = c(0, 100),
           "type" = "inclusive")
    ), "sample_dry_mass_g"
  )

  expect_equal(
    numeric_range(df, p),
    list("sample_dry_mass_g" = c(0,0,0,0))
  )

})

test_that("numeric range (inclusive): 2 QC fail", {

  df <- tibble::tibble(
    "sample_dry_mass_g" = c(0, -1, 100, NA)
  )

  p <- setNames(
    list(
      list("fail_range" = c(0, 100),
           "type" = "inclusive")
    ), "sample_dry_mass_g"
  )

  expect_equal(
    numeric_range(df, p),
    list("sample_dry_mass_g" = c(0,-4,0,0))
  )

})
