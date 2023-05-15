test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# List of 1
# $ location_name:List of 1
# ..$ values: tibble [44 × 2] (S3: tbl_df/tbl/data.frame)
# .. ..$ observatory_code: chr [1:44] "PAN-BDT" "PAN-BDT" "PAN-BDT" "PAN-BDT" ...
# .. ..$ location_name   : chr [1:44] "STRI Seagrass" "Almirante Seagrass" "Juan Point Seagrass" "STRI Tower" ...


# Output Example (actual output is unnamed list)
# $ location_names    :List of 1
# ..$ location_name: num [1:305] 0 0 0 0 0 0 0 0 0 0 ...

test_that("test works, all pass", {

  df <- tibble::tibble(
    "observatory_code" = c("PAN-BDT", "PAN-BDT", "PAN-BDT", NA),
    "location_name" = c(NA, "STRI Seagrass", "Almirante Seagrass", NA)
  )

  p <- setNames(
    list(list("values" = tibble::tibble(
      "observatory_code" = c("PAN-BDT", "PAN-BDT"),
      "location_name" = c("STRI Seagrass", "Almirante Seagrass")
    ))), "location_name"
  )

  expect_equal(
    location_names(df, p),
    list("location_name" = c(0,0,0,0))
  )

})

test_that("test works, 1 missing value", {

  df <- tibble::tibble(
    "observatory_code" = c("PAN-BDT", "PAN-BDT", "PAN-BDT", NA),
    "location_name" = c(NA, "STRI Seagrass", "Almirante", NA)
  )

  p <- setNames(
    list(list("values" = tibble::tibble(
      "observatory_code" = c("PAN-BDT", "PAN-BDT"),
      "location_name" = c("STRI Seagrass", "Almirante Seagrass")
    ))), "location_name"
  )

  expect_equal(
    location_names(df, p),
    list("location_name" = c(0,0,-3,0))
  )

})

test_that("test works, all values fail", {

  df <- tibble::tibble(
    "observatory_code" = c("PAN-BDT", "PAN-BDT"),
    "location_name" = c("Seagrass", "Almirante")
  )

  p <- setNames(
    list(list("values" = tibble::tibble(
      "observatory_code" = c("PAN-BDT", "PAN-BDT"),
      "location_name" = c("STRI Seagrass", "Almirante Seagrass")
    ))), "location_name"
  )

  expect_equal(
    location_names(df, p),
    list("location_name" = c(-3,-3))
  )
})
