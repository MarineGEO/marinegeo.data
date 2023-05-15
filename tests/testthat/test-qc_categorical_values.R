# List of 5
# $ observatory_code:List of 1
# ..$ values: chr [1:42] "PER-CCP" "USA-LJC" "BLZ-CBC" "PAN-BDT" ...
# $ scientific_name :List of 1
# ..$ values: chr [1:280] "Gnatholepis thompsoni" "Hypoplectrus puella" "Lytechinus variegatus" "Chaetodon capistratus" ...
# $ taxonomic_id    :List of 1
# ..$ values: num [1:277] 277493 281120 367850 159661 158815 ...
# $ cover_code      :List of 1
# ..$ values: chr [1:7] "A" "B" "C" "D" ...
# $ sediment_type   :List of 1
# ..$ values: chr [1:2] "sand" "mud"


# Output Example (actual output is unnamed list)

# $ categorical_values:List of 5
# ..$ observatory_code: num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ scientific_name : num [1:305] -3 0 0 -3 -3 0 0 -3 -3 0 ...
# ..$ taxonomic_id    : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ cover_code      : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...
# ..$ sediment_type   : num [1:305] 0 0 0 0 0 0 0 0 0 0 ...


test_that("test works, all pass", {

  df <- tibble::tibble(
    "cover_code" = c(NA, "B", "A")
  )

  p <- setNames(
    list(
      list("values" = c("A", "B", "C"))
    ), "cover_code"
  )

  expect_equal(
    categorical_values(df, p),
    list("cover_code" = c(0,0,0))
  )

})

test_that("test works, 1 missing value", {

  df <- tibble::tibble(
    "cover_code" = c("A", "X", "A")
  )

  p <- setNames(
    list(
      list("values" = c("A", "B", "C"))
    ), "cover_code"
  )

  expect_equal(
    categorical_values(df, p),
    list("cover_code" = c(0,-3,0))
  )

})

test_that("test works, all values fail", {

  df <- tibble::tibble(
    "cover_code" = c("Z", "X", "Y")
  )

  p <- setNames(
    list(
      list("values" = c("A", "B", "C"))
    ), "cover_code"
  )

  expect_equal(
    categorical_values(df, p),
    list("cover_code" = c(-3,-3,-3))
  )

})
