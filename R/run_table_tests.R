#' Run Table Tests
#'
#' @param df
#' @param table_parameters
#'
#' @return
#' @export
#'
#' @examples
run_table_tests <- function(df, table_parameters){

  # Subset input list to only table tests (remove column tests)
  table_tests <- table_parameters$tests

  # Vector of column names expected in df
  # and assign to a column evaluation list in the tests list.
  schema_columns <- names(table_parameters$columns)
  table_tests[["column_evaluation"]] <- list("values" = schema_columns)

  # Get names of tests
  test_names <- names(table_tests)

  # Loop over tests and call function
  # all_results has a sub-list for each test
  # Results for each column in a test is a numeric vector within that sub-list
  # list
  #   test name list
  #     column name = results
  #     column name = results
  results <- lapply(test_names, function(x){
    if(existsFunction(x)){
      get(x)(df, table_tests[[x]])
    }
  })

  names(results) <- test_names

  return(results)
}
