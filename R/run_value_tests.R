#' Run Value Tests
#'
#' @param df
#' @param table_parameters
#'
#' @return
#' @export
#'
#' @examples
run_value_tests <- function(df, table_parameters){

  variables <- colnames(df)

  column_parameters <- table_parameters$columns

  # Only keep columns that are present in df
  # And columns that have tests
  parameters_subset <- compact(
    lapply(column_parameters[names(column_parameters) %in% variables], function(params){

      if("tests" %in% names(params)){
        params
      } else {
        NULL
      }

    })
  )

  # Get names of tests
  test_names <- unique(unlist(
    lapply(parameters_subset, function(column_list){
      names(column_list$tests)
    })
  ))


  # reorganize by test - column, rather than column - test
  parameters_test <- lapply(test_names, function(test){

    compact(
      lapply(parameters_subset, function(column_name){

        if(test %in% names(column_name$tests)){
          column_name$tests[[test]]
        } else {
          NULL
        }

      })
    )

  })

  names(parameters_test) <- test_names

  # Loop over tests and call function
  # all_results has a sub-list for each test
  # Results for each column in a test is a numeric vector within that sub-list
  # list
  #   test name list
  #     column name = results
  #     column name = results
  results <- lapply(test_names, function(x){
    if(existsFunction(x)){
      get(x)(df, parameters_test[[x]])
    }
  })

  names(results) <- test_names

  return(results)
}
