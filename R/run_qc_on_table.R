
#' Run QC tests on tables
#'
#' @param df
#' @param protocol
#' @param table
#'
#' @return
#' @export
#'
#' @examples
run_qc_on_table <- function(df, protocol, table){

  # Assemble parameters by injecting knowledge hub vars into table params
  # and removing all other protocol-table combos
  table_parameters <- assemble_table_parameters(marinegeo_schema, protocol, table)

  # Run QC tests
  # Value tests run on the values associated with one or more column in a dataframe
  value_test_results <- run_value_tests(df, table_parameters)

  # Table tests
  table_test_results <- run_table_tests(df, table_parameters)

  output <- # create_quality_control_summary(
    list(
      values = value_test_results,
      tables = table_test_results
    )
  # )

  return(output)
}

#' Create Quality Control Summary
#'
#' @param quality_control_results
#'
#' @return
#' @export
#'
#' @examples
create_quality_control_summary <- function(quality_control_results){

  summary <- bind_rows(
    lapply(quality_control_results, function(test_type){

      test_names <- names(test_type)
      bind_rows(
        lapply(test_names, function(test_name){

          column_names <- names(test_type[[test_name]])

          bind_rows(
            lapply(column_names, function(column_name){

              test_result <- test_type[[test_name]][[column_name]]
              unique_results <- unique(test_result)[unique(test_result) != 0]

              bind_rows(
                lapply(unique_results, function(failure_type){

                  tibble(result = test_result) %>%
                    rowid_to_column("id") %>%
                    filter(result == failure_type) %>%
                    mutate(test = test_name,
                           column = column_name)

                }))
            }))
        }))
    })) %>%
    group_by(column, test, result) %>%
    summarize(invalid_rows = case_when(
      length(id) > 5 ~ "> 5 rows",
      T ~ paste(id, collapse=",")
    ))
}


# tibble(numeric_result = results) %>%
#   filter(numeric_result != 0) %>%
#   mutate(result = case_when(
#     numeric_result == "-6" ~ "Unexpected number of quadrats",
#     numeric_result == "-5" ~ "Above maximum range",
#     numeric_result == "-4" ~ "Below minimum range",
#     numeric_result == "-3" ~ "Invalid Categorical Value",
#     numeric_result == "-2" ~ "Missing Value",
#     T ~ NA_character_
#   )) %>%
#   count(result) %>%
#   mutate(
#     column = column_name,
#     test = test_name
#   )
