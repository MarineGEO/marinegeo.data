

get_table_qc <- function(df, protocol, table){

  output <- run_qc_on_table(df, protocol, table)

  summary <- create_quality_control_summary(output)

  return(summary)

}

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

  # Number of rows in the data
  num_rows <- length(quality_control_results[[1]][[1]][[1]])

  # Extract non-0 flags and convert to a dataframe for all tests
  summary_raw <- bind_rows(
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
    }))

  # If all tests passed, then nrow == 0
  if(nrow(summary_raw) > 0){
    summary <- summary_raw %>%
        group_by(column, test, result) %>%
        summarize(invalid_rows = case_when(
          length(id) == num_rows ~ "All rows",
          length(id) > 5 ~ "> 5 rows",
          T ~ paste(id, collapse=",")
        )) %>%
        mutate(result = case_when(
          result == -6 ~ "Unexpected number of quadrats",
          result == -5 ~ "Above maximum range",
          result == -4 ~ "Below minimum range",
          result == -3 ~ "Invalid Categorical Value",
          result == -2 ~ "Missing Value",
          T ~ NA_character_
        ))

  } else {
    summary <- tibble(
      result = "All tests passed"
    )
  }

  return(summary)

}

extract_table_warnings <- function(df, protocol, table){

  raw_qc <- run_qc_on_table(df, protocol, table)

  # Values or Table test
  output <- lapply(raw_qc, function(test_type){

    compact(
      # Tests
      setNames(
        lapply(names(test_type), function(test){

          # For each column, extract row numbers that had non-0 status
          rowids <- unname(unlist(
            lapply(test_type[[test]], function(column_results){
              which(column_results != 0)
            })
          ))

          if(length(rowids) > 0){
            df %>%
              rowid_to_column("id") %>%
              filter(id %in% rowids)
          } else {
            NULL
          }
        }),

        names(test_type)
      )
    )
  })

  # Remove tables and values hierarchical level
  output <- unlist(output, recursive = F)

  return(output)

}
