#' Run QC on sample event
#'
#' @param data_list
#'
#' @return
#' @export
#'
#' @examples
run_qc_on_sample_event <- function(data_list, dataset = "L2_standardized"){

  # Loop over each protocol and table
  # Bind summary dataframes together
  # The raw QC output is not kept
  output <- bind_rows(
    lapply(names(data_list), function(protocol_name){

      bind_rows(
        lapply(names(data_list[[protocol_name]]), function(table_name){

          run_qc_on_table(data_list[[protocol_name]][[table_name]],
                       protocol_name, table_name, dataset) %>%
            mutate(protocol = protocol_name,
                   table = table_name,
                   dataset = !!dataset) %>%
            select(protocol, table, dataset, everything())

        })
      )
    })
  )

  return(output)

}

#' Conduct sample event summary
#'
#' @param data_list
#' @param grouping
#'
#' @return
#' @export
#'
#' @examples
run_sample_event_summary <- function(data_list, grouping){

  tests <- marinegeo_schema$cross_protocol_tests
  test_names <- names(marinegeo_schema$cross_protocol_tests)

  results <- purrr::compact(
    setNames(
      lapply(test_names, function(x){

        test_grouping <- tests[[x]]$grouping

        if(existsFunction(x) & test_grouping %in% c("cross-protocol", grouping)){
          get(x)(data_list, tests[[x]]$parameters)
        } else {
          NULL
        }
      }),
      test_names
    )
  )

  # results_df <- bind_rows(
  #   compact(
  #     lapply(results, function(result){
  #       if(nrow(result) == 0){
  #         NULL
  #       } else {
  #         result
  #       }
  #     })
  #   )
  # )

  return(results)

}
