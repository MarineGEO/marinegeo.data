#' Run QC on sample event
#'
#' @param data_list
#'
#' @return
#' @export
#'
#' @examples
run_qc_on_sample_event <- function(data_list){

  # Loop over each protocol and table
  # Bind summary dataframes together
  # The raw QC output is not kept
  output <- bind_rows(
    lapply(names(data_list), function(protocol_name){

      bind_rows(
        lapply(names(data_list[[protocol_name]]), function(table_name){
          get_table_qc(data_list[[protocol_name]][[table_name]],
                       protocol_name, table_name) %>%
            mutate(protocol = protocol_name,
                   table = table_name) %>%
            select(protocol, table, everything())

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

  test_parameters <- marinegeo_schema$protocol_grouping[[grouping]]$tests
  test_names <- names(test_parameters)

  results <- lapply(test_names, function(x){
    if(existsFunction(x)){
      get(x)(data_list, test_parameters[[x]]) %>%
        mutate(test = x)
    }
  })

  results_df <- bind_rows(
    compact(
      lapply(results, function(result){
        if(nrow(result) == 0){
          NULL
        } else {
          result
        }
      })
    )
  )

  return(results_df)

}
