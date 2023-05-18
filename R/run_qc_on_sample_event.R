get_sample_event_qc <- function(data_list, grouping){

  table_output <- run_qc_on_sample_event(data_list)

  # Join in with grouping test
  grouping_output <- run_qc_multi_protocol(data_list, grouping)

  combined_output <- bind_rows(table_output, grouping_output)

  return(combined_output)

}

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

run_qc_multi_protocol <- function(data_list, grouping){

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
