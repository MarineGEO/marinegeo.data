run_qc_multi_protocol <- function(data_list, grouping){

  # grouping <- "seagrass"

  test_parameters <- marinegeo_schema$protocol_grouping[[grouping]]$tests
  test_names <- names(test_parameters)

  results <- lapply(test_names, function(x){
    if(existsFunction(x)){
      get(x)(data_list, test_parameters[[x]])
    }
  })

  names(results) <- test_names

  return(results)

}
