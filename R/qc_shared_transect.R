
shared_transect <- function(data_list, p){

  # Subset data list by target protocols
  target_protocols <- data_list[names(data_list) %in% p$protocols]
  target_columns <- p$columns

  # This will invalid results if any dataframes end up NULL
  total_num_tables <- sum(unlist(
    lapply(target_protocols, function(x){
      length(x)
    })
  ))

  flagged_transects <- dplyr::bind_rows(
    lapply(names(target_protocols), function(protocol_name){

      dplyr::bind_rows(
        purrr::compact(
          lapply(names(target_protocols[[protocol_name]]), function(table_name){

            df <- target_protocols[[protocol_name]][[table_name]]

            if(all(target_columns %in% colnames(df))){

              df %>%
                dplyr::select(all_of(target_columns)) %>%
                dplyr::distinct() %>%
                dplyr::mutate(protocol = protocol_name,
                              table = table_name)
            } else {
              NULL
            }
          })
        )
      )
    })
  ) %>%
    count(across(all_of(target_columns))) %>%
    filter(n != total_num_tables)

  missing_transects <- dplyr::bind_rows(
    lapply(names(target_protocols), function(protocol_name){

      dplyr::bind_rows(
        purrr::compact(
          lapply(names(target_protocols[[protocol_name]]), function(table_name){

            df <- target_protocols[[protocol_name]][[table_name]] %>%
              dplyr::select(all_of(target_columns)) %>%
              dplyr::distinct()

            if(all(target_columns %in% colnames(df))){

              flagged_transects %>%
                dplyr::mutate(protocol = protocol_name,
                              table = table_name) %>%
                dplyr::anti_join(df, by = target_columns)

            } else {
              NULL
            }


          })
        )
      )
    })
  )

  return(missing_transects)
}
