# For each transect, expect 1 sample collection date
transect_dates <- function(data_list, p){

  # Subset data list by target protocols
  target_protocols <- data_list[names(data_list) %in% p$protocols]

  target_columns <- c(p$columns, p$date_column)

  flagged_dates <- dplyr::bind_rows(
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
    group_by(across(all_of(p$columns))) %>%
    summarize(total_dates = n_distinct({ p$date_column }),
              tables = paste(protocol, " ", table, collapse = ", ")) %>%
    filter(total_dates > 1)

  return(flagged_dates)
}
