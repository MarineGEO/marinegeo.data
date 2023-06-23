
#' Summarize sample event
#'
#' @param data_list
#' @param p
#'
#' @return
#' @export
#'
#' @examples
sample_event_summary <- function(data_list, p){

  #browser()

  # Subset data list by target protocols
  target_protocols <- data_list[names(data_list) %in% p$protocols]
  target_columns <- p$columns

  se_summary <- dplyr::bind_rows(
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
    mutate(protocol = gsub("-", "_", paste0(protocol, "_", table)),
           status = T) %>%
    select(-table) %>%
    pivot_wider(names_from = protocol, values_from = status)

  return(se_summary)
}
