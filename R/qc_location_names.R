# FUNCTION: Location Name Validity ##############################
# objective: check categorical values in the data against the allowable values in the config_list
# Because location names are acquired from the knowledge hub, this is a distinct test
# from the categorical validity test
# inputs (arguments): df (input dataframe to undergo QAQC), p (test-specific parameters derived from config file)
# outputs (returns): test results as a list

#' Location Name test
#'
#' @param df
#' @param p
#'
#' @return
#' @export
#'
#' @examples
location_names <- function(df, p){

  # The test depends on observatory code being present
  # If not, all values fail
  if(!"observatory_code" %in% colnames(df)){

    return(
      list(
        location_name = rep(-3, nrow(df))
      )
    )

  } else {

    p <- p$location_name$values %>%
      tibble::rowid_to_column("rowid")

    results <- dplyr::left_join(df, p, by = c("observatory_code", "location_name")) %>%
      dplyr::mutate(result = dplyr::case_when(
        is.na(rowid) & (is.na(location_name) | is.na(observatory_code)) ~ 0,
        is.na(rowid) ~ -3,
        T ~ 0
      )) %>%
      dplyr::pull(result)

    return(list(
      location_name = results
    ))

  }

}
