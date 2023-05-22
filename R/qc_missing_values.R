# FUNCTION: Missing Values Test #########
# objective: check for missing values
# inputs (arguments): df (input dataframe to undergo QAQC), p (test-specific parameters derived from config file)
# outputs (returns): test results as a list

#' Missing Values Test
#'
#' @param df
#' @param p
#'
#' @return
#' @export
#'
#' @examples
missing_values <- function(df, p){

  # for each protocol identified in the config list, gather the data
  dat <- lapply(names(p), function(x){
    df[[x]]
  })

  # rename list elements
  names(dat) <- names(p)

  # assemble into single list
  input <- list(dat)

  # return validity where -3 = invalid categorical variable, -2 = missing or NA, 0 = valid for each table
  return(
    purrr::pmap(input,  ~ dplyr::case_when(
      is.na(..1) ~ -2,
      T ~ 0
    ))
  )
}
