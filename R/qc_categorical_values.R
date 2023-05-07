# FUNCTION: Categorical Variable Validity ##############################
# objective: check categorical values in the data against the allowable values in the config_list
# inputs (arguments): df (input dataframe to undergo QAQC), p (test-specific parameters derived from config file)
# outputs (returns): test results as a list

#' Categorical Values Test
#'
#' @param df
#' @param p
#'
#' @return
#' @export
#'
#' @examples
categorical_values <- function(df, p){

  # for each protocol identified in the config list, gather the data
  dat <- lapply(names(p), function(x){
    df[[x]]
  })

  # rename list elements
  names(dat) <- names(p)

  # for each protocol identified in the config list, extract the categorical value vectors
  allowable_values <- lapply(names(p), function(x){
    p[[x]]$values
  })

  # gather data and allowable values in a list
  input <- list(dat, allowable_values)

  # return validity where -3 = invalid categorical variable, -2 = missing or NA, 0 = valid for each table
  return(
    pmap(input,  ~ case_when(
      # is.na(..1) ~ -2, # missing values are now a separate test
      !(..1 %in% ..2) ~ -3,
      T ~ 0
    ))
  )
}
