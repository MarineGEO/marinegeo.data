# FUNCTION: Integer Range Test ##############################
# objective: check numeric values in the data against the inclusive values in the config_list
# inputs (arguments): df (input dataframe to undergo QAQC), p (test-specific parameters derived from config file)
# outputs (returns): test results as a list

#' Integer Range Test
#'
#' @param df
#' @param p
#'
#' @return
#' @export
#'
#' @examples
integer_range <- function(df, p){

  # for each protocol identified in the config list, gather the data
  dat <- lapply(names(p), function(x){
    df[[x]]
  })

  # rename list elements
  names(dat) <- names(p)

  # for each protocol identified in the config list, extract the lower end of the range
  minimums <- lapply(names(p), function(x){
    p[[x]]$fail_range[1]
  })

  # for each protocol identified in the config list, extract the upper end of the range
  maximums <- lapply(names(p), function(x){
    p[[x]]$fail_range[2]
  })

  # gather data and allowable values in a list
  input <- list(dat, minimums, maximums)

  # return validity where -4 = below min range, -5 = above max range, -3 = in range but not integer, 0 = valid for each table
  # Values must be within the range and also an integer value
  return(
    pmap(input,  ~ case_when(
      ..1 %in% ..2:..3 ~ 0,
      is.na(..1) ~ 0, # Missing values are separate test
      ..1 < ..2 ~ -4,
      ..1 > ..3 ~ -5,
      T ~ -3 # This should be the case when a decimal value is within the integer range
    ))
  )
}
