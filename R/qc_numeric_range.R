# FUNCTION: Inclusive Range Test ##############################
# objective: check numeric values in the data against the inclusive values in the config_list
# inputs (arguments): df (input dataframe to undergo QAQC), p (test-specific parameters derived from config file)
# outputs (returns): test results as a list

#' Numeric Range Test
#'
#' @param df
#' @param p
#'
#' @return
#' @export
#'
#' @examples
numeric_range <- function(df, p){

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

  type <- lapply(names(p), function(x){
    p[[x]]$type
  })

  # gather data and allowable values in a list
  input <- list(dat, minimums, maximums, type)

  # return validity where -4 = below min range, -5 = above max range, 0 = valid for each table
  return(
    purrr::pmap(input,  ~ dplyr::case_when(
      is.na(..1) ~ 0,
      ..1 < ..2 & ..4 == "inclusive" ~ -4,
      ..1 > ..3 & ..4 == "inclusive" ~ -5,
      ..1 <= ..2 & ..4 == "exclusive" ~ -4,
      ..1 >= ..3 & ..4 == "exclusive" ~ -5,
      T ~ 0
    ))
  )

}
