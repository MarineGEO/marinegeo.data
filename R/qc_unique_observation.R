# FUNCTION: Unique Observation ##############################
# objective: check that a single observation exists for group of columns that indicate unique observation in data
# inputs (arguments): df (input dataframe to undergo QAQC), p (test-specific parameters derived from config file)
# outputs (returns): test results as a list

#' Unique Observation Test
#'
#' @param df
#' @param p
#'
#' @return
#' @export
#'
#' @examples
unique_observation <- function(df, p){

  grouping_cols <- p$values

  # The test depends on observatory code being present
  # If not, all values fail
  if(!all(grouping_cols %in% colnames(df))){

    # Not yet sure what to do here

  } else {

    invalid_results <- df %>%
      dplyr::count(dplyr::across(tidyselect::all_of(grouping_cols))) %>%
      dplyr::filter(n > 1)

    evaluation <- dplyr::left_join(df, invalid_results, by = grouping_cols) %>%
      dplyr::mutate(result = dplyr::case_when(
        !is.na(n) ~ -3,
        T ~ 0
      )) %>%
      dplyr::pull(result)

    # The return vector is the name of the final grouping variable
    # This should possibly be altered to a vector for each grouping var
    result <- setNames(
      list(evaluation),
      grouping_cols[length(grouping_cols)]
    )

    return(result)

  }

}
