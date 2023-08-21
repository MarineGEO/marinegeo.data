#' Get Column Order for table
#'
#' @param protocol
#' @param table
#' @param level
#'
#' @return
#' @export
#'
#' @examples
get_column_order <- function(protocol, table, dataset = "L2_standardized"){

  protocol_data_structure <- marinegeo_schema$protocols[[protocol]]$datasets[[dataset]][[table]]$columns

  column_order <- names(protocol_data_structure)

  return(column_order)
}

#' Title
#'
#' @param protocol
#' @param table
#'
#' @return
#' @export
#'
#' @examples
is_table <- function(protocol, dataset, table){

  if(protocol %in% names(marinegeo_schema$protocols)){

    if(dataset %in% names(marinegeo_schema$protocols[[protocol]]$datasets)){

      if(table %in% names(marinegeo_schema$protocols[[protocol]]$datasets[[dataset]])){
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }

}

#' Check order and missing or invalid columns
#'
#' @param df
#' @param column_order
#'
#' @return
#' @export
#'
#' @examples
validate_data_structure <- function(df, column_order){

  invalid_cols <- colnames(df)[!colnames(df) %in% column_order]
  missing_cols <- column_order[!column_order %in% colnames(df)]

  df_return <- tibble(
    "column" = NA_character_,
    "result" = NA_character_,
    .rows = 0
  )

  if(length(invalid_cols) > 0){

    df_return <- df_return %>%
      add_row(
        column = invalid_cols,
        result = rep("Invalid Column", length(invalid_cols))
      )
  }

  if(length(missing_cols) > 0){

    df_return <- df_return %>%
      add_row(
        column = missing_cols,
        result = rep("Missing Column", length(missing_cols))
      )
  }

  output_list <- list("summary" = df_return %>%
                        mutate(test = "Column Name Validity"),
                      "results" = list(
                        "Invalid Column" = invalid_cols,
                        "Missing Column" = missing_cols
                      ))

  return(output_list)

}
