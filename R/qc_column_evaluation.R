
#' Column Evaluation Test
#'
#' @param df
#' @param column_order_list
#'
#' @return
#' @export
#'
#' @examples
column_evaluation <- function(df, column_order_list){

  column_order <- unlist(unname(column_order_list))

  invalid_cols <- colnames(df)[!colnames(df) %in% column_order]
  missing_cols <- column_order[!column_order %in% colnames(df)]

  invalid_list <- setNames(
    lapply(invalid_cols, function(column_name){
      rep(-3, nrow(df))
  }), invalid_cols)

  missing_list <- setNames(
    lapply(missing_cols, function(column_name){
      rep(-2, nrow(df))
    }), missing_cols)

  return(c(invalid_list, missing_list))

}
