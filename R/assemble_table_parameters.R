#' Assemble Table Parameters
#'
#' @param parameters
#' @param protocol
#' @param table
#'
#' @return
#' @export
#'
#' @examples
assemble_table_parameters <- function(parameters, protocol, table){

  table_parameters <- parameters[[protocol]]$tables[[table]]

  if("observatory_code" %in% names(table_parameters$columns)){

    table_parameters$columns$observatory_code[["tests"]] <- list(
      "categorical_values" = parameters$knowledge_hub$categorical_values$observatory_code
    )

  }

  if("scientific_name" %in% names(table_parameters$columns)){

    table_parameters$columns$scientific_name[["tests"]] <- list(
      "categorical_values" = parameters$knowledge_hub$categorical_values$scientific_name
    )

  }

  if("taxonomic_id" %in% names(table_parameters$columns)){

    table_parameters$columns$taxonomic_id[["tests"]] <- list(
      "categorical_values" = parameters$knowledge_hub$categorical_values$taxonomic_id
    )

  }

  if("location_name" %in% names(table_parameters$columns)){

    table_parameters$columns$location_name[["tests"]] <- list(
      "location_names" = parameters$knowledge_hub$location_names$location_name
    )

  }

  return(table_parameters)
}
