#' Check if coordinates are defined in sample event transect metadata table
#'
#' @param data_list
#' @param p
#'
#' @return
#' @export
#'
#' @examples
coordinates_present <- function(data_list, p){

  #browser()
  if("sample-event-protocol" %in% names(data_list)){

    protocol_groups <- list(
      seagrass = c("seagrass-density", "seagrass-shoots", "seagrass-epifauna", "seagrass-macroalgae", "seagrass-biomass",
                              "sediment-organic-matter"),

      predation_assay = c("predation-assay")
    )

    coordinate_summary <- dplyr::bind_rows(
      purrr::compact(
        setNames(
          lapply(names(protocol_groups), function(protocol_group){

            if(any(names(data_list) %in% unlist(protocol_groups[[protocol_group]]))){

              target_protocols <- data_list[names(data_list) %in% protocol_names]

              coordinates <- data_list[["sample-event-protocol"]][["transect-metadata"]] %>%
                dplyr::filter(protocol_id == protocol_group)

              dplyr::bind_rows(
                lapply(names(target_protocols), function(protocol_name){

                  dplyr::bind_rows(
                    purrr::compact(
                      lapply(names(target_protocols[[protocol_name]]), function(table_name){

                        df <- target_protocols[[protocol_name]][[table_name]] %>%
                          dplyr::select(all_of(c("sample_event_id", "observatory_code", "location_name", "transect"))) %>%
                          dplyr::distinct()

                      })
                    )
                  )
                })
              ) %>%
                dplyr::distinct() %>%
                dplyr::full_join(seagrass_coordinates)

            }
          }), names(protocol_groups))
      )
    )

    return(coordinate_summary)

  } else return("No sample event protocol provided")

}
