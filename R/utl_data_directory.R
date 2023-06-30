#' Title
#'
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
inventory_L2_data <- function(directory){

  inventory <- tibble(relative_path = list.files(directory, recursive = T),
                  path = list.files(directory, recursive = T, full.names = T)) %>%
    separate(relative_path, into = c("protocol", "table", "filename"), sep = "/")

  return(inventory)
}

write_data_directory_to_L3 <- function(destination, file_inventory){

  files_to_copy <- file_inventory %>%
    filter(!is.na(filename))

  for(i in 1:nrow(files_to_copy)){

    protocol <- files_to_copy$protocol[i]
    table <- files_to_copy$table[i]
    filepath <- files_to_copy$path[i]
    filename <- files_to_copy$filename[i]

    data <- data.table::fread(filepath)

    # First test if protocol and table exist
    # is_table() returns T or F
    if(is_table(protocol, table)){
      # Then make sure column order is accurate
      column_order <- marinegeo.data::get_column_order(protocol, table)

      if(all(colnames(data) == column_order)){
        destination_filepath <- paste0(destination, "/", protocol, "/", table,
                                       "/", filename)

        print(destination_filepath)

        # if(file.exists(destination_filepath)){
        #   file.remove(destination_filepath)
        # }
        #
        # file.copy(
        #   from = filepath,
        #   to = destination_filepath
        # )

      } else {

        print(
          paste0(protocol, "-", table, " has an invalid column structure")
        )
      }

    } else {
      print(paste0(protocol, "-", table, " is not a defined table"))
    }
  }
}
