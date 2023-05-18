
load_sample_event_data <- function(directory){

  # creates tibble that includes filepath, and split of protocol, table, and filename
  file_index <- tibble(
    filepath = list.files(directory, full.names = T, recursive = T)) %>%
    mutate(directory = gsub(directory, "", filepath)) %>%
    # separates based on folder names (that are in protocol/table/.csv format)
    separate(directory, sep = "/", into = c("drop", "protocol", "table", "filename"), remove = F) %>%
    select(-drop)

  # gathers names of all protocols
  protocols <- unique(file_index$protocol)

  # for every protocol...
  df_list <- lapply(protocols, function(x){

    # gathers names of all tables
    tables <- file_index %>%
      filter(protocol == x) %>%
      count(table) %>%
      pull(table)

    # for every table...
    table_list <- lapply(tables, function(y){

      # gathers file path of y table
      filepaths <- file_index %>%
        filter(protocol == x, table == y) %>%
        pull(filepath)

      # reads in .csv and adds it to table_list
      bind_rows(
        lapply(filepaths, function(z){
          fread(z)
        })
      )
    })

    # rename table list
    names(table_list) <- tables

    table_list
  })

  # rename protocol list
  names(df_list) <- protocols

  return(df_list)

}
