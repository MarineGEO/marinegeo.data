# protocol
#   info
#   tables
#     table a
#       info
#       columns
#         a
#           test a
#           test b
#         b

# library(data.table)
# library(tidyverse)
# library(jsonlite)

create_schema <- function(){

  empty_schema <- create_schema_list("./inst/marinegeo_data_structure/")
  schema_table_tests <- populate_table_parameters(empty_schema)
  output <- populate_column_parameters(schema_table_tests)
  knowledge_hub_list <- create_knowledge_hub_list()
  protocol_grouping_list <- protocol_grouping_list()

  schema <- c(output, list(knowledge_hub = knowledge_hub_list,
                           protocol_grouping = protocol_grouping_list))

  write_schema(schema)

  return(schema)
}

write_schema <- function(schema){

  saveRDS(schema, "./inst/marinegeo_schema/marinegeo_schema.rds")

  write_json(schema,
             pretty = T,
             auto_unbox = T,
             path = "./inst/marinegeo_schema/marinegeo_schema.json")

}

create_schema_list <- function(filepath_to_data_structure){

  # Load CSV data structure files
  data_structure <- read_csv(list.files(
    filepath_to_data_structure,
    full.names = T
  ), show_col_types = F)

  # Create index of all protocol-tables
  protocol_index <- data_structure %>%
    select(protocol, table) %>%
    distinct()

  protocols <- protocol_index %>%
    count(protocol) %>%
    pull(protocol)

  empty_schema <-
    # Protocol Level
    setNames(
      lapply(protocols, function(protocol){

        # print(protocol)

        protocol_tables <- protocol_index %>%
          filter(protocol == !!protocol) %>%
          count(table) %>%
          pull(table)

        # "Tables" level
        setNames(
          list(

            # Protocol Table Level
            setNames(
              lapply(protocol_tables, function(table){

                table_columns <- data_structure %>%
                  filter(protocol == !!protocol,
                         table == !!table,
                         level == 2) %>%
                  pull(column_name)

                # "Columns" Level
                setNames(
                  list(

                    # Table Columns Level
                    setNames(
                      lapply(table_columns, function(column){


                      }),

                      table_columns
                    )

                  ),

                  "columns"
                )

              }),
              protocol_tables
            )
          ),
          "tables"
        )

      }), protocols)

  return(empty_schema)
}

populate_table_parameters <- function(schema){

  # Load QC parameters CSVs to list
  directory_path <- "./inst/csv_qaqc_parameters/table_tests/"
  test_names_dirs <- list.dirs(directory_path)[list.dirs(directory_path) != directory_path]
  test_names <- basename(test_names_dirs)

  param_list <- lapply(test_names_dirs, function(directory){

    read_csv(list.files(
      directory, full.names = T
    ), show_col_types = F)

  })

  names(param_list) <- test_names

  # Create dataframe of protocol - table - tests
  # This will be iterated on to fill out parameters in schema
  table_inventory <- bind_rows(
    lapply(param_list, function(df){

      df %>%
        select(protocol, table, test) %>%
        distinct()

    })
  )

  protocols <- table_inventory %>%
    count(protocol) %>%
    pull(protocol)

  for(protocol in protocols){

    protocol_df <- table_inventory %>%
      filter(protocol == !!protocol)

    tables <- protocol_df %>%
      count(table) %>%
      pull(table)

    for(table in tables){

      tests <- protocol_df %>%
        filter(table == !!table) %>%
        count(test) %>%
        pull(test)

      table_list <- lapply(tests, function(test){

        result <- param_list[[test]] %>%
          filter(protocol == !!protocol,
                 table == !!table)

        if(test == "unique_observation"){

          list(
            values = result$column_name
          )

        } else {
          NULL
        }

      })

      names(table_list) <- tests
      schema[[protocol]]$tables[[table]][["tests"]] <- table_list

    }

  }

  return(schema)

}

populate_column_parameters <- function(schema){

  # Load QC parameters CSVs to list
  directory_path <- "./inst/csv_qaqc_parameters/column_tests/"
  test_names_dirs <- list.dirs(directory_path)[list.dirs(directory_path) != directory_path]
  test_names <- basename(test_names_dirs)

  param_list <- lapply(test_names_dirs, function(directory){

    read_csv(list.files(
      directory, full.names = T
    ), show_col_types = F)

  })

  names(param_list) <- test_names

  # Create dataframe of protocol - table - tests
  # This will be iterated on to fill out parameters in schema
  column_inventory <- bind_rows(
    lapply(param_list, function(df){

      df %>%
        select(protocol, table, column_name, test) %>%
        distinct()

    })
  )

  protocols <- column_inventory %>%
    count(protocol) %>%
    pull(protocol)

  for(protocol in protocols){

    protocol_df <- column_inventory %>%
      filter(protocol == !!protocol)

    tables <- protocol_df %>%
      count(table) %>%
      pull(table)

    for(table in tables){

      columns <- protocol_df %>%
        filter(table == !!table) %>%
        count(column_name) %>%
        pull(column_name)

      for(column_name in columns){

        tests <- protocol_df %>%
          filter(table == !!table,
                 column_name == !!column_name) %>%
          count(test) %>%
          pull(test)

        column_list <- lapply(tests, function(test){

          result <- param_list[[test]] %>%
            filter(protocol == !!protocol,
                   table == !!table,
                   column_name == !!column_name)

          if(test == "categorical_values"){

            list(
              values = result$value
            )

          } else if(test == "missing_values"){

            list(value_required = TRUE)

          } else if(test == "integer_range"){

            list(fail_range = c(result$minimum, result$maximum))

          } else if(test == "numeric_range"){

            list(
              fail_range = c(result$minimum, result$maximum),
              type = result$type
            )

          } else {
            NULL
          }

        })

        # browser()

        names(column_list) <- tests
        # schema[[protocol]]$tables[[table]]$columns[[column_name]] <- column_list
        schema[[protocol]]$tables[[table]]$columns[[column_name]][["tests"]] <- column_list
      }

    }

  }

  return(schema)
}



create_knowledge_hub_list <- function(){

  knowledge_hub_list <- list(

    categorical_values = list(
      observatory_code = list(
        values = read_csv(list.files(
          "./inst/knowledge_hub/observatories/", full.names = T
        ), show_col_types = F)$observatory_code
      ),

      scientific_name = list(
        values = unique(
          read_csv(list.files(
            "./inst/knowledge_hub/taxonomy/", full.names = T
          ), show_col_types = F)$scientific_name
        )
      ),

      taxonomic_id = list(
        values = unique(
          read_csv(list.files(
            "./inst/knowledge_hub/taxonomy/", full.names = T
          ), show_col_types = F)$taxonomic_id
        )
      )
    ),

    location_names = list(
      location_name = list(
        values = read_csv(list.files(
          "./inst/knowledge_hub/sampling_locations/", full.names = T
        ), show_col_types = F) %>%
          select(-habitat)
      )
    )

  )

  return(knowledge_hub_list)
}

protocol_grouping_list <- function(){

  protocol_grouping <- list(

    seagrass = list(

      tests = list(
        shared_transect = list(
          protocols = c("seagrass-density", "seagrass-shoots", "seagrass-epifauna", "seagrass-macroalgae", "seagrass-biomass"),
          columns = c("sample_event_id", "observatory_code","sample_collection_date", "location_name", "transect")
        ),

        transect_dates = list(
          protocols = c("seagrass-density", "seagrass-shoots", "seagrass-epifauna", "seagrass-macroalgae", "seagrass-biomass"),
          columns = c("sample_event_id", "observatory_code", "location_name", "transect"),
          date_column = "sample_collection_date"
        )

        # mandatory_protocols = list(
        #   exclusive_protocols = c(),
        #   shared_protocols = c()
        # ),
        #
        # optional_protocols = list(
        #   exclusive_protocols = c(),
        #   shared_protocols = c()
        # )

      )
    )
  )
}
