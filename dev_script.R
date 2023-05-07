## Test QAQC ####

library(devtools)

library(marinegeo.data)

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(data.table)
library(jsonlite)

load_all()

# cover
df <- fread("./inst/USA-IRL_seagrass_cover-sample-data_2022.csv")
protocol <- "seagrass-density"
table <- "cover-sample-data"

# macrophyte mass
df <- fread("./inst/USA-IRL_seagrass_macrophyte-mass-data.csv")
protocol <- "seagrass-epifauna"
table <- "macrophyte-mass-data"

output <- run_qc_on_table(df, protocol, table)

summary <- create_quality_control_summary(output)


## Write schema ####
library(devtools)

library(data.table)
library(tidyverse)
library(jsonlite)

load_all()

empty_schema <- create_schema_list("./inst/marinegeo_data_structure/")
schema_table_tests <- populate_table_parameters(empty_schema)
output <- populate_column_parameters(schema_table_tests)
knowledge_hub_list <- create_knowledge_hub_list()
write_schema_json(output, knowledge_hub_list)

# Not in a function:
marinegeo_schema <- c(output, list(knowledge_hub = knowledge_hub_list))
usethis::use_data(marinegeo_schema, internal = TRUE)
