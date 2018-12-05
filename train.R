library(mhealthtools)
library(synapser)
library(keras)
library(dplyr)

TREMOR_TABLE <- "syn10676309"
ASSAY_COLUMN <- "deviceMotion_tremor_handAtShoulderLength_right.json.items"

fetch_data <- function() {
  tremor_table <- synTableQuery(paste("SELECT * FROM", TREMOR_TABLE,
                                      "WHERE (dataGroups = 'parkinson'",
                                      "OR dataGroups = 'control') LIMIT 10"))
  data_paths <- synDownloadTableColumns(tremor_table, ASSAY_COLUMN)
  data_files <- purrr::map(data_paths, flatten_data, metric = "userAcceleration")
  data_tibble <- tibble(!!ASSAY_COLUMN := as.integer(names(data_files)),
                        data = data_files)
  tremor_df <- tremor_table$asDataFrame() %>% 
    select(SourceTableID, recordId, healthCode, dataGroups, !!ASSAY_COLUMN) %>% 
    left_join(data_tibble) %>% 
    as_tibble()
  return(tremor_df)
}

train_test_split <- function(tremor_df) {
  
}

flatten_data <- function(path, metric) {
  dat <- jsonlite::fromJSON(path)
  dat <- dat %>% 
    dplyr::select(timestamp, metric) %>% 
    jsonlite::flatten()
  names(dat) <- c("t", "x", "y", "z")
  return(tibble::as_tibble(dat))
}

main <- function()
  synLogin() 
  tremor_df <- fetch_data()
  train_test_data <- train_test_split(tremor_df)
  
main()