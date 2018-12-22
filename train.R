library(mhealthtools)
library(keras)
library(dplyr)
library(reticulate)
library(PRROC)

TREMOR_TABLE <- "syn10676309"
ASSAY_COLUMN <- "deviceMotion_tremor_handAtShoulderLength_right.json.items"
TRAINING_HEALTHCODES <- "syn12292452"
TESTING_HEALTHCODES <- "syn12576772"
VIRTUALENV_DIR <- "~/.virtualenvs/r-tensorflow"
DATASET_SIZE = 5000
AUGMENTED_TRAIN_SIZE = 10000

setup_virtualenv <- function() {
  if (dir.exists(VIRTUALENV_DIR)) {
    use_virtualenv(VIRTUALENV_DIR)
  } else {
    install_keras(extra_packages = c("synapseclient", "pandas"))
    use_virtualenv(VIRTUALENV_DIR)
  }
}

syn_login <- function() {
  synapseclient <- import("synapseclient")
  syn <- synapseclient$login()
  return(syn)
}

fetch_data <- function(syn, train_test_hc) {
  healthcode_str <- paste0(
    "('", paste(train_test_hc$healthCode, collapse = "','"), "')")
  tremor_table <- syn$tableQuery(paste("SELECT * FROM", TREMOR_TABLE,
                                      "WHERE healthCode IN", healthcode_str,
                                      "AND", paste0('"', ASSAY_COLUMN, '"'),
                                      "IS NOT NULL", "LIMIT", DATASET_SIZE))
  data_paths <- syn$downloadTableColumns(tremor_table, ASSAY_COLUMN)
  data_files <- purrr::map(data_paths, flatten_data, metric = "userAcceleration")
  data_tibble <- tibble(!!ASSAY_COLUMN := as.integer(names(data_files)),
                        data = data_files)
  tremor_df <- tremor_table$asDataFrame() %>% 
    select(SourceTableID, recordId, healthCode, dataGroups, !!ASSAY_COLUMN) %>% 
    filter(dataGroups %in% c("parkinson", "control")) %>% 
    mutate(dataGroups = as.character(dataGroups)) %>% 
    left_join(train_test_hc) %>%
    left_join(data_tibble) %>% 
    as_tibble()
  return(tremor_df)
}

flatten_data <- function(path, metric) {
  dat <- jsonlite::fromJSON(path)
  dat <- dat %>% 
    dplyr::select(timestamp, metric) %>% 
    jsonlite::flatten()
  names(dat) <- c("t", "x", "y", "z")
  return(dplyr::as_tibble(dat))
}

get_train_test_hc <- function(syn) {
  training_hc <- readr::read_tsv(syn$get(TRAINING_HEALTHCODES)$path) %>%
    select(healthCode)
  testing_hc <- readr::read_tsv(syn$get(TESTING_HEALTHCODES)$path) %>%
    select(healthCode)
  train_test_hc <- bind_rows(
    list(train = training_hc, test = testing_hc), .id = "group") %>% 
    sample_n(nrow(.))
  return(train_test_hc)
}

preprocess_data <- function(sensor_data) {
  features <- mhealthtools::accelerometer_features(
    sensor_data = sensor_data,
    time_filter = c(1, 9),
    detrend = T,
    models = function(transformed_sensor_data) {
      group_length <- group_size(transformed_sensor_data)[[1]]
      transformed_sensor_data <- transformed_sensor_data %>% 
        mutate(index = 1:group_length) %>% 
        tidyr::spread(axis, acceleration) %>% 
        select(-index) %>%
        simplify2array()
    })
  return(features$model_features[[1]])
}

pad_data <- function(sensor_data) {
  padding <- matrix(0, 4000 - dim(sensor_data)[1], 3)
  standard_sensor_data <- sensor_data %>%
    as_tibble() %>% 
    purrr::map(~ (. - mean(.)) / sd(.)) %>%
    simplify2array()
  padded_sensor_data <- standard_sensor_data %>%
    rbind(padding) %>%
    keras::array_reshape(c(4000, 3))
  return(padded_sensor_data)
}

bandpass_data <- function(sensor_data) {
  sensor_data <- as_tibble(sensor_data) # for purrr::map / preserve colnames
  bandpassed_sensor_data <- purrr::map(sensor_data, function(d) {
    mhealthtools:::bandpass(
      d,
      window_length = 256,
      sampling_rate = 100,
      frequency_range = c(1, 25))
  }) %>% 
    simplify2array()
  return(bandpassed_sensor_data)
}

random_unit_vector <- function() {
  ru <- list()
  theta <- runif(1, 0, 2*pi)
  ru$z <- runif(1, -1, 1)
  ru$y <- sqrt(1 - ru$z ^ 2) * sin(theta)
  ru$x <- sqrt(1 - ru$z ^ 2) * cos(theta)
  return(ru)
}

add_noise_to_data <- function(sensor_data, stretch_factor = 0.5, noise_factor = 0.1) {
  sensor_data <- as_tibble(sensor_data) # for purrr::map / keep colnames
  noisy_sensor_data <- purrr::map(sensor_data, function(d) {
    sd_d <- sd(d)
    d * (sample(c(-1, 1), 1) + (sample(c(-1, 1), 1) * sd_d * stretch_factor)) +
      rnorm(length(d), sd = noise_factor * sd_d)
  })
  noisy_sensor_data <- simplify2array(noisy_sensor_data)
  ru <- random_unit_vector()
  theta <- runif(1, -pi, pi)
  rotated_noisy_sensor_data <- rgl::rotate3d(
    noisy_sensor_data, angle = theta, x = ru$x, y = ru$y, z = ru$z)
  return(rotated_noisy_sensor_data)
}

oversample_data <- function(tremor_df, num_samples = 1000, add_noise = TRUE) {
  nrow_tremor_df <- nrow(tremor_df)
  tremor_df_oversampled <- purrr::map_dfr(1:num_samples, function(i) {
    index <- i %% nrow_tremor_df
    index <- if_else(index == 0, nrow_tremor_df, index)
    df_row <- tremor_df[index,] %>% 
      mutate(sample_batch = ceiling(i / nrow_tremor_df))
    df_row$data[[1]] <- add_noise_to_data(df_row$data[[1]])
    return(df_row)
  })
  return(tremor_df_oversampled)
}

balance_parkinson_control <- function(tremor_df, min_samples = 1000) {
  num_parkinson <- sum(tremor_df$dataGroups == "parkinson")
  num_control <- sum(tremor_df$dataGroups == "control")
  if (num_parkinson == 0 || num_control == 0) {
    stop("The dataset contains only a single group (one of parkinson or control).")
  }
  min_samples <- max(nrow(tremor_df) + abs(num_parkinson - num_control),
                     min_samples)
  additional_parkinson <- max(0, ceiling(0.5 * min_samples - num_parkinson))
  additional_control <- max(0, ceiling(0.5 * min_samples - num_control))
  if (additional_parkinson > 0) {
    tremor_df_augmented <- oversample_data(
      tremor_df %>% filter(dataGroups == "parkinson"),
      num_samples = additional_parkinson)
    tremor_df <- bind_rows(tremor_df, tremor_df_augmented)
  }
  if (additional_control > 0) {
    tremor_df_augmented <- oversample_data(
      tremor_df %>% filter(dataGroups == "control"),
      num_samples = additional_control)
    tremor_df <- bind_rows(tremor_df, tremor_df_augmented)
  }
  tremor_df <- sample_n(tremor_df, size = nrow(tremor_df))
  return(tremor_df)
}

train_network <- function(tremor_df) {
  training <- tremor_df %>% filter(group == "train")
  testing <- tremor_df %>% filter(group == "test")
  train_X <- training$data %>% 
    simplify2array() %>% 
    array_reshape(c(dim(.)[3], 4000, 3))
  test_X <- testing$data %>%
    simplify2array() %>% 
    array_reshape(c(dim(.)[3], 4000, 3))
  train_y <- training$dataGroups %>%
    recode(control = 0, parkinson = 1)
  test_y <- testing$dataGroups %>%
    recode(control = 0, parkinson = 1)
  model <- mhealthtools:::guanlab_nn_architecture() %>%
    compile(loss = loss_binary_crossentropy,
            optimizer = optimizer_sgd(momentum = 0.9))
  history <- model %>%
    fit(train_X, train_y,
        epochs = 10,
        batch_size = 128,
        validation_split = 0.2)
}

main <- function() {
  setup_virtualenv()
  syn <- syn_login()
  train_test_hc <- get_train_test_hc(syn)
  tremor_df <- fetch_data(syn, train_test_hc) %>% 
    mutate(data = purrr::map(.$data, preprocess_data))
  tremor_train_augmented <- tremor_df %>%
    filter(group == "train") %>%
    balance_parkinson_control(min_samples = AUGMENTED_TRAIN_SIZE)
  tremor_df_balanced <- bind_rows(tremor_df %>% filter(group == "test"),
                                  tremor_train_augmented) %>% 
    mutate(data = purrr::map(.$data, bandpass_data),
           data = purrr::map(.$data, pad_data))
}
  
#main()