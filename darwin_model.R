########################################################################
# #testzone
# Purpose: To train Darwin Hybrid-grafting inspired model on mobile
#          sensor data Case 1: Rest Tremor in PD, data sourced from mPower
#          by Sage Bionetworks. < TRAIN >
# Author: Meghasyam Tummalacherla 
# ******Code adapted from previous work by Phil Snyder******
# email: meghasyam@sagebase.org, phil.snyder@sagebase.org
########################################################################
rm(list = ls())
gc()
library(tidyverse)
library(keras)
install_keras()
set.seed(1234567890)

#######################################
# Load train and test meta data, required functions
#######################################
load('darwin.RData')
source('darwin_functions.R')

## TRAIN DATA
train.data.X <- apply(train.tbl.meta, 1, function(x){
  left.data <- tryCatch({
    jsonlite::fromJSON(as.character(x['deviceMotion_tremor_handInLap_left.json.items_filePath'] %>% 
                                      unlist())) %>% 
      processRawData()
  },error = function(T){return(NULL)})
  
  right.data <- tryCatch({
    jsonlite::fromJSON(as.character(x['deviceMotion_tremor_handInLap_right.json.items_filePath'] %>% 
                                      unlist())) %>% 
      processRawData()
  },error = function(T){return(NULL)})
  
  participant.data <- participantModifiedData(left.data, right.data)
  
  return(participant.data %>% as.data.frame())
})

train.data.X <- lapply(train.data.X, function(x){
  return(x %>% simplify2array())
}) %>% 
  simplify2array() %>% 
  keras::array_reshape(c(dim(.)[3], dim(.)[1], dim(.)[2]))

train.data.Y <- train.tbl.meta %>% 
  dplyr::left_join(train.hc) %>% 
  dplyr::select(healthCode, PD)
train.data.Y <- as.numeric(train.data.Y$PD) - 1  
# 1 means yes they have PD, 0 means they are a control

## TEST DATA
test.data.X <- apply(test.tbl.meta, 1, function(x){
  left.data <- tryCatch({
    jsonlite::fromJSON(as.character(x['deviceMotion_tremor_handInLap_left.json.items_filePath'] %>% 
                                      unlist())) %>% 
      processRawData()
  },error = function(T){return(NULL)})
  
  right.data <- tryCatch({
    jsonlite::fromJSON(as.character(x['deviceMotion_tremor_handInLap_right.json.items_filePath'] %>% 
                                      unlist())) %>% 
      processRawData()
  },error = function(T){return(NULL)})
  
  participant.data <- participantData(left.data, right.data)
  
  return(participant.data %>% as.data.frame())
})

test.data.X <- lapply(test.data.X, function(x){
  return(x %>% simplify2array())
}) %>% 
  simplify2array() %>% 
  keras::array_reshape(c(dim(.)[3], dim(.)[1], dim(.)[2]))

test.data.Y <- test.tbl.meta %>% 
  dplyr::left_join(test.hc) %>% 
  dplyr::select(healthCode, PD)
test.data.Y <- as.numeric(test.data.Y$PD) - 1  
# 1 means yes they have PD, 0 means they are a control

#######################################
# Build the keras model architechture
#######################################
# # Baseline sequential model
# 
# model <- keras::keras_model_sequential() %>% 
#   keras::layer_flatten(input_shape = c(2000,6)) %>% 
#   keras::layer_dense(units = 128, activation = 'relu') %>%
#   keras::layer_dense(units = 64, activation = 'relu') %>% 
#   keras::layer_dense(units = 64, activation = 'relu') %>% 
#   keras::layer_dense(units = 32, activation = 'relu') %>% 
#   keras::layer_dense(units = 1, activation = 'sigmoid')
# 
# model %>% keras::compile(
#   optimizer = 'rmsprop',
#   loss = 'binary_crossentropy',
#   metrics = c('acc')
# )
# 
# history <- model %>% keras::fit(
#   train.data.X, train.data.Y,
#   epochs = 10,
#   batch_size = 128,
#   validation_split = 0.2
# )
# 
# # Evaluate on test data to see accuracy
# results <- model %>% keras::evaluate(test.data.X, test.data.Y)
# results

# Updated Baseline sequential model

model <- keras::keras_model_sequential() %>% 
  keras::layer_flatten(input_shape = c(2000,6)) %>% 
  keras::layer_dense(units = 128, activation = 'relu') %>%
  keras::layer_dense(units = 64, activation = 'relu') %>% 
  keras::layer_dropout(rate = 0.25) %>% 
  keras::layer_dense(units = 64, activation = 'relu') %>%
  keras::layer_dropout(rate = 0.2) %>% 
  keras::layer_dense(units = 32, activation = 'relu') %>%
  keras::layer_dense(units = 1, activation = 'sigmoid')

model %>% keras::compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = c('acc')
)

history <- model %>% keras::fit(
  train.data.X, train.data.Y,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)

# Evaluate on test data to see accuracy
results <- model %>% keras::evaluate(test.data.X, test.data.Y)
results

keras::save_model_hdf5(model, filepath = 'updated_baseline.hd5')


# GET AUC FROM PREDICTIONS
library(pROC)
predictions <- keras::predict_classes(model, train.data.X) %>% 
  as.numeric()
roc_obj <- pROC::roc(train.data.Y, predictions)
pROC::auc(roc_obj)
