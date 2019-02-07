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

#######################################
# Load train and test data
#######################################

#######################################
# Build the keras model architechture
#######################################
# Baseline 1D convolution sequential model

model <- keras::keras_model_sequential() %>% 
  keras::layer_flatten(input_shape = c(2000,6)) %>% 
  keras::layer_dense(units = 64, activation = 'relu') %>%
  keras::layer_dense(units = 64, activation = 'relu') %>% 
  keras::layer_dense(units = 32, activation = 'relu') %>% 
  keras::layer_dense(units = 1, activation = 'sigmoid')

model %>% keras::compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = c('auroc')
)

history <- model %>% keras::fit(
  train.data.X, train.data.Y
)