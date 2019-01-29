########################################################################
# #testzone
# Purpose: To train Darwin Hybrid-grafting inspired model on mobile
#          sensor data Case 1: Rest Tremor in PD, data sourced from mPower
#          by Sage Bionetworks.
# Author: Meghasyam Tummalacherla 
# ******Code adapted from previous work by Phil Snyder******
# email: meghasyam@sagebase.org, phil.snyder@sagebase.org
########################################################################
rm(list = ls())
gc()
devtools::install_github("Sage-Bionetworks/mhealthtools")

##############
# Required libraries
##############
library(mhealthtools)
library(synapser)
library(githubr)
library(dplyr)
library(tidyverse)
library(rgl)


##############
# Required functions
##############

## Process the RAW sample sensor data to get it into list of dataframes
processRawData <- function(rawSensorData){
  # Read the Json File and process it into mhealthtools tidy format
  
  sensorData <-   tryCatch({
    sensor_data <- rawSensorData
    
    accel_data <- sensor_data$userAcceleration
    accel_data$t <- sensor_data$timestamp - 
      min(sensor_data$timestamp, na.rm = T)
    
    gyro_data <- sensor_data$rotationRate
    gyro_data$t <- accel_data$t
    
    # grav_data <- sensor_data$gravity
    # grav_data$t <- accel_data$t
    # We are not considering the gravity data in the first run of 
    # this model
    
    
    sensor_data <- list(accelerometer = accel_data,
                        gyroscope = gyro_data)
  }, error = function(err) {
    sensor_data <- list(accelerometer = NA,
                        gyroscope = NA)
    # NAs are handled in mhealthtools
  })
}

## To transform the output that has the tidy format (like the output of
## processRawData) into one single dataframe with column names
## t,xa,ya,za,xg,yg,zg
meltSensorData <- function(sensor_data){
  mod_data <- tryCatch({
    modified_data_acc <- sensor_data$accelerometer %>% 
      dplyr::select(t, xa = x, ya = y, za = z) 
    modified_data_gyro <- sensor_data$gyroscope %>% 
      dplyr::select(xg = x, yg = y, zg = z)
    modified_data <- cbind(modified_data_acc, modified_data_gyro)
    return(modified_data)
  },
  error = function(e){
    return(sensor_data)
  })
  return(mod_data)
}


## To Scale and add normal noise with standard deviation which is noise_magnitude*sd
## of the data in the column, to a sensor data containing columns x,y,z
scaleAndNoise <- function(sensor_data, scaling = 1, noise_magnitude = 0.1){
  modified_data <- sensor_data %>% dplyr::select(x,y,z) %>% 
    purrr::map(function(d) {
      sd_d <- sd(d)
      d <- d*scaling+
        rnorm(length(d), sd = noise_magnitude * sd_d)
    }) %>% as.data.frame()
  modified_data$t <- sensor_data$t
  return(modified_data)
}

## To rotate the given sensor data (accelerometer or gyroscope)
## in 3D and generate new modified data
hybridize <- function(sensor_data, vec = NULL, theta = NULL){
  if(is.null(vec)){
    vec <- random_unit_vector()
  }
  if(is.null(theta)){
    theta <- runif(1, 0, 2*pi)
  }
  
  xyz <- sensor_data %>% dplyr::select(x,y,z) %>% simplify2array()
  xyz <- rgl::rotate3d(xyz, angle = theta, 
                       x = vec$x,y = vec$y, z = vec$z)
  xyz <- as.data.frame(xyz) %>% `colnames<-`(c('x','y','z'))
  xyz$t <- sensor_data$t
  return(xyz)  
}

## To graft (cut, rotate and add back) the given sensor data 
## (accelerometer or gyroscope) in 3D and generate new modified data
graft <- function(sensor_data, graft_range = c(0.4,0.6),
                  vec_graft = NULL, theta_graft= NULL){
  if(is.null(vec_graft)){
    vec_graft <- random_unit_vector()
  }
  if(is.null(theta_graft)){
    theta_graft <- runif(1, 0, 2*pi)
  }
  
  graft_split_time <- round(
    runif(1, min = graft_range[1], max = graft_range[2]) * length(sensor_data$t))
  graft_split_time <- sensor_data$t[graft_split_time]
  
  parent_data <- sensor_data %>% dplyr::filter(t < graft_split_time)
  graft_data <- sensor_data %>% dplyr::filter(t >= graft_split_time) %>% 
    hybridize(vec = vec_graft, theta = theta_graft)
  
  return(rbind(parent_data, graft_data))
  
}

## Reset the t column in the data frame, so that the first element is 0
## and NAs are removed
resetTimestamp <- function(sensor_data, na.omit = T){
  if(na.omit){
    sensor_data <- sensor_data %>% na.omit()
  }
  sensor_data$t <- sensor_data$t - min(sensor_data$t, na.rm = T)
  return(sensor_data)
}

## Filter a given sensor data in time and frequency
filterSensorData <- function(sensor_data, time_range = c(2,9),
                             freq_range = c(0.5, 20)){
  
  fs <- (length(sensor_data$t))/(max(sensor_data$t, na.rm = T)-min(sensor_data$t, na.rm = T))
  fs <- fs/2
  #NOTE:: This is actually half the sampling rate
  bandpass_params <- signal::ellipord(Wp = c(freq_range[1]/fs,freq_range[2]/fs), 
                                      Ws = c((freq_range[1]-0.2)/fs, (freq_range[2] + 2)/fs),
                                      Rp = 0.001,
                                      Rs = 0.001)
  bandpass_filter <- suppressWarnings(signal::ellip(bandpass_params))
  
  sensor_data$x <- signal::filter(bandpass_filter, sensor_data$x) %>% as.numeric()
  sensor_data$y <- signal::filter(bandpass_filter, sensor_data$y) %>% as.numeric()
  sensor_data$z <- signal::filter(bandpass_filter, sensor_data$z) %>% as.numeric()
  
  sensor_data <- sensor_data %>% dplyr::filter(t >= time_range[1],
                                               t <= time_range[2])
  return(sensor_data)
}

## Pad data, pad/slice given sensor data to 1000 (or specified) number of samples 

padData <- function(sensor_data, n = 1000){
  if(n > nrow(sensor_data)){
    padding <- matrix(0, n - nrow(sensor_data), ncol(sensor_data)) %>% 
      as.data.frame() %>% 
      `colnames<-`(colnames(sensor_data))
    sensor_data <- rbind(sensor_data, padding)
  }else{
    sensor_data <- sensor_data[1:n, ]
  }
  return(sensor_data)
}

## Plot sensor data with columns x,y,z,t
plotSensorData <- function(sensor_data){
  par(mfrow = c(3,1))
  plot(sensor_data$t, sensor_data$x, type = 'l')
  plot(sensor_data$t, sensor_data$y, type = 'l')
  plot(sensor_data$t, sensor_data$z, type = 'l')
}

## Generate a random unit vector in 3D
random_unit_vector <- function() {
  ru <- list()
  theta <- runif(1, 0, 2*pi)
  ru$z <- runif(1, -1, 1)
  ru$y <- sqrt(1 - ru$z ^ 2) * sin(theta)
  ru$x <- sqrt(1 - ru$z ^ 2) * cos(theta)
  return(ru)
}

## Apply the concept of Hybridization and grafting onto the sensor data
## including accelerometer and gyroscope as a whole
signal_darwin <- function(sensor_data, hybridization = 0.8,
                          grafting = 0.2, grafting_range = c(0.4,0.6),
                          scale_thresholds = c(0.8, 1.2),
                          time_range = c(2,10), freq_range = c(0.5,20),
                          noise_thresholds = c(0, 0.2),
                          signal_length = 1000){
  # sensor_data: a list with two dataframes namely accelerometer and
  # gyroscope, i.e the output of preprocessSensorData
  
  hybridization.tag <- (runif(1,min = 0, max = 1) <= hybridization)
  grafting.tag <- (runif(1,min = 0, max = 1) <= grafting)
  
  modified_data <- tryCatch({
    random_unit_vec <- random_unit_vector()
    random_theta <- runif(1, min = 0, max = 2*pi)
    scale_acc <- runif(1, min = scale_thresholds[1], max = scale_thresholds[2])
    noise_acc <- runif(1, min = noise_thresholds[1], max = noise_thresholds[2])
    scale_gyro <- runif(1, min = scale_thresholds[1], max = scale_thresholds[2])
    noise_gyro <- runif(1, min = noise_thresholds[1], max = noise_thresholds[2])
    
    # Hybridization
    if(hybridization.tag){
      sensor_data$accelerometer <- sensor_data$accelerometer %>%
        hybridize(vec = random_unit_vec, theta = random_theta)
      
      sensor_data$gyroscope <- sensor_data$gyroscope %>%
        hybridize(vec = random_unit_vec, theta = random_theta)
    }
    
    # Grafting
    if(grafting.tag){
      sensor_data$accelerometer <- sensor_data$accelerometer %>%
        graft(vec_graft = random_unit_vec, theta_graft = random_theta)
      
      sensor_data$gyroscope <- sensor_data$gyroscope %>%
        graft(vec_graft = random_unit_vec, theta_graft = random_theta)
    }
    
    # Filtering
    sensor_data$accelerometer <- filterSensorData(sensor_data$accelerometer,
                                                  time_range = time_range, 
                                                  freq_range = freq_range)
    sensor_data$gyroscope <- filterSensorData(sensor_data$gyroscope,
                                              time_range = time_range, 
                                              freq_range = freq_range)
    
    # Scale and noise
    sensor_data$accelerometer <- scaleAndNoise(sensor_data$accelerometer,
                                               scaling = scale_acc,
                                               noise_magnitude = noise_acc)
    sensor_data$gyroscope <- scaleAndNoise(sensor_data$gyroscope,
                                           scaling = scale_gyro, 
                                           noise_magnitude = noise_gyro)
    
    # Reset timestamp and make all data uniform of length 1000 samples (x,y,t,z)
    sensor_data$accelerometer <- resetTimestamp(sensor_data$accelerometer)
    sensor_data$gyroscope <- resetTimestamp(sensor_data$gyroscope)
    
    # Pad data to specified pad length
    sensor_data$accelerometer <- padData(sensor_data$accelerometer,
                                         n = signal_length)
    sensor_data$gyroscope <- padData(sensor_data$gyroscope,
                                     n = signal_length)
    
    return(sensor_data)
  },
  error = function(T){return(sensor_data)})
  
  return(modified_data)
}

## Give transformed data for a given record (given left and right hand files)
participantModifiedData <- function(left.data = NULL, right.data = NULL){
  # left.data is output of processRawData
  # right.data is output of processRawData
  modified.data <- matrix(0, nrow = 2000, ncol = 6)
  
  # Repeat right hand measurement as left, if left hand data is missing
  if(is.null(left.data)){
    left.data <- right.data
  }
  # Repeat left hand measurement as right, if right hand data is missing
  if(is.null(right.data)){
    right.data <- left.data
  }
  
  if(!is.null(left.data) && !is.null(right.data)){
    
    left_data_mod <- signal_darwin(left.data) %>%
      meltSensorData()
    right_data_mod <- signal_darwin(right.data) %>% 
      meltSensorData()
    
    flip.tag <- (runif(1, min = 0, max = 1) <= 0.5)
    # To flip left and right hand records, some people might have 
    # tremor in left and some in right, if that is not the case we 
    if(flip.tag){
      first_half <- left_data_mod
      second_half <- right_data_mod
    }else{
      first_half <- right_data_mod
      second_half <- left_data_mod
    }
    
    modified.data <- rbind(first_half, second_half) %>% 
      dplyr::select(xa,ya,za,xg,yg,zg) %>% 
      simplify2array()
  }
  
  return(modified.data)
}

#######################################
# Download Synapse Table, and select and download required columns, figure out filepath locations
#######################################
synapser::synLogin()

tremor.tbl.id <- 'syn10676309'
all.used.ids <- tremor.tbl.id

rest.train.hc <- 'syn12292451'
rest.test.hc <- 'syn12576772'

tremor.tbl.syn <- synapser::synTableQuery(paste('select * from', tremor.tbl.id,' limit 10000'))
tremor.tbl <- tremor.tbl.syn$asDataFrame()

columnsToDownload = c('deviceMotion_tremor_handInLap_right.json.items',
                      'deviceMotion_tremor_handInLap_left.json.items')

tremor.json.loc <- lapply(columnsToDownload, function(col.name){
  tbl.files <- synapser::synDownloadTableColumns(tremor.tbl.syn, col.name) %>% 
    base::as.data.frame() %>% 
    `colnames<-`(c('filePath')) 
  tbl.files$fileId <- as.numeric(rownames(tbl.files))
  tbl.files <- tbl.files %>%
    dplyr::select(fileId, filePath) %>% 
    `colnames<-`(c(col.name, paste0(col.name,'_filePath')))
  return(tbl.files)
}) 

tremor.tbl.meta <- tremor.tbl
for(i in seq(length(tremor.json.loc))){
  tremor.tbl.meta <- tremor.tbl.meta %>% 
    dplyr::left_join(tremor.json.loc[[i]])
}

# Subset to required hc
train.hc <- synapser::synGet(rest.train.hc)$path %>% read.csv(sep = '\t')
test.hc <- synapser::synGet(rest.test.hc)$path %>% read.csv(sep = '\t')

train.tbl.meta <- tremor.tbl.meta %>% 
  dplyr::filter(healthCode %in% train.hc$healthCode) %>% 
  unique()
test.tbl.meta <- tremor.tbl.meta %>% 
  dplyr::filter(healthCode %in% test.hc$healthCode) %>% 
  unique()

#######################################
# TEST-ZONE
#######################################
test.rec <- train.tbl.meta[1,]
left.data <- tryCatch({
  jsonlite::fromJSON(as.character(test.rec['deviceMotion_tremor_handInLap_left.json.items_filePath'] %>% 
                                  unlist())) %>% 
    processRawData()
},error = function(T){return(NULL)})

right.data <- tryCatch({
  jsonlite::fromJSON(as.character(test.rec['deviceMotion_tremor_handInLap_right.json.items_filePath'] %>% 
                                    unlist())) %>% 
    processRawData()
},error = function(T){return(NULL)})

participant.data <- participantModifiedData(left.data, right.data)
