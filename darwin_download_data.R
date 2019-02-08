########################################################################
# #testzone
# Purpose: To train Darwin Hybrid-grafting inspired model on mobile
#          sensor data Case 1: Rest Tremor in PD, data sourced from mPower
#          by Sage Bionetworks. < GENERATE TRAIN AND TEST DATA >
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

#######################################
# Download Synapse Table, and select and download required columns, figure out filepath locations
#######################################
synapser::synLogin()

tremor.tbl.id <- 'syn10676309'
all.used.ids <- tremor.tbl.id

rest.train.hc <- 'syn12292451'
rest.test.hc <- 'syn12576772'

tremor.tbl.syn <- synapser::synTableQuery(paste('select * from', tremor.tbl.id))
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

save(list = c('train.tbl.meta', 'test.tbl.meta', 'train.hc', 'test.hc'), file = 'darwin.RData')
