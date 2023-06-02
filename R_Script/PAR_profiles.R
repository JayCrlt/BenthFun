rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

Folder         <- "Data/Spring_2023/PI_Curves/Light"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))

files_PAR = list() ; for (i in 1:length(document_files)) {
  files_PAR[[i]]           <- read.csv(paste(Folder, document_files[i], sep = "/"), header=T) }

Sensor_Temporal_1 <- rbind(files_PAR[[1]], files_PAR[[3]]) %>% data.frame() %>% 
  mutate(., Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) %>% 
  dplyr::select(c(Datetime, Date, Time, raw_value, calibrated_value)) %>% 
  mutate(., sensor_used = rep())

Sensor_Temporal_3 <- rbind(files_PAR[[2]], files_PAR[[4]]) %>% data.frame() %>% 
  mutate(., Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) %>% 
  dplyr::select(c(Datetime, Date, Time, raw_value, calibrated_value))

