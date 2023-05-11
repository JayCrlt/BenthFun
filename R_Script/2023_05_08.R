rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

### Diving_Log
Diving_log <- read_excel("Data/Spring_2023/Diving_log_Spring_2023_BenthFun.xlsx", 
                         col_types = c("date", "text", "text", "date", "date", 
                                       "date", "text", "text", "numeric", "numeric")) %>% 
  mutate(., Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S"))

# 3 minutes mismatch 
minutes_mismatch = 3 * 60

#####################
## Incubation Date ##
#####################

date <- "2023-05-08"

# Set differents paths
Folder         <- paste("Data/Spring_2023/Transplants/O2", date, sep ="/")
document_files <- list.files(paste(getwd(), Folder, sep = "/"))
files_O2 = list() ; for (i in 1:length(document_files)) {
  files_O2[[i]]           <- read.csv(paste(Folder, document_files[i], sep = "/"), header=T)
  colnames(files_O2[[i]]) <- c("Time", "BV", "Temperature", "O2", "Q") 
  files_O2[[i]]$Time      <- ISOdatetime(1970,1,1,1,0,0) + files_O2[[i]]$Time - minutes_mismatch}

## Time intervals
Date_incubation   <- paste(basename(Folder))
subset_diving_log <- paste("Diving_log", basename(Folder), sep = "_") 
subset_diving_log = Diving_log %>% mutate(., Diving_Date = lubridate::ymd(Diving_Date)) %>% 
  dplyr::filter(., Diving_Date == format(as.POSIXct(basename(Folder)), format = "%Y-%m-%d UTC"))
tiles_numbering <- unique(subset_diving_log$`Tile_N°`) %>% na.omit()
subset_diving_log <- subset_diving_log %>% arrange(O2_sensor_used)

## Corresponding tile to incubation condition, pH condition and transplant time
label <- paste("T0_ELOW", str_extract(subset_diving_log$Chamber, "[^_]+"), subset_diving_log$`Tile_N°`, sep = "_")

for (i in 1:16) {
  label[[i]] = list()
  for (j in O2_sensor_used)
  label[[i]] = d
}


# First_tile
Light_tile_02 <-  files_O2[[1]] %>% filter(Time >= '2023-05-08 10:43:00') %>% filter(Time <= '2023-05-08 12:09:00')
Night_tile_02 <-  files_O2[[1]] %>% filter(Time >= '2023-05-08 12:48:00') %>% filter(Time <= '2023-05-08 14:30:00')
# Second_tile
Light_tile_18 <-  files_O2[[2]] %>% filter(Time >= '2023-05-08 10:41:00') %>% filter(Time <= '2023-05-08 12:14:00')
Night_tile_18 <-  files_O2[[2]] %>% filter(Time >= '2023-05-08 12:48:00') %>% filter(Time <= '2023-05-08 14:30:00')
# Third_tile
Light_tile_13 <-  files_O2[[3]] %>% filter(Time >= '2023-05-08 10:42:00') %>% filter(Time <= '2023-05-08 12:18:00')
Night_tile_13 <-  files_O2[[3]] %>% filter(Time >= '2023-05-08 12:48:00') %>% filter(Time <= '2023-05-08 14:30:00')
# Fourth_tile
Light_tile_09 <-  files_O2[[5]] %>% filter(Time >= '2023-05-08 13:04:00') %>% filter(Time <= '2023-05-08 14:04:00')
Night_tile_09 <-  files_O2[[5]] %>% filter(Time >= '2023-05-08 10:49:00') %>% filter(Time <= '2023-05-08 12:32:00')
# Fifth_tile
Light_tile_06 <-  files_O2[[6]] %>% filter(Time >= '2023-05-08 12:56:00') %>% filter(Time <= '2023-05-08 14:04:00')
Night_tile_06 <-  files_O2[[6]] %>% filter(Time >= '2023-05-08 10:47:00') %>% filter(Time <= '2023-05-08 12:31:00')
# Sixth_tile
Light_tile_05 <-  files_O2[[7]] %>% filter(Time >= '2023-05-08 12:54:00') %>% filter(Time <= '2023-05-08 14:04:00')
Night_tile_05 <-  files_O2[[7]] %>% filter(Time >= '2023-05-08 10:49:00') %>% filter(Time <= '2023-05-08 12:33:00')
# Blank_1
Light_blank_1 <-  files_O2[[4]] %>% filter(Time >= '2023-05-08 10:45:00') %>% filter(Time <= '2023-05-08 12:23:00')
Night_blank_1 <-  files_O2[[4]] %>% filter(Time >= '2023-05-08 12:48:00') %>% filter(Time <= '2023-05-08 14:30:00')
# Sixth_tile
Light_blank_2 <-  files_O2[[8]] %>% filter(Time >= '2023-05-08 12:55:00') %>% filter(Time <= '2023-05-08 14:04:00')
Night_blank_2 <-  files_O2[[8]] %>% filter(Time >= '2023-05-08 10:47:00') %>% filter(Time <= '2023-05-08 12:33:00')




A = ggplot() + 
  geom_point(data = Light_tile_02, aes(x = Time, y = O2), fill = "red", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_18, aes(x = Time, y = O2), fill = "orange", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_13, aes(x = Time, y = O2), fill = "brown", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_09, aes(x = Time, y = O2), fill = "darkgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_06, aes(x = Time, y = O2), fill = "lightgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_05, aes(x = Time, y = O2), fill = "limegreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_blank_1, aes(x = Time, y = O2), fill = "lightblue", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_blank_2, aes(x = Time, y = O2), fill = "cornflowerblue", col = "black", size = 2, shape = 21) +
  theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(1,11.5))

B = ggplot() + 
  geom_point(data = Night_tile_02, aes(x = Time, y = O2), fill = "red", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_18, aes(x = Time, y = O2), fill = "orange", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_13, aes(x = Time, y = O2), fill = "brown", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_09, aes(x = Time, y = O2), fill = "darkgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_06, aes(x = Time, y = O2), fill = "lightgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_05, aes(x = Time, y = O2), fill = "limegreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_blank_1, aes(x = Time, y = O2), fill = "lightblue", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_blank_2, aes(x = Time, y = O2), fill = "cornflowerblue", col = "black", size = 2, shape = 21) +
  theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(1,11.5))

A + B
