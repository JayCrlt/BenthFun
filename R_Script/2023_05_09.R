rm(list = ls()) ; options(cores = 4)
library(tidyverse) ; library(patchwork)

# 3 minutes mismatch 
minutes_mismatch = 3 * 60

# Set differents paths
Folder         <- "../../../2023_05_09/O2"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))
files_O2 = list() ; for (i in 1:length(document_files)) {
  files_O2[[i]]           <- read.csv(paste(Folder, document_files[i], sep = "/"), header=T)
  colnames(files_O2[[i]]) <- c("Time", "BV", "Temperature", "O2", "Q") 
  files_O2[[i]]$Time      <- ISOdatetime(1970,1,1,1,0,0) + files_O2[[i]]$Time - minutes_mismatch}

## Time intervals
# First_tile
Light_tile_01 <-  files_O2[[1]] %>% filter(Time >= '2023-05-09 10:50:00') %>% filter(Time <= '2023-05-09 12:06:00')
Night_tile_01 <-  files_O2[[1]] %>% filter(Time >= '2023-05-09 12:28:00') %>% filter(Time <= '2023-05-09 13:15:00')
# Second_tile
Light_tile_19 <-  files_O2[[2]] %>% filter(Time >= '2023-05-09 10:49:00') %>% filter(Time <= '2023-05-09 11:59:00')
Night_tile_19 <-  files_O2[[2]] %>% filter(Time >= '2023-05-09 12:29:00') %>% filter(Time <= '2023-05-09 13:15:00')
# Third_tile
Light_tile_29 <-  files_O2[[3]] %>% filter(Time >= '2023-05-09 10:53:00') %>% filter(Time <= '2023-05-09 12:10:00')
Night_tile_29 <-  files_O2[[3]] %>% filter(Time >= '2023-05-09 12:30:00') %>% filter(Time <= '2023-05-09 13:15:00')
# Fourth_tile
Light_tile_10 <-  files_O2[[5]] %>% filter(Time >= '2023-05-09 12:20:00') %>% filter(Time <= '2023-05-09 13:20:00')
Night_tile_10 <-  files_O2[[5]] %>% filter(Time >= '2023-05-09 11:01:00') %>% filter(Time <= '2023-05-09 11:43:00')
# Fifth_tile
Light_tile_11 <-  files_O2[[6]] %>% filter(Time >= '2023-05-09 12:22:00') %>% filter(Time <= '2023-05-09 13:21:00')
Night_tile_11 <-  files_O2[[6]] %>% filter(Time >= '2023-05-09 10:58:00') %>% filter(Time <= '2023-05-09 11:47:00')
# Sixth_tile
Light_tile_03 <-  files_O2[[7]] %>% filter(Time >= '2023-05-09 12:23:00') %>% filter(Time <= '2023-05-09 13:28:00')
Night_tile_03 <-  files_O2[[7]] %>% filter(Time >= '2023-05-09 10:59:00') %>% filter(Time <= '2023-05-09 11:48:00')
# Blank_1
Light_blank_1 <-  files_O2[[4]] %>% filter(Time >= '2023-05-09 10:55:00') %>% filter(Time <= '2023-05-09 11:53:00')
Night_blank_1 <-  files_O2[[4]] %>% filter(Time >= '2023-05-09 12:31:00') %>% filter(Time <= '2023-05-09 13:15:00')
# Sixth_tile
Light_blank_2 <-  files_O2[[8]] %>% filter(Time >= '2023-05-09 12:24:00') %>% filter(Time <= '2023-05-09 13:35:00')
Night_blank_2 <-  files_O2[[8]] %>% filter(Time >= '2023-05-09 10:57:00') %>% filter(Time <= '2023-05-09 11:49:00')




A = ggplot() + 
  geom_point(data = Light_tile_01, aes(x = Time, y = O2), fill = "red", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_19, aes(x = Time, y = O2), fill = "orange", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_29, aes(x = Time, y = O2), fill = "brown", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_10, aes(x = Time, y = O2), fill = "darkgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_11, aes(x = Time, y = O2), fill = "lightgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_03, aes(x = Time, y = O2), fill = "limegreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_blank_1, aes(x = Time, y = O2), fill = "lightblue", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_blank_2, aes(x = Time, y = O2), fill = "cornflowerblue", col = "black", size = 2, shape = 21) +
  theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(5,16.5))

B = ggplot() + 
  geom_point(data = Night_tile_01, aes(x = Time, y = O2), fill = "red", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_19, aes(x = Time, y = O2), fill = "orange", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_29, aes(x = Time, y = O2), fill = "brown", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_10, aes(x = Time, y = O2), fill = "darkgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_11, aes(x = Time, y = O2), fill = "lightgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_03, aes(x = Time, y = O2), fill = "limegreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_blank_1, aes(x = Time, y = O2), fill = "lightblue", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_blank_2, aes(x = Time, y = O2), fill = "cornflowerblue", col = "black", size = 2, shape = 21) +
  theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(5,16.5))

A + B
