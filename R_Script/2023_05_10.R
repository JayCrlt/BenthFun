rm(list = ls()) ; options(cores = 4)
library(tidyverse) ; library(patchwork)

# 3 minutes mismatch 
minutes_mismatch = 3 * 60

# Set differents paths
Folder         <- "../../../2023_05_10/O2"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))
files_O2 = list() ; for (i in 1:length(document_files)) {
  files_O2[[i]]           <- read.csv(paste(Folder, document_files[i], sep = "/"), header=T)
  colnames(files_O2[[i]]) <- c("Time", "BV", "Temperature", "O2", "Q") 
  files_O2[[i]]$Time      <- ISOdatetime(1970,1,1,1,0,0) + files_O2[[i]]$Time - minutes_mismatch}

## Time intervals
# First_tile
Light_tile_04 <-  files_O2[[1]] %>% filter(Time >= '2023-05-10 11:00:00') %>% filter(Time <= '2023-05-10 12:10:00')
Night_tile_04 <-  files_O2[[1]] %>% filter(Time >= '2023-05-10 12:42:00') %>% filter(Time <= '2023-05-10 13:27:00')
# Second_tile
Light_tile_07 <-  files_O2[[2]] %>% filter(Time >= '2023-05-10 11:02:00') %>% filter(Time <= '2023-05-10 12:14:00')
Night_tile_07 <-  files_O2[[2]] %>% filter(Time >= '2023-05-10 12:44:00') %>% filter(Time <= '2023-05-10 13:28:00')
# Third_tile
Light_tile_28 <-  files_O2[[3]] %>% filter(Time >= '2023-05-10 11:03:00') %>% filter(Time <= '2023-05-10 12:20:00')
Night_tile_28 <-  files_O2[[3]] %>% filter(Time >= '2023-05-10 12:46:00') %>% filter(Time <= '2023-05-10 13:29:00')
# Fourth_tile
Light_tile_08 <-  files_O2[[5]] %>% filter(Time >= '2023-05-10 12:37:00') %>% filter(Time <= '2023-05-10 13:36:00')
Night_tile_08 <-  files_O2[[5]] %>% filter(Time >= '2023-05-10 11:13:00') %>% filter(Time <= '2023-05-10 11:59:00')
# Fifth_tile
Light_tile_12 <-  files_O2[[6]] %>% filter(Time >= '2023-05-10 12:38:00') %>% filter(Time <= '2023-05-10 13:42:00')
Night_tile_12 <-  files_O2[[6]] %>% filter(Time >= '2023-05-10 11:14:00') %>% filter(Time <= '2023-05-10 12:00:00')
# Sixth_tile
Light_tile_14 <-  files_O2[[7]] %>% filter(Time >= '2023-05-10 12:39:00') %>% filter(Time <= '2023-05-10 13:46:00')
Night_tile_14 <-  files_O2[[7]] %>% filter(Time >= '2023-05-10 11:16:00') %>% filter(Time <= '2023-05-10 12:01:00')
# Blank_1
Light_blank_1 <-  files_O2[[4]] %>% filter(Time >= '2023-05-10 11:05:00') %>% filter(Time <= '2023-05-10 12:26:00')
Night_blank_1 <-  files_O2[[4]] %>% filter(Time >= '2023-05-10 12:47:00') %>% filter(Time <= '2023-05-10 13:30:00')
# Sixth_tile
Light_blank_2 <-  files_O2[[8]] %>% filter(Time >= '2023-05-10 12:41:00') %>% filter(Time <= '2023-05-10 13:48:00')
Night_blank_2 <-  files_O2[[8]] %>% filter(Time >= '2023-05-10 11:17:00') %>% filter(Time <= '2023-05-10 12:02:00')




A = ggplot() + 
  geom_point(data = Light_tile_04, aes(x = Time, y = O2), fill = "red", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_07, aes(x = Time, y = O2), fill = "orange", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_28, aes(x = Time, y = O2), fill = "brown", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_08, aes(x = Time, y = O2), fill = "darkgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_12, aes(x = Time, y = O2), fill = "lightgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_tile_14, aes(x = Time, y = O2), fill = "limegreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_blank_1, aes(x = Time, y = O2), fill = "lightblue", col = "black", size = 2, shape = 21) +
  geom_point(data = Light_blank_2, aes(x = Time, y = O2), fill = "cornflowerblue", col = "black", size = 2, shape = 21) +
  theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(5,16.5))

B = ggplot() + 
  geom_point(data = Night_tile_04, aes(x = Time, y = O2), fill = "red", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_07, aes(x = Time, y = O2), fill = "orange", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_28, aes(x = Time, y = O2), fill = "brown", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_08, aes(x = Time, y = O2), fill = "darkgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_12, aes(x = Time, y = O2), fill = "lightgreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_tile_14, aes(x = Time, y = O2), fill = "limegreen", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_blank_1, aes(x = Time, y = O2), fill = "lightblue", col = "black", size = 2, shape = 21) +
  geom_point(data = Night_blank_2, aes(x = Time, y = O2), fill = "cornflowerblue", col = "black", size = 2, shape = 21) +
  theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(5,16.5))

A + B
