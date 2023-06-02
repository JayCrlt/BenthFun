rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

`%notin%` <- Negate(`%in%`)

Folder         <- "Data/Spring_2023/PI_Curves/Light"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))

files_PAR = list() ; for (i in 1:length(document_files)) {
  files_PAR[[i]]           <- read.csv(paste(Folder, document_files[i], sep = "/"), header=T) }

Sensor_Temporal_1 <- rbind(files_PAR[[1]], files_PAR[[3]]) %>% data.frame() %>% 
  mutate(., Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) %>% 
  dplyr::select(c(Datetime, Date, Time, raw_value, calibrated_value)) %>% 
  mutate(., sensor_used = rep("Sensor_Temporal_1", dim(rbind(files_PAR[[1]], files_PAR[[3]]))[1]))

Sensor_Temporal_3 <- rbind(files_PAR[[2]], files_PAR[[4]]) %>% data.frame() %>% 
  mutate(., Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) %>% 
  dplyr::select(c(Datetime, Date, Time, raw_value, calibrated_value)) %>% 
  mutate(., sensor_used = rep("Sensor_Temporal_3", dim(rbind(files_PAR[[2]], files_PAR[[4]]))[1]))

Light_Activity <- rbind(Sensor_Temporal_1, Sensor_Temporal_3) %>% data.frame() %>% group_by(Date) %>% group_split()

ELOW_AM <- Light_Activity[[2]] %>% dplyr::filter(Datetime >= "2023-05-29 06:10:00", Datetime <= "2023-05-29 13:35:00")
ELOW_PM <- Light_Activity[[2]] %>% dplyr::filter(Datetime >= "2023-05-29 16:50:00", Datetime <= "2023-05-29 20:55:00")
LOW_AM <- Light_Activity[[3]] %>% dplyr::filter(Datetime >= "2023-05-30 06:25:00", Datetime <= "2023-05-30 14:00:00")
LOW_PM <- Light_Activity[[3]] %>% dplyr::filter(Datetime >= "2023-05-30 17:15:00", Datetime <= "2023-05-30 19:50:00")
AMB_AM <- Light_Activity[[4]] %>% dplyr::filter(Datetime >= "2023-05-31 06:20:00", Datetime <= "2023-05-31 13:45:00")
AMB_PM <- Light_Activity[[4]] %>% dplyr::filter(Datetime >= "2023-05-31 17:05:00", Datetime <= "2023-05-31 19:45:00")

Light_Activity <- rbind(ELOW_AM, ELOW_PM, LOW_AM, LOW_PM, AMB_AM, AMB_PM) %>% data.frame()
avg_light <- Light_Activity %>% group_by(Datetime) %>% summarise(calibrated_value_avg = mean(calibrated_value), 
                                                                 calibrated_value_sd  = sd(calibrated_value))
values_to_rm = avg_light %>% dplyr::filter(calibrated_value_sd >= 20)
Light_Activity = Light_Activity %>% dplyr::filter(Datetime %notin% c(values_to_rm$Datetime))

Light_Activity <- Light_Activity %>% group_by(Date) %>% group_split()

A <- ggplot(Light_Activity[[1]], aes(x = Datetime, y = calibrated_value)) + 
  geom_point(aes(fill = sensor_used), shape = 21, size = 3, alpha = .7) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 15), 
              linetype = "dashed", size = 0.5, col = "black", se = F) +
  scale_x_datetime(name = "") + scale_y_continuous(name = "Light intensity (units)") +
  theme_classic() + ggtitle("Extreme Low conditions")
