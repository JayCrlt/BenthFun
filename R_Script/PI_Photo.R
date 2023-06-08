#rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

# Diving log
Diving_log <- read_excel("Data/Spring_2023/Diving_log_Spring_2023_BenthFun.xlsx", 
                         col_types = c("date", "text", "text", "date", "date", 
                                       "date", "text", "text", "numeric", "numeric"),
                         sheet = "Corrected") %>% 
  mutate(Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S"))

label_decomposition <- str_split(Diving_log$Label, fixed("_"))
for (i in 1:length(label_decomposition)) {
  Diving_log$`Stage experiment`[i] <- label_decomposition[[i]][1] 
  Diving_log$pH_Cond[i] <- label_decomposition[[i]][3] }

Diving_log_PAR <- Diving_log %>% dplyr::filter(`Stage experiment` == "PI")

date      <- "2023-05-29"

## Condition 
if (date == "2023-05-29") { 
  condition <- "PI – ELOW" } else if (date == "2023-05-30") { 
    condition <- "PI – LOW" } else if (date == "2023-05-31") { 
      condition <- "PI – AMB" } else {
        condition <- NA }

#####################

# Set differents paths
Folder         <- paste("Data/Spring_2023/PI_Curves/O2", date, sep ="/")
document_files <- list.files(paste(getwd(), Folder, sep = "/"))
files_O2 = list() ; for (i in 1:length(document_files)) {
  files_O2[[i]]           <- read.csv(paste(Folder, document_files[i], sep = "/"), header=T)
  colnames(files_O2[[i]]) <- c("Time", "BV", "Temperature", "O2", "Q") 
  files_O2[[i]]$Time      <- ISOdatetime(1970,1,1,1,0,0) + files_O2[[i]]$Time}

## Time intervals
Date_incubation   <- paste(basename(Folder))
subset_diving_log <- paste("Diving_log", basename(Folder), sep = "_") 
subset_diving_log = Diving_log %>% mutate(., Diving_Date = lubridate::ymd(Diving_Date)) %>% 
  dplyr::filter(., Diving_Date == format(as.POSIXct(basename(Folder)), format = "%Y-%m-%d UTC"))
Tiles_match       <- data.frame(O2_sensor_used = unique(subset_diving_log$O2_sensor_used), 
                                `Tile_N°` = unique(subset_diving_log$`Tile_N°`))
tiles_numbering   <- unique(subset_diving_log$`Tile_N°`) %>% na.omit()
subset_diving_log <- subset_diving_log %>% arrange(O2_sensor_used)
subset_diving_log$process <- rep("Net Photosynthesis", length(subset_diving_log$Diving_Date))
subset_diving_log <- subset_diving_log |> group_by(O2_sensor_used) |> group_split()

# net_photosynthesis files
Profile_O2 = list(vector(mode = "list", length = 8), vector(mode = "list", length = 7), 
                  vector(mode = "list", length = 7), vector(mode = "list", length = 1))

for (incubation_time in 1:8){
  for (O2_sensor_used in 1:4) {
    Profile_O2[[O2_sensor_used]][[incubation_time]] <- files_O2[[O2_sensor_used]] %>% 
      mutate(Time = format(as.POSIXct(Time), format = "%H:%M:%S")) %>% 
      dplyr::filter(., Time >= subset_diving_log[[O2_sensor_used]]$Start_incubation[incubation_time]) %>% 
      dplyr::filter(., Time <= subset_diving_log[[O2_sensor_used]]$Stop_Incubation[incubation_time]) %>%
      mutate(Incubation_Time = paste("T", incubation_time, sep =""))
      
  }
}

Sensor_1_Profile <- Profile_O2[[1]] %>% bind_rows() %>% mutate(O2_sensor_used = "1")
Sensor_2_Profile <- Profile_O2[[2]] %>% bind_rows() %>% mutate(O2_sensor_used = "2")
Sensor_3_Profile <- Profile_O2[[3]] %>% bind_rows() %>% mutate(O2_sensor_used = "3")
Sensor_4_Profile <- Profile_O2[[4]] %>% bind_rows() %>% mutate(O2_sensor_used = "4")
PI_Profiles <- rbind(Sensor_1_Profile, Sensor_2_Profile, Sensor_3_Profile, Sensor_4_Profile) %>% left_join(Tiles_match) %>% 
  mutate(Time = as.POSIXct(Time, format = "%H:%M:%S"))

## Set the plots limits 
max_limits_y <- PI_Profiles %>% summarise(max = max(O2))
min_limits_y <- PI_Profiles %>% summarise(min = min(O2))
max_limits_x <- PI_Profiles %>% summarise(max = max(Time))
min_limits_x <- PI_Profiles %>% summarise(min = min(Time))

net_photosynthesis_viz <- ggplot(data = PI_Profiles) + theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(min_limits_y$min, max_limits_y$max)) +
  geom_point(aes(x = as.POSIXct(Time + 7200, format = "%H:%M:%S"), y = O2, fill = Tile_N.), col = "black", size = 2, shape = 21) + 
  scale_x_datetime(name = "Time (UTC + 2:00)", breaks = "30 min", labels = scales::date_format("%H:%M")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16)) +
  ggtitle("Net photosynthesis")

## Define the photosynthesis rate

PI_Profiles_rates <- PI_Profiles |> group_by(Incubation_Time, O2_sensor_used) |> group_split()

net_photosynthesis_rates = vector("list", length(PI_Profiles_rates)) 
photosynthesis_rates_df = vector("list", length(PI_Profiles_rates))
for (i in 1:length(net_photosynthesis_rates)) {
  net_photosynthesis_rates[[i]] <- rbind(PI_Profiles_rates[[i]] %>% slice_max(O2, n = 3) %>% slice(1:3), 
                                         PI_Profiles_rates[[i]] %>% slice_min(O2, n = 3) %>% slice(1:3)) 
  # making all combinations (3 x 3)
  photosynthesis_rates_df[[i]]   <- data.frame(Time_difference  = c(difftime(net_photosynthesis_rates[[i]]$Time[1], net_photosynthesis_rates[[i]]$Time[6]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[1], net_photosynthesis_rates[[i]]$Time[5]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[1], net_photosynthesis_rates[[i]]$Time[4]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[2], net_photosynthesis_rates[[i]]$Time[6]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[2], net_photosynthesis_rates[[i]]$Time[5]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[2], net_photosynthesis_rates[[i]]$Time[4]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[3], net_photosynthesis_rates[[i]]$Time[6]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[3], net_photosynthesis_rates[[i]]$Time[5]),
                                                                    difftime(net_photosynthesis_rates[[i]]$Time[3], net_photosynthesis_rates[[i]]$Time[4])),
                                               O2_Concentration = c(net_photosynthesis_rates[[i]]$O2[1] - net_photosynthesis_rates[[i]]$O2[6],
                                                                    net_photosynthesis_rates[[i]]$O2[1] - net_photosynthesis_rates[[i]]$O2[5],
                                                                    net_photosynthesis_rates[[i]]$O2[1] - net_photosynthesis_rates[[i]]$O2[4],
                                                                    net_photosynthesis_rates[[i]]$O2[2] - net_photosynthesis_rates[[i]]$O2[6],
                                                                    net_photosynthesis_rates[[i]]$O2[2] - net_photosynthesis_rates[[i]]$O2[5],
                                                                    net_photosynthesis_rates[[i]]$O2[2] - net_photosynthesis_rates[[i]]$O2[4],
                                                                    net_photosynthesis_rates[[i]]$O2[3] - net_photosynthesis_rates[[i]]$O2[6],
                                                                    net_photosynthesis_rates[[i]]$O2[3] - net_photosynthesis_rates[[i]]$O2[5],
                                                                    net_photosynthesis_rates[[i]]$O2[3] - net_photosynthesis_rates[[i]]$O2[4]))
  photosynthesis_rates_df[[i]]   <- photosynthesis_rates_df[[i]] |> mutate(abs_time = abs(Time_difference), 
                                                                           Tile_N. = unique(PI_Profiles_rates[[i]]$Tile_N.),
                                                                           incubation_time = unique(PI_Profiles_rates[[i]]$Incubation_Time))
  photosynthesis_rates_df[[i]]   <- photosynthesis_rates_df[[i]] %>% slice_max(abs_time, n = 3) %>% 
    mutate(photosynthesis_rate = O2_Concentration / as.numeric(Time_difference, units = "hours")) %>% 
    slice_max(photosynthesis_rate, n = 3) }

### Compiling data
mean_value_photo = vector("list", length(PI_Profiles_rates)) ; for (i in 1:length(PI_Profiles_rates)) {
  mean_value_photo[[i]] <- photosynthesis_rates_df[[i]] %>% group_by(Tile_N., incubation_time) %>%
    summarise(net_photosynthesis_rate_avg = mean(photosynthesis_rate), net_photosynthesis_rate_sd = sd(photosynthesis_rate))}

title = unique(subset_diving_log[[1]]$pH_Cond)

(compiled_data <- mean_value_photo %>% bind_rows() %>% dplyr::filter(., `Tile_N.` != "NA") %>%
  ggplot(., aes(x = incubation_time, y = net_photosynthesis_rate_avg, fill = Tile_N., fill = Tile_N.)) + 
  geom_bar(stat = "identity", position="dodge", color = "black") +
  theme_dark() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        panel.grid.major = element_line(NA),
        panel.grid.minor = element_line(NA),
        axis.ticks.y = element_line(NA),
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) +
  scale_fill_manual(values = c("firebrick1", "firebrick", "darkred")) +
  #scale_fill_manual(values = c("gold", "goldenrod", "goldenrod4")) +
  #scale_fill_manual(values = c("dodgerblue", "dodgerblue3", "dodgerblue4")) +
  scale_y_continuous(name = expression(O[2]~"concentration (mg."*L^-1*h^-1*")"), limits = c(-5, 18)) +
  scale_x_discrete(name = "") +
  geom_linerange(aes(x = incubation_time, 
                    ymax = net_photosynthesis_rate_avg + net_photosynthesis_rate_sd,
                    ymin = net_photosynthesis_rate_avg - net_photosynthesis_rate_sd),
                position = position_dodge(width = 0.9), colour="black") +
    ggtitle(title))