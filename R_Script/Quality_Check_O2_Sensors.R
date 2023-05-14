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

# Useful functions
floor_dec   <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# Set colors (you need to pick 8 colors)
colors = c("red", "orange", "brown", "lightblue", "lightgreen", "limegreen", "darkgreen", "cornflowerblue")

#####################
## Incubation Date ##
#####################

date      <- "2023-05-10"

## Condition 
if (date == "2023-05-08") { 
  condition <- "T0_ELOW" } else if (date == "2023-05-09") { 
    condition <- "T0_LOW" } else if (date == "2023-05-10") { 
      condition <- "T0_AMB" } else {
        condition <- NA }

#####################

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
subset_diving_log$process <- c(rep(c("Net Photosynthesis", "Dark_respiration"), 4), rep(c("Dark_respiration", "Net Photosynthesis"), 4))

# Split dark respiration from net photosynthesis
subset_diving_log <- subset_diving_log %>% group_by(process) %>% group_split()

## Corresponding tile to incubation condition, pH condition and transplant time
label = vector("list", 2) ; for (process in 1:2) {
  label[[process]] <- paste(condition, str_extract(subset_diving_log[[process]]$Chamber, "[^_]+"), subset_diving_log[[process]]$`Tile_N°`, sep = "_")}
label_dark_respiration = abind::abind(label[[1]])
label_net_photosynthesis = abind::abind(label[[2]])

# Dark_Respiration files
dark_respiration <- vector("list", length = 8) ; for (O2_sensor_used in 1:8) {
  dark_respiration[[O2_sensor_used]] = files_O2[[O2_sensor_used]] %>% 
    mutate(Time = format(as.POSIXct(Time), format = "%H:%M:%S")) %>% 
    dplyr::filter(., Time >= subset_diving_log[[1]]$Start_incubation[O2_sensor_used]) %>% 
    dplyr::filter(., Time <= subset_diving_log[[1]]$Stop_Incubation[O2_sensor_used])}

# Dark_Respiration files
net_photosynthesis <- vector("list", length = 8) ; for (O2_sensor_used in 1:8) {
  net_photosynthesis[[O2_sensor_used]] = files_O2[[O2_sensor_used]] %>% 
    mutate(Time = format(as.POSIXct(Time), format = "%H:%M:%S")) %>% 
    dplyr::filter(., Time >= subset_diving_log[[2]]$Start_incubation[O2_sensor_used]) %>% 
    dplyr::filter(., Time <= subset_diving_log[[2]]$Stop_Incubation[O2_sensor_used])}

## change character to date time
for (O2_sensor_used in 1:8) {
  net_photosynthesis[[O2_sensor_used]]$Time = strptime(paste(date, net_photosynthesis[[O2_sensor_used]]$Time, sep = " "), "%Y-%m-%d %H:%M:%S") 
  dark_respiration[[O2_sensor_used]]$Time = strptime(paste(date, dark_respiration[[O2_sensor_used]]$Time, sep = " "), "%Y-%m-%d %H:%M:%S") 
}

## Set the plots limits 
max_limits_y <- net_photosynthesis %>% bind_rows() %>% summarise(max = max(O2))
min_limits_y <- dark_respiration %>% bind_rows() %>% summarise(min = min(O2))
max_limits_x <- net_photosynthesis %>% bind_rows() %>% summarise(max = max(Time))
min_limits_x <- net_photosynthesis %>% bind_rows() %>% summarise(min = min(Time))

## Steps between tiles position
step_time <- difftime(max_limits_x$max, min_limits_x$min + 3600) / 8
position_tile_label <- seq(min_limits_x$min + 3600, max_limits_x$max - step_time, step_time)
data_tile_position <- data.frame(x = position_tile_label, y = max_limits_y - 1, label = subset_diving_log[[2]]$`Tile_N°`)
data_tile_position$label[4] = "B1" ; data_tile_position$label[8] = "B2" 
data_tile_position$label <- factor(data_tile_position$label, levels = data_tile_position$label)
legend_tile_position <- data.frame(x = min_limits_x$min, y = max_limits_y - 1, label = "Tile used:")

### First Viz

# Dark respiration
dark_respiration_viz <- ggplot() + theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(min_limits_y$min, max_limits_y$max))
for (i in 1:8) { dark_respiration_viz = dark_respiration_viz + 
  geom_point(data = dark_respiration[[i]], aes(x = as.POSIXct(Time + 7200, format = "%H:%M:%S"), y = O2), fill = colors[i], col = "black", size = 2, shape = 21) }
dark_respiration_viz <- dark_respiration_viz + scale_x_datetime(name = "Time (UTC + 2:00)", breaks = "30 min", labels = scales::date_format("%H:%M")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16)) +
  ggtitle("Dark respiration")

# Net photosynthesis
net_photosynthesis_viz <- ggplot() + theme_classic() +
  scale_y_continuous(name = "O2 concentration (mg/L)", limits = c(min_limits_y$min, max_limits_y$max))
for (i in 1:8) { net_photosynthesis_viz = net_photosynthesis_viz + 
  geom_point(data = net_photosynthesis[[i]], aes(x = as.POSIXct(Time + 7200, format = "%H:%M:%S"), y = O2), fill = colors[i], col = "black", size = 2, shape = 21) }
net_photosynthesis_viz <- net_photosynthesis_viz + scale_x_datetime(name = "Time (UTC + 2:00)", breaks = "30 min", labels = scales::date_format("%H:%M")) + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16)) +
  ggtitle("Net photosynthesis")

# Community observed
legend_plot <- ggplot(data_tile_position, aes(x = x, y = max, label = label, fill = label)) + geom_label() +
  scale_fill_manual(values = colors) + 
  annotate("text", x = legend_tile_position$x + step_time, y = legend_tile_position$max, label = legend_tile_position$label, size = 6) +
  scale_x_datetime(limits = c(min_limits_x$min, max_limits_x$max)) +
  scale_y_continuous(limits = c(floor_dec(legend_tile_position$max, 2), ceiling_dec(legend_tile_position$max, 2))) +
  theme_void() + theme(legend.position="none")

# Quality check
(Quality_check <- (dark_respiration_viz + net_photosynthesis_viz) / legend_plot + plot_annotation(title = paste(date, condition, sep = " – ")) +
    plot_layout(heights = c(5, 1)) +
    theme(plot.margin = unit(rep(0.5,4),"cm")))