### Changes over time due to pH change
rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)

# Load data
Biomass   <- read_excel("Outputs/Summary/Biomass_Transplants_data.xlsx")
Functions <- read_excel("Outputs/Summary/Summary_Process_BenthFun.xlsx") %>% dplyr::filter(Main_Exp == "Transplants")
Nutrients <- read_excel("Outputs/Summary/Nutrients_Transplants_data.xlsx")
PAR_tiles <- read_excel("Outputs/Summary/PAR_Transplants.xlsx") 

# Colors
values_grandient_color = c("#402d21", "#553c2c", "#704f3a", "#805b43", "#90664b", "#a07154",
                           "#af8266", "#ba947b", "#cbae9b", "#d3bbab", "#e4d5cc", "#efe7e1", "#ffffff",
                           "#dcecd4", "#cee5c3", "#bbdbac", "#a4cf8f", "#9fcc8a", "#88c06d", "#7ab85c",
                           "#6cb04c", "#629f44", "#578e3d", "#497733", "#3f662c", "#314f22")

## Compile to a single dataframe()
# First Functions and Nutrients
dataset_change <- data.frame(Tile    = c(Functions$Tile, Nutrients$Tile),
                             pH      = c(Functions$pH_cond, Nutrients$pH),
                             Time    = c(Functions$incub_time, Nutrients$Time),
                             Process = c(Functions$Process, Nutrients$Nutrients_var),
                             output  = c(Functions$avg_output, Nutrients$Nutrients_val))

dataset_change$pH[dataset_change$pH == "extreme low pH conditions"] = "ELOW"
dataset_change$pH[dataset_change$pH == "low pH conditions"] = "LOW"
dataset_change$pH[dataset_change$pH == "ambient pH conditions"] = "AMB"

# Std with biomass
dataset_change_cal <- dataset_change %>% dplyr::filter(Process == "calcifcation rate") %>% 
  left_join(Biomass %>% dplyr::select(-c(Biomass_overall, Biomass_npp, Biomass_fil, 
                                         Biomass_std_overall, Biomass_std_cal, Biomass_std_npp, Biomass_std_fil)),
            by = c("Tile", "pH", "Time")) %>% mutate(output_std = output / Biomass_cal) %>% select(-Biomass_cal)
dataset_change_npp <- dataset_change %>% dplyr::filter(Process == "gross photosynthesis rate") %>% 
  left_join(Biomass %>% dplyr::select(-c(Biomass_overall, Biomass_cal, Biomass_fil, 
                                         Biomass_std_overall, Biomass_std_cal, Biomass_std_npp, Biomass_std_fil)),
            by = c("Tile", "pH", "Time")) %>% mutate(output_std = output / Biomass_npp) %>% select(-Biomass_npp)
dataset_change_tot <- dataset_change %>% dplyr::filter(Process %notin% c("calcifcation rate", "gross photosynthesis rate")) %>% 
  left_join(Biomass %>% dplyr::select(-c(Biomass_cal, Biomass_npp, Biomass_fil, 
                                         Biomass_std_overall, Biomass_std_cal, Biomass_std_npp, Biomass_std_fil)),
            by = c("Tile", "pH", "Time")) %>% mutate(output_std = output / Biomass_overall) %>% select(-Biomass_overall)

##### PAR CORRECTION ----
### Need to correct by the PAR!
ELO_PAR        <- 3.13*(1-1.61*exp(-0.07*seq(0,600,1)))
scaled_ELO_PAR <- data.frame(PAR_intensity = seq(0,600,1), correction = 
  ((ELO_PAR - min(ELO_PAR)) / (max(ELO_PAR) - min(ELO_PAR))) * 100)
LOW_PAR        <- 6.94*(1-1.32*exp(-0.07*seq(0,600,1)))
scaled_LOW_PAR <- data.frame(PAR_intensity = seq(0,600,1), correction = 
  ((LOW_PAR - min(LOW_PAR)) / (max(LOW_PAR) - min(LOW_PAR))) * 100)
AMB_PAR        <- 5.13*(1-1.51*exp(-0.08*seq(0,600,1)))
scaled_AMB_PAR <- data.frame(PAR_intensity = seq(0,600,1), correction = 
  ((AMB_PAR - min(AMB_PAR)) / (max(AMB_PAR) - min(AMB_PAR))) * 100)

## Compile the dataset
dataset_change      <- rbind(dataset_change_cal, dataset_change_npp, dataset_change_tot) %>% arrange(Tile) %>% 
  select(Tile, pH, Time, nb_days, Process, output, output_std) %>% left_join(PAR_tiles, by = c("Tile", "pH", "Time"))
# ELO correction by PAR
dataset_change_ELO_corrected <- dataset_change %>% 
  dplyr::filter(pH == "ELOW", Process %in% c("calcifcation rate", "gross photosynthesis rate",
                                             "net photosynthesis rate", "dark respiration rate")) %>% 
  mutate(PAR_intensity = round(PAR_intensity, 0)) %>% 
  left_join(scaled_ELO_PAR) %>% mutate(output = (output / correction) * 100) %>% select(-correction)
dataset_change_ELO <- rbind(dataset_change_ELO_corrected, dataset_change %>% 
                              dplyr::filter(pH == "ELOW", Process %notin% 
                                            c("calcifcation rate", "gross photosynthesis rate",
                                              "net photosynthesis rate", "dark respiration rate")))
# LOW correction by PAR
dataset_change_LOW_corrected <- dataset_change %>% 
  dplyr::filter(pH == "LOW", Process %in% c("calcifcation rate", "gross photosynthesis rate",
                                            "net photosynthesis rate", "dark respiration rate")) %>% 
  mutate(PAR_intensity = round(PAR_intensity, 0)) %>% 
  left_join(scaled_LOW_PAR) %>% mutate(output = (output / correction) * 100) %>% select(-correction)
dataset_change_LOW <- rbind(dataset_change_LOW_corrected, dataset_change %>% 
                              dplyr::filter(pH == "LOW", Process %notin% 
                                              c("calcifcation rate", "gross photosynthesis rate",
                                                "net photosynthesis rate", "dark respiration rate")))
# AMB correction by PAR
dataset_change_AMB_corrected <- dataset_change %>% 
  dplyr::filter(pH == "AMB", Process %in% c("calcifcation rate", "gross photosynthesis rate",
                                            "net photosynthesis rate", "dark respiration rate")) %>% 
  mutate(PAR_intensity = round(PAR_intensity, 0)) %>% 
  left_join(scaled_AMB_PAR) %>% mutate(output = (output / correction) * 100) %>% select(-correction)
dataset_change_AMB <- rbind(dataset_change_AMB_corrected, dataset_change %>% 
                              dplyr::filter(pH == "AMB", Process %notin% 
                                              c("calcifcation rate", "gross photosynthesis rate",
                                                "net photosynthesis rate", "dark respiration rate")))

# Dataset change
dataset_change  <- rbind(dataset_change_ELO, dataset_change_LOW, dataset_change_AMB)

##### Calcification from ELOW ----
# No calcifying organisms but small negligible calcification detected (avg < 5 units of Ak)
dataset_change$output[dataset_change$Process == "calcifcation rate" & 
                        dataset_change$Time == "T3" & dataset_change$pH == "ELOW"] = 0
dataset_change$output_std[dataset_change$Process == "calcifcation rate" & 
                            dataset_change$Time == "T3" & dataset_change$pH == "ELOW"] = 0

##### RATIO WITH T0 ----
dataset_change_init <- dataset_change %>% dplyr::filter(Time == "T0") %>% 
  rename(output_init = output, output_std_init = output_std, Time_init = Time) %>% 
  select(-c(nb_days, Time_init, PAR_intensity))
dataset_change      <- dataset_change %>% left_join(dataset_change_init, by = c("Tile", "pH", "Process")) %>% 
  mutate(change_std = output_std / output_std_init) 
dataset_change$change_std[dataset_change$change_std == Inf] = 0

dataset_change_tot = dataset_change %>% group_by(pH, Process, Time, nb_days) %>% 
  summarise(change_std_avg = mean(change_std), change_std_sd = sd(change_std))

dataset_change = dataset_change %>% mutate(pH = fct_relevel(pH, c("ELOW", "LOW", "AMB")))
dataset_change_viz = dataset_change
dataset_change_viz$change_std[dataset_change_viz$change_std > 10] = 10
dataset_change_viz$change_std[dataset_change_viz$change_std < -10] = -10
dataset_change_viz %>%  
  ggplot(aes(x = nb_days, y = change_std, fill = Process, group = nb_days)) + 
  geom_boxplot(outliers = FALSE) + 
  geom_jitter(color = "black", shape = 21, alpha = .7) +
  facet_grid(pH~Process)

##### Work with ranges
dataset_change = dataset_change %>% mutate(range = cut(change_std, breaks = c(-1000, -200, -100, seq(-10, 10, 1), 100, 200, 1000), 
                                                       include.lowest = T)) %>% arrange(range)
chosen_range   = data.frame(range           = unique(dataset_change$range),
                            arbitrary_value = seq(1, length(unique(dataset_change$range)), 1),
                            color           = values_grandient_color)
dataset_change = dataset_change %>% left_join(chosen_range) %>% arrange(Tile, Time)

dataset_change %>% dplyr::filter(Tile == "tile_18", Time == "T3") %>% 
  select(Tile, pH, Time, Process, change_std, range, arbitrary_value, color) %>% 
  ggplot() + geom_bar(aes(y = arbitrary_value, x = Process), stat = "identity", color = "black") +
  geom_hline(yintercept = 14, linetype = "dashed", color = "red") + 
  scale_fill_manual(aes(values = color)) +
  coord_polar(start = 0)
