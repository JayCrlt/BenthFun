### Changes over time due to pH change
rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)

# Load data
Biomass   <- read_excel("Outputs/Summary/Biomass_Transplants_data.xlsx")
Functions <- read_excel("Outputs/Summary/Summary_Process_BenthFun.xlsx") %>% dplyr::filter(Main_Exp == "Transplants")
Nutrients <- read_excel("Outputs/Summary/Nutrients_Transplants_data.xlsx")
PAR_tiles <- read_excel("Outputs/Summary/PAR_Transplants.xlsx") 

# Colors
values_grandient_color = c("#402d21", "#553c2c", "#704f3a", "#805b43", "#90664b", "#a07154", "#a67a5f",
                           "#af8266", "#ba947b", "#c2a18b", "#cbae9b", "#d3bbab", "#e4d5cc", "#efe7e1", 
                           "#ffffff", "#cee5c3", "#bbdbac", "#a4cf8f", "#9fcc8a", "#88c06d", "#7ab85c",
                           "#6cb04c", "#629f44", "#578e3d", "#497733", "#3f662c", "#314f22")

# Usefull functions
element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}
element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
    element$family <- element$hi.family %||% element$family
  }
  NextMethod()
}

# Fix some values
Functions$avg_output[Functions$Process == "dark respiration rate" & Functions$avg_output > 0] = 0
Functions$avg_output[Functions$Process == "net photosynthesis rate"] = 
  Functions$avg_output[Functions$Process == "gross photosynthesis rate"] -
  Functions$avg_output[Functions$Process == "dark respiration rate"]

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

##### Define according to communities
Comm = data.frame(Communities = c(rep("Mixed", 6), rep("forest", 6), rep("encrusting", 6)),
                  Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                           "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                           "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28"))
dataset_change = dataset_change %>% left_join(Comm)

##### Model functions
training_data <- expand.grid(nb_days = seq(0,130,.05),
                             Communities = c("Mixed", "forest", "encrusting"), pH = c("ELOW", "LOW", "AMB"))
# calcification
dataset_change_CR = dataset_change %>% dplyr::filter(Process == "calcifcation rate") %>% 
  mutate(change_std = abs(change_std) + 1e-26) %>% dplyr::filter(pH != "ELOW" | Time != "T3")
CR_model  <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) + 0, init = "0",
                 data = dataset_change_CR, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(CR_model) # R2 = 47%
training_data_CR = cbind(training_data, predict(CR_model, training_data))
# change back the sign
training_data_CR$Estimate[training_data_CR$pH == "ELOW"] = -training_data_CR$Estimate[training_data_CR$pH == "ELOW"]+2
(CR_plot = ggplot(training_data_CR, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point()+ scale_y_continuous(limits = c(-10,10)))

# Dark respiration
dataset_change_DR = dataset_change %>% dplyr::filter(Process == "dark respiration rate") %>% 
  mutate(change_std = abs(change_std) + 1e-26) %>% dplyr::filter(pH != "ELOW" | Time != "T2")
DR_model  <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) + 0, init = "0",
                 data = dataset_change_DR, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(DR_model) # R2 = 43%
training_data_DR = cbind(training_data, predict(DR_model, training_data))
(DR_plot = ggplot(training_data_DR, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point()+ scale_y_continuous(limits = c(0,10)))

# GPP
dataset_change_GPP = dataset_change %>% dplyr::filter(Process == "gross photosynthesis rate") %>% 
  mutate(change_std = abs(change_std) + 1e-26)
GPP_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_GPP, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(GPP_model) # R2 = 53%
training_data_GPP = cbind(training_data, predict(GPP_model, training_data))
(GPP_plot = ggplot(training_data_GPP, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(0,15)))

# NH3
dataset_change_NH3 = dataset_change %>% dplyr::filter(Process == "NH3") %>% 
  mutate(change_std = abs(change_std) + 1e-26) %>% dplyr::filter(pH != "ELOW" | Time != "T2")
NH3_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_NH3, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NH3_model) # R2 = 48%
training_data_NH3 = cbind(training_data, predict(NH3_model, training_data))
# change back the sign
training_data_NH3$Estimate[training_data_NH3$pH %in% c("LOW","ELOW")] = -training_data_NH3$Estimate[training_data_NH3$pH %in% c("LOW","ELOW")]+2
(NH3_plot = ggplot(training_data_NH3, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-15,15)))

# NO2
dataset_change_NO2 = dataset_change %>% dplyr::filter(Process == "NO2") %>% 
  mutate(change_std = abs(change_std) + 1e-26) %>% dplyr::filter(pH != "ELOW" | Time != "T2")
NO2_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_NO2, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NO2_model) # R2 = 57%
training_data_NO2 = cbind(training_data, predict(NO2_model, training_data))
# change back the sign
training_data_NO2$Estimate = -training_data_NO2$Estimate+2
(NO2_plot = ggplot(training_data_NO2, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-15,15)))

# NO3
dataset_change_NO3 = dataset_change %>% dplyr::filter(Process == "NO3") %>% 
  mutate(change_std = abs(change_std) + 1e-26) %>% dplyr::filter(pH != "ELOW" | Time != "T2")
NO3_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_NO3, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NO3_model) # R2 = 48%
training_data_NO3 = cbind(training_data, predict(NO3_model, training_data))
# change back the sign
training_data_NO3$Estimate[training_data_NO3$pH == "ELOW"] = -training_data_NO3$Estimate[training_data_NO3$pH == "ELOW"]+2
(NO3_plot = ggplot(training_data_NO3, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-25,25)))

# PO4
dataset_change_PO4 = dataset_change %>% dplyr::filter(Process == "PO4") %>% 
  mutate(change_std = abs(change_std) + 1e-26) 
PO4_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_PO4, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(PO4_model) # R2 = 46%
training_data_PO4 = cbind(training_data, predict(PO4_model, training_data))
# change back the sign
training_data_PO4$Estimate[training_data_PO4$pH == "ELOW"] = -training_data_PO4$Estimate[training_data_PO4$pH == "ELOW"]+2
(PO4_plot = ggplot(training_data_PO4, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-25,25)))

# SiO4
dataset_change_SiO4 = dataset_change %>% dplyr::filter(Process == "SiO4") %>% 
  mutate(change_std = abs(change_std) + 1e-26) 
SiO4_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                  data = dataset_change_SiO4, family = weibull(), cores = 4, chains = 4, iter = 10000,
                  warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(SiO4_model) # R2 = 45%
training_data_SiO4 = cbind(training_data, predict(SiO4_model, training_data))
# change back the sign
training_data_SiO4$Estimate = -training_data_SiO4$Estimate + 2
(SiO4_plot = ggplot(training_data_SiO4, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-25,25)))

# Merge everything
data_model <- rbind(training_data_CR, training_data_DR, training_data_GPP, training_data_NH3, training_data_NO2,
      training_data_NO3, training_data_PO4, training_data_SiO4) %>% 
  mutate(Function = c(rep("CR", 23409), rep("DR", 23409), rep("GPP", 23409), rep("NH3", 23409), 
                      rep("NO2", 23409), rep("NO3", 23409), rep("PO4", 23409), rep("SiO4", 23409)))

data_model = data_model %>% mutate(ribbon_neg = Estimate - Est.Error,
                                   ribbon_pos = Estimate + Est.Error)

data_model %>% # dplyr::filter(Communities == "Mixed") %>% 
  ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
  #geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos)) +
  geom_point(aes(shape = Communities), size = 1) + 
  facet_grid(Communities~Function) + 
  scale_x_continuous(name = "number of days", limits = c(0,100)) +
  scale_y_continuous(name = "factor_of_change", limits = c(-50,50)) 
