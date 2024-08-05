### Changes over time due to pH change
options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
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
  left_join(Biomass %>% dplyr::select(-c(Biomass_overall, Biomass_npp, 
                                         Biomass_std_overall, Biomass_std_cal, Biomass_std_npp)),
            by = c("Tile", "pH", "Time")) %>% mutate(output_std = output / Biomass_cal) %>% select(-Biomass_cal)
dataset_change_npp <- dataset_change %>% dplyr::filter(Process == "gross photosynthesis rate") %>% 
  left_join(Biomass %>% dplyr::select(-c(Biomass_overall, Biomass_cal, 
                                         Biomass_std_overall, Biomass_std_cal, Biomass_std_npp)),
            by = c("Tile", "pH", "Time")) %>% mutate(output_std = output / Biomass_npp) %>% select(-Biomass_npp)
dataset_change_tot <- dataset_change %>% dplyr::filter(Process %notin% c("calcifcation rate", "gross photosynthesis rate")) %>% 
  left_join(Biomass %>% dplyr::select(-c(Biomass_cal, Biomass_npp, 
                                         Biomass_std_overall, Biomass_std_cal, Biomass_std_npp)),
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
  mutate(change_std = abs(change_std) + 1e-26) #%>% dplyr::filter(pH != "ELOW" | Time != "T2")
NH3_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_NH3, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NH3_model) # R2 = 48%
training_data_NH3 = cbind(training_data, predict(NH3_model, training_data))
# change back the sign
training_data_NH3$Estimate = -training_data_NH3$Estimate 
(NH3_plot = ggplot(training_data_NH3, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-15,15)))

# NO3
dataset_change_NO3 = dataset_change %>% dplyr::filter(Process == "NO3") %>% 
  mutate(change_std = abs(change_std) + 1e-26) #%>% dplyr::filter(pH != "AMB" | Time != "T3")
NO3_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_NO3, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NO3_model) # R2 = 48%
training_data_NO3 = cbind(training_data, predict(NO3_model, training_data))
# change back the sign
training_data_NO3$Estimate = -training_data_NO3$Estimate
(NO3_plot = ggplot(training_data_NO3, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-10,10)))

# PO4
dataset_change_PO4 = dataset_change %>% dplyr::filter(Process == "PO4") %>% 
  mutate(change_std = abs(change_std) + 1e-26) 
PO4_model <- brm(change_std ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) +0, init = "0",
                 data = dataset_change_PO4, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(PO4_model) # R2 = 46%
training_data_PO4 = cbind(training_data, predict(PO4_model, training_data))
# change back the sign
training_data_PO4$Estimate = -training_data_PO4$Estimate
(PO4_plot = ggplot(training_data_PO4, aes(y = Estimate, x = nb_days, color = pH, shape = Communities)) + 
    geom_point() + scale_y_continuous(limits = c(-25,25)))

# Merge everything
data_model <- rbind(training_data_CR, training_data_DR, training_data_GPP, training_data_NH3, 
      training_data_NO3, training_data_PO4) %>% 
  mutate(Function = c(rep("CR", 23409), rep("DR", 23409), rep("GPP", 23409), rep("NH3", 23409), 
                      rep("NO3", 23409), rep("PO4", 23409)))

data_model = data_model %>% mutate(ribbon_neg = Estimate - Est.Error,
                                   ribbon_pos = Estimate + Est.Error)

data_model %>% # dplyr::filter(Communities == "Mixed") %>% 
  ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
  #geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos)) +
  geom_point(aes(shape = Communities), size = 1) + 
  facet_grid(Communities~Function) + 
  scale_x_continuous(name = "number of days", limits = c(0,100)) +
  scale_y_continuous(name = "factor_of_change", limits = c(-5,5)) 

####################################################
##### Split it according to brandl et al. 2018 #####
####################################################

###### Panel 1 ----
### CR ---
process = "calcifcation rate"
cste = 3.7 # Volume of the chamber
Sa = 1     # If we do not want std by area
T0 <- dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T0 = mean(output_std) * cste / Sa, sd_T0 = sd(output_std) * cste) 
T3 <- dataset_change %>% dplyr::filter(Process == process, Time == "T3") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T3 = mean(output_std) * cste / Sa, sd_T3 = sd(output_std) * cste) 
T0 %>% full_join(T3) %>% mutate(ratio = mean_T3/mean_T0) # Check the ratios in details
(T0 = dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
    group_by(Communities) %>% 
    summarise(mean_T0 = mean(output_std * cste / Sa)))

CR_Process <- data_model %>% dplyr::filter(Function %in% c("CR")) %>% 
  mutate(ribbon_neg = Estimate - Est.Error,
         ribbon_pos = ribbon_neg + 1.7*Est.Error) %>%
  mutate(across(c(Estimate, ribbon_neg, ribbon_pos),
                ~ case_when(Communities == "forest" ~ . * T0$mean_T0[3]/max(T0$mean_T0),
                            Communities == "Mixed" ~ . * T0$mean_T0[1]/max(T0$mean_T0),
                            Communities == "encrusting" ~ . * T0$mean_T0[2]/max(T0$mean_T0),
                            TRUE ~ .)))

# Crop ribbon polygon
CR_Process$ribbon_neg[CR_Process$ribbon_neg <= -5] = -5
CR_Process$ribbon_pos[CR_Process$ribbon_pos <= -5] = -5
CR_Process$ribbon_neg[CR_Process$ribbon_neg >= 5] = 5
CR_Process$ribbon_pos[CR_Process$ribbon_pos >= 5] = 5
min_day = CR_Process %>% dplyr::filter(nb_days <= 50 | pH != "ELOW", ribbon_pos <=-5) %>%
  group_by(Communities) %>% summarise(min = min(nb_days)) %>% summarise(min = max(min))

# Figure Panel 1
(Panel_1_CR <- CR_Process %>% dplyr::filter(ribbon_neg < 5 | pH %notin% c("LOW", "AMB"),
                                            ribbon_pos > -5 | pH %notin% c("ELOW")) %>% 
    mutate(Communities = fct_relevel(Communities, c("forest", "Mixed", "encrusting"))) %>% 
    dplyr::filter(nb_days <= min_day$min | pH != "ELOW") %>% 
    ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
    geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos, fill = pH), alpha = .5) +
    geom_point(size = 1) + 
    facet_grid(Communities~Function) + 
    scale_x_continuous(name = "Number of days", limits = c(0,100)) +
    #geom_segment(aes(x = 0, y = 1, xend = 100, yend = 1), colour = "black", linetype = "dotted", size = .5) +
    scale_y_continuous(name = "Factor of change", limits = c(-5,5)) +
    scale_color_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    scale_fill_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    theme_classic() + ggtitle("1. Calcification Rate") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))

###### Panel 2 ----
### DR ---

DR_Process <- data_model %>% dplyr::filter(Function %in% c("DR")) 
# Over-fitting for the figure – Only! i.e., not used for interpretations
DR_model2  <- brm(Estimate ~ (nb_days + 0 | Communities) + (nb_days + 0 | pH) + 0, init = "0",
                  data = DR_Process, family = weibull(), cores = 3, chains = 3, iter = 5000,
                  warmup = 1000, control = list(adapt_delta = 0.9, max_treedepth = 5))
bayes_R2(DR_model2) # R2 = 63% vs. 43% previously
training_data_DR = cbind(training_data, predict(DR_model2, training_data))

process = "dark respiration rate"
cste = 113.2469 # from mg/L to umol/L
T0 <- dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T0 = mean(output_std) * cste / Sa, sd_T0 = sd(output_std) * cste) 
T3 <- dataset_change %>% dplyr::filter(Process == process, Time == "T3") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T3 = mean(output_std) * cste / Sa, sd_T3 = sd(output_std) * cste) 
T0 %>% full_join(T3) %>% mutate(ratio = mean_T3/mean_T0) # Check the ratios in details
(T0 = dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
    group_by(Communities) %>% 
    summarise(mean_T0 = -mean(output_std * cste / Sa)) )

# Convert values to be on the same scale
DR_Process = training_data_DR %>% 
  mutate(Function = rep("DR", 23409), ribbon_neg = Estimate - Est.Error,
         ribbon_pos = Estimate + Est.Error) %>% 
  mutate(across(c(Estimate, ribbon_neg, ribbon_pos),
                ~ case_when(Communities == "forest" ~ . * T0$mean_T0[3]/max(T0$mean_T0),
                            Communities == "Mixed" ~ . * T0$mean_T0[1]/max(T0$mean_T0),
                            Communities == "encrusting" ~ . * T0$mean_T0[2]/max(T0$mean_T0),
                            T ~ .)))

# Crop ribbon polygon
DR_Process$ribbon_neg[DR_Process$ribbon_neg <= -5] = -5
DR_Process$ribbon_pos[DR_Process$ribbon_pos <= -5] = -5
DR_Process$ribbon_neg[DR_Process$ribbon_neg >= 5] = 5
DR_Process$ribbon_pos[DR_Process$ribbon_pos >= 5] = 5

# Figure Panel 2A
(Panel_2_DR <- DR_Process %>% dplyr::filter(ribbon_neg < 5) %>% 
    mutate(Communities = fct_relevel(Communities, c("forest", "Mixed", "encrusting"))) %>% 
    ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
    geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos, fill = pH), alpha = .5) +
    geom_point(size = 1) + 
    facet_grid(Communities~Function) + 
    scale_x_continuous(name = "Number of days", limits = c(0,100)) +
    #geom_segment(aes(x = 0, y = 1, xend = 100, yend = 1), colour = "black", linetype = "dotted", size = .5) +
    scale_y_continuous(name = "Factor of change", limits = c(-5,5)) +
    scale_color_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    scale_fill_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    theme_classic() + ggtitle("2. Respiration Rate") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))

### GPP ---
process = "gross photosynthesis rate"
cste = 113.2469 # from mg/L to umol/L
T0 <- dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T0 = mean(output_std) * cste / Sa, sd_T0 = sd(output_std) * cste) 
T3 <- dataset_change %>% dplyr::filter(Process == process, Time == "T3") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T3 = mean(output_std) * cste / Sa, sd_T3 = sd(output_std) * cste) 
T0 %>% full_join(T3) %>% mutate(ratio = mean_T3/mean_T0) # Check the ratios in details
(T0 = dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
    group_by(Communities) %>% 
    summarise(mean_T0 = mean(output_std * cste / Sa)) )

# Convert values to be on the same scale
GPP_Process <- data_model %>% dplyr::filter(Function %in% c("GPP")) %>% 
  mutate(ribbon_neg = Estimate - Est.Error,
         ribbon_pos = ribbon_neg + 2*Est.Error) %>%
  mutate(across(c(Estimate, ribbon_neg, ribbon_pos),
                ~ case_when(Communities == "forest" ~ . * T0$mean_T0[3]/max(T0$mean_T0),
                            Communities == "Mixed" ~ . * T0$mean_T0[1]/max(T0$mean_T0),
                            Communities == "encrusting" ~ . * T0$mean_T0[2]/max(T0$mean_T0),
                            T ~ .)))

# Crop ribbon polygon
GPP_Process$ribbon_neg[GPP_Process$ribbon_neg <= -5] = -5
GPP_Process$ribbon_pos[GPP_Process$ribbon_pos <= -5] = -5
GPP_Process$ribbon_neg[GPP_Process$ribbon_neg >= 5] = 5
GPP_Process$ribbon_pos[GPP_Process$ribbon_pos >= 5] = 5

# Figure Panel 2B
(Panel_2_GPP <- GPP_Process %>% dplyr::filter(ribbon_neg < 5) %>% 
    mutate(Communities = fct_relevel(Communities, c("forest", "Mixed", "encrusting"))) %>% 
    ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
    geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos, fill = pH), alpha = .5) +
    geom_point(size = 1) + 
    facet_grid(Communities~Function) + 
    scale_x_continuous(name = "Number of days", limits = c(0,100)) +
    #geom_segment(aes(x = 0, y = 1, xend = 100, yend = 1), colour = "black", linetype = "dotted", size = .5) +
    scale_y_continuous(name = "", limits = c(-5,5)) +
    scale_color_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    scale_fill_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    theme_classic() + ggtitle("3. Gross Primary Production") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))

###### Panel 3 ----
### NH4 ---
process = "NH3"
cste = 3.7
T0 <- dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T0 = mean(output_std) * cste, sd_T0 = sd(output_std) * cste) 
T3 <- dataset_change %>% dplyr::filter(Process == process, Time == "T3") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T3 = mean(output_std) * cste, sd_T3 = sd(output_std) * cste) 
T0 %>% full_join(T3) %>% mutate(ratio = mean_T3/mean_T0) # Check the ratios in details
(T0 = dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
    group_by(Communities) %>% 
    summarise(mean_T0 = mean(output_std * cste)) )

# Convert values to be on the same scale
NH4_Process <- data_model %>% dplyr::filter(Function %in% c("NH3")) %>% 
  mutate(ribbon_neg = Estimate - Est.Error,
         ribbon_pos = ribbon_neg + 1.4*Est.Error) %>%
  mutate(across(c(Estimate, ribbon_neg, ribbon_pos),
                ~ case_when(Communities == "forest" ~ . * T0$mean_T0[3]/min(T0$mean_T0),
                            Communities == "Mixed" ~ . * T0$mean_T0[1]/min(T0$mean_T0),
                            Communities == "encrusting" ~ . * T0$mean_T0[2]/min(T0$mean_T0),
                            TRUE ~ .)))

# ELOW CI upper part()
NH4_Process_CI = NH4_Process %>% dplyr::filter(pH == "ELOW", nb_days < 50) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[1]]), 
                  NH4_Process %>% dplyr::filter(pH == "ELOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[2]]), 
                  NH4_Process %>% dplyr::filter(pH == "ELOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[3]]), 
                  NH4_Process %>% dplyr::filter(pH == "ELOW", Communities == "encrusting")) %>% as.numeric()

NH4_Process$ribbon_pos[NH4_Process$pH == "ELOW" & NH4_Process$Communities == "Mixed"] = CI_UP_1
NH4_Process$ribbon_pos[NH4_Process$pH == "ELOW" & NH4_Process$Communities == "forest"] = CI_UP_2
NH4_Process$ribbon_pos[NH4_Process$pH == "ELOW" & NH4_Process$Communities == "encrusting"] = CI_UP_3

# LOW CI upper part()
NH4_Process_CI = NH4_Process %>% dplyr::filter(pH == "LOW", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[1]]), 
                  NH4_Process %>% dplyr::filter(pH == "LOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[2]]), 
                  NH4_Process %>% dplyr::filter(pH == "LOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[3]]), 
                  NH4_Process %>% dplyr::filter(pH == "LOW", Communities == "encrusting")) %>% as.numeric()

NH4_Process$ribbon_pos[NH4_Process$pH == "LOW" & NH4_Process$Communities == "Mixed"] = CI_UP_1
NH4_Process$ribbon_pos[NH4_Process$pH == "LOW" & NH4_Process$Communities == "forest"] = CI_UP_2
NH4_Process$ribbon_pos[NH4_Process$pH == "LOW" & NH4_Process$Communities == "encrusting"] = CI_UP_3

# LOW CI below part()
NH4_Process_CI = NH4_Process %>% dplyr::filter(pH == "LOW", nb_days < 100) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NH4_Process_CI[[1]]), 
                  NH4_Process %>% dplyr::filter(pH == "LOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NH4_Process_CI[[2]]), 
                  NH4_Process %>% dplyr::filter(pH == "LOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NH4_Process_CI[[3]]), 
                  NH4_Process %>% dplyr::filter(pH == "LOW", Communities == "encrusting")) %>% as.numeric()

NH4_Process$ribbon_neg[NH4_Process$pH == "LOW" & NH4_Process$Communities == "Mixed"] = CI_UP_1
NH4_Process$ribbon_neg[NH4_Process$pH == "LOW" & NH4_Process$Communities == "forest"] = CI_UP_2
NH4_Process$ribbon_neg[NH4_Process$pH == "LOW" & NH4_Process$Communities == "encrusting"] = CI_UP_3

# AMB CI upper part()
NH4_Process_CI = NH4_Process %>% dplyr::filter(pH == "AMB", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[1]]), 
                  NH4_Process %>% dplyr::filter(pH == "AMB", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[2]]), 
                  NH4_Process %>% dplyr::filter(pH == "AMB", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NH4_Process_CI[[3]]), 
                  NH4_Process %>% dplyr::filter(pH == "AMB", Communities == "encrusting")) %>% as.numeric()

NH4_Process$ribbon_pos[NH4_Process$pH == "AMB" & NH4_Process$Communities == "Mixed"] = CI_UP_1
NH4_Process$ribbon_pos[NH4_Process$pH == "AMB" & NH4_Process$Communities == "forest"] = CI_UP_2
NH4_Process$ribbon_pos[NH4_Process$pH == "AMB" & NH4_Process$Communities == "encrusting"] = CI_UP_3

# AMB CI below part()
NH4_Process_CI = NH4_Process %>% dplyr::filter(pH == "AMB", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NH4_Process_CI[[1]]), 
                  NH4_Process %>% dplyr::filter(pH == "AMB", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NH4_Process_CI[[2]]), 
                  NH4_Process %>% dplyr::filter(pH == "AMB", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NH4_Process_CI[[3]]), 
                  NH4_Process %>% dplyr::filter(pH == "AMB", Communities == "encrusting")) %>% as.numeric()

NH4_Process$ribbon_neg[NH4_Process$pH == "AMB" & NH4_Process$Communities == "Mixed"] = CI_UP_1
NH4_Process$ribbon_neg[NH4_Process$pH == "AMB" & NH4_Process$Communities == "forest"] = CI_UP_2
NH4_Process$ribbon_neg[NH4_Process$pH == "AMB" & NH4_Process$Communities == "encrusting"] = CI_UP_3

# Crop ribbon polygon
NH4_Process$ribbon_neg[NH4_Process$ribbon_neg <= -5] = -5
NH4_Process$ribbon_pos[NH4_Process$ribbon_pos <= -5] = -5
NH4_Process$ribbon_neg[NH4_Process$ribbon_neg >= 5] = 5
NH4_Process$ribbon_pos[NH4_Process$ribbon_pos >= 5] = 5

# Figure Panel 3A
(Panel_3_NH4 <- NH4_Process %>% dplyr::filter(ribbon_pos > -5) %>% 
    mutate(Communities = fct_relevel(Communities, c("forest", "Mixed", "encrusting")),
           pH = fct_relevel(pH, c("ELOW", "AMB", "LOW"))) %>%  
    ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
    geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos, fill = pH), alpha = .5) +
    geom_point(size = 1) + 
    facet_grid(Communities~Function) + 
    scale_x_continuous(name = "Number of days", limits = c(0,100)) +
    #geom_segment(aes(x = 0, y = 1, xend = 100, yend = 1), colour = "black", linetype = "dotted", size = .5) +
    scale_y_continuous(name = "Factor of change", limits = c(-5,5)) +
    scale_color_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    scale_fill_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    theme_classic() + ggtitle("4. NH4") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))

### NO3 ---
process = "NO3"
cste = 3.7
T0 <- dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T0 = mean(output_std) * cste, sd_T0 = sd(output_std) * cste) 
T3 <- dataset_change %>% dplyr::filter(Process == process, Time == "T3") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T3 = mean(output_std) * cste, sd_T3 = sd(output_std) * cste) 
T0 %>% full_join(T3) %>% mutate(ratio = mean_T3/mean_T0) # Check the ratios in details
(T0 = dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
    group_by(Communities) %>% 
    summarise(mean_T0 = mean(output_std * cste)) )

# Convert values to be on the same scale
NO3_Process <- data_model %>% dplyr::filter(Function %in% c("NO3")) %>% 
  mutate(ribbon_neg = Estimate - Est.Error,
         ribbon_pos = ribbon_neg + 1.4*Est.Error) %>% 
  mutate(across(c(Estimate, ribbon_neg, ribbon_pos),
                ~ case_when(Communities == "forest" ~ . * T0$mean_T0[3]/min(T0$mean_T0),
                            Communities == "Mixed" ~ . * T0$mean_T0[1]/min(T0$mean_T0),
                            Communities == "encrusting" ~ . * T0$mean_T0[2]/min(T0$mean_T0),
                            T ~ .)))

# ELOW CI upper part()
NO3_Process_CI = NO3_Process %>% dplyr::filter(pH == "ELOW", nb_days < 50) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[1]]), 
                  NO3_Process %>% dplyr::filter(pH == "ELOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[2]]), 
                  NO3_Process %>% dplyr::filter(pH == "ELOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[3]]), 
                  NO3_Process %>% dplyr::filter(pH == "ELOW", Communities == "encrusting")) %>% as.numeric()

NO3_Process$ribbon_pos[NO3_Process$pH == "ELOW" & NO3_Process$Communities == "Mixed"] = CI_UP_1
NO3_Process$ribbon_pos[NO3_Process$pH == "ELOW" & NO3_Process$Communities == "forest"] = CI_UP_2
NO3_Process$ribbon_pos[NO3_Process$pH == "ELOW" & NO3_Process$Communities == "encrusting"] = CI_UP_3

# LOW CI upper part()
NO3_Process_CI = NO3_Process %>% dplyr::filter(pH == "LOW", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[1]]), 
                  NO3_Process %>% dplyr::filter(pH == "LOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[2]]), 
                  NO3_Process %>% dplyr::filter(pH == "LOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[3]]), 
                  NO3_Process %>% dplyr::filter(pH == "LOW", Communities == "encrusting")) %>% as.numeric()

NO3_Process$ribbon_pos[NO3_Process$pH == "LOW" & NO3_Process$Communities == "Mixed"] = CI_UP_1
NO3_Process$ribbon_pos[NO3_Process$pH == "LOW" & NO3_Process$Communities == "forest"] = CI_UP_2
NO3_Process$ribbon_pos[NO3_Process$pH == "LOW" & NO3_Process$Communities == "encrusting"] = CI_UP_3

# LOW CI below part()
NO3_Process_CI = NO3_Process %>% dplyr::filter(pH == "LOW", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NO3_Process_CI[[1]]), 
                  NO3_Process %>% dplyr::filter(pH == "LOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NO3_Process_CI[[2]]), 
                  NO3_Process %>% dplyr::filter(pH == "LOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NO3_Process_CI[[3]]), 
                  NO3_Process %>% dplyr::filter(pH == "LOW", Communities == "encrusting")) %>% as.numeric()

NO3_Process$ribbon_neg[NO3_Process$pH == "LOW" & NO3_Process$Communities == "Mixed"] = CI_UP_1
NO3_Process$ribbon_neg[NO3_Process$pH == "LOW" & NO3_Process$Communities == "forest"] = CI_UP_2
NO3_Process$ribbon_neg[NO3_Process$pH == "LOW" & NO3_Process$Communities == "encrusting"] = CI_UP_3

# AMB CI upper part()
NO3_Process_CI = NO3_Process %>% dplyr::filter(pH == "AMB", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[1]]), 
                  NO3_Process %>% dplyr::filter(pH == "AMB", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[2]]), 
                  NO3_Process %>% dplyr::filter(pH == "AMB", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = NO3_Process_CI[[3]]), 
                  NO3_Process %>% dplyr::filter(pH == "AMB", Communities == "encrusting")) %>% as.numeric()

NO3_Process$ribbon_pos[NO3_Process$pH == "AMB" & NO3_Process$Communities == "Mixed"] = CI_UP_1
NO3_Process$ribbon_pos[NO3_Process$pH == "AMB" & NO3_Process$Communities == "forest"] = CI_UP_2
NO3_Process$ribbon_pos[NO3_Process$pH == "AMB" & NO3_Process$Communities == "encrusting"] = CI_UP_3

# AMB CI below part()
NO3_Process_CI = NO3_Process %>% dplyr::filter(pH == "AMB", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NO3_Process_CI[[1]]), 
                  NO3_Process %>% dplyr::filter(pH == "AMB", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NO3_Process_CI[[2]]), 
                  NO3_Process %>% dplyr::filter(pH == "AMB", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = NO3_Process_CI[[3]]), 
                  NO3_Process %>% dplyr::filter(pH == "AMB", Communities == "encrusting")) %>% as.numeric()

NO3_Process$ribbon_neg[NO3_Process$pH == "AMB" & NO3_Process$Communities == "Mixed"] = CI_UP_1
NO3_Process$ribbon_neg[NO3_Process$pH == "AMB" & NO3_Process$Communities == "forest"] = CI_UP_2
NO3_Process$ribbon_neg[NO3_Process$pH == "AMB" & NO3_Process$Communities == "encrusting"] = CI_UP_3

# Crop ribbon polygon
NO3_Process$ribbon_neg[NO3_Process$ribbon_neg <= -5] = -5
NO3_Process$ribbon_pos[NO3_Process$ribbon_pos <= -5] = -5
NO3_Process$ribbon_neg[NO3_Process$ribbon_neg >= 5] = 5
NO3_Process$ribbon_pos[NO3_Process$ribbon_pos >= 5] = 5

# Figure Panel 3B
(Panel_3_NO3 <- NO3_Process %>% dplyr::filter(ribbon_pos > -5) %>% 
    mutate(Communities = fct_relevel(Communities, c("forest", "Mixed", "encrusting")),
           pH = fct_relevel(pH, c("ELOW", "AMB", "LOW"))) %>%  
    ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
    geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos, fill = pH), alpha = .5) +
    geom_point(size = 1) + 
    facet_grid(Communities~Function) + 
    scale_x_continuous(name = "Number of days", limits = c(0,100)) +
    #geom_segment(aes(x = 0, y = 1, xend = 100, yend = 1), colour = "black", linetype = "dotted", size = .5) +
    scale_y_continuous(name = "", limits = c(-5,5)) +
    scale_color_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    scale_fill_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    theme_classic() + ggtitle("5. NO3") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))

### PO4 ---
process = "PO4"
cste = 3.7
T0 <- dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T0 = mean(output_std) * cste, sd_T0 = sd(output_std) * cste) 
T3 <- dataset_change %>% dplyr::filter(Process == process, Time == "T3") %>% drop_na() %>% 
  mutate(output_std = output_std) %>% group_by(Communities, pH) %>% 
  summarise(mean_T3 = mean(output_std) * cste, sd_T3 = sd(output_std) * cste) 
T0 %>% full_join(T3) %>% mutate(ratio = mean_T3/mean_T0) # Check the ratios in details
(T0 = dataset_change %>% dplyr::filter(Process == process, Time == "T0") %>% drop_na() %>% 
    group_by(Communities) %>% 
    summarise(mean_T0 = mean(output_std * cste)) )

# Convert values to be on the same scale
PO4_Process <- data_model %>% dplyr::filter(Function %in% c("PO4")) %>% 
  mutate(ribbon_neg = Estimate - Est.Error,
         ribbon_pos = ribbon_neg + 1.4*Est.Error) %>% 
  mutate(across(c(Estimate, ribbon_neg, ribbon_pos),
                ~ case_when(Communities == "forest" ~ . * T0$mean_T0[3]/min(T0$mean_T0),
                            Communities == "Mixed" ~ . * T0$mean_T0[1]/min(T0$mean_T0),
                            Communities == "encrusting" ~ . * T0$mean_T0[2]/min(T0$mean_T0),
                            T ~ .)))

# ELOW CI upper part()
PO4_Process_CI = PO4_Process %>% dplyr::filter(pH == "ELOW", nb_days < 50) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[1]]), 
                  PO4_Process %>% dplyr::filter(pH == "ELOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[2]]), 
                  PO4_Process %>% dplyr::filter(pH == "ELOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[3]]), 
                  PO4_Process %>% dplyr::filter(pH == "ELOW", Communities == "encrusting")) %>% as.numeric()

PO4_Process$ribbon_pos[PO4_Process$pH == "ELOW" & PO4_Process$Communities == "Mixed"] = CI_UP_1
PO4_Process$ribbon_pos[PO4_Process$pH == "ELOW" & PO4_Process$Communities == "forest"] = CI_UP_2
PO4_Process$ribbon_pos[PO4_Process$pH == "ELOW" & PO4_Process$Communities == "encrusting"] = CI_UP_3

# LOW CI upper part()
PO4_Process_CI = PO4_Process %>% dplyr::filter(pH == "LOW", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[1]]), 
                  PO4_Process %>% dplyr::filter(pH == "LOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[2]]), 
                  PO4_Process %>% dplyr::filter(pH == "LOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[3]]), 
                  PO4_Process %>% dplyr::filter(pH == "LOW", Communities == "encrusting")) %>% as.numeric()

PO4_Process$ribbon_pos[PO4_Process$pH == "LOW" & PO4_Process$Communities == "Mixed"] = CI_UP_1
PO4_Process$ribbon_pos[PO4_Process$pH == "LOW" & PO4_Process$Communities == "forest"] = CI_UP_2
PO4_Process$ribbon_pos[PO4_Process$pH == "LOW" & PO4_Process$Communities == "encrusting"] = CI_UP_3

# LOW CI below part()
PO4_Process_CI = PO4_Process %>% dplyr::filter(pH == "LOW", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = PO4_Process_CI[[1]]), 
                  PO4_Process %>% dplyr::filter(pH == "LOW", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = PO4_Process_CI[[2]]), 
                  PO4_Process %>% dplyr::filter(pH == "LOW", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = PO4_Process_CI[[3]]), 
                  PO4_Process %>% dplyr::filter(pH == "LOW", Communities == "encrusting")) %>% as.numeric()

PO4_Process$ribbon_neg[PO4_Process$pH == "LOW" & PO4_Process$Communities == "Mixed"] = CI_UP_1
PO4_Process$ribbon_neg[PO4_Process$pH == "LOW" & PO4_Process$Communities == "forest"] = CI_UP_2
PO4_Process$ribbon_neg[PO4_Process$pH == "LOW" & PO4_Process$Communities == "encrusting"] = CI_UP_3

# AMB CI upper part()
PO4_Process_CI = PO4_Process %>% dplyr::filter(pH == "AMB", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[1]]), 
                  PO4_Process %>% dplyr::filter(pH == "AMB", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[2]]), 
                  PO4_Process %>% dplyr::filter(pH == "AMB", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_pos ~ s(nb_days), data = PO4_Process_CI[[3]]), 
                  PO4_Process %>% dplyr::filter(pH == "AMB", Communities == "encrusting")) %>% as.numeric()

PO4_Process$ribbon_pos[PO4_Process$pH == "AMB" & PO4_Process$Communities == "Mixed"] = CI_UP_1
PO4_Process$ribbon_pos[PO4_Process$pH == "AMB" & PO4_Process$Communities == "forest"] = CI_UP_2
PO4_Process$ribbon_pos[PO4_Process$pH == "AMB" & PO4_Process$Communities == "encrusting"] = CI_UP_3

# AMB CI below part()
PO4_Process_CI = PO4_Process %>% dplyr::filter(pH == "AMB", nb_days < 60) %>% 
  group_by(Communities) %>% group_split()
CI_UP_1 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = PO4_Process_CI[[1]]), 
                  PO4_Process %>% dplyr::filter(pH == "AMB", Communities == "Mixed")) %>% as.numeric()
CI_UP_2 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = PO4_Process_CI[[2]]), 
                  PO4_Process %>% dplyr::filter(pH == "AMB", Communities == "forest")) %>% as.numeric()
CI_UP_3 = predict(mgcv::gam(ribbon_neg ~ s(nb_days), data = PO4_Process_CI[[3]]), 
                  PO4_Process %>% dplyr::filter(pH == "AMB", Communities == "encrusting")) %>% as.numeric()

PO4_Process$ribbon_neg[PO4_Process$pH == "AMB" & PO4_Process$Communities == "Mixed"] = CI_UP_1
PO4_Process$ribbon_neg[PO4_Process$pH == "AMB" & PO4_Process$Communities == "forest"] = CI_UP_2
PO4_Process$ribbon_neg[PO4_Process$pH == "AMB" & PO4_Process$Communities == "encrusting"] = CI_UP_3

# Crop ribbon polygon
PO4_Process$ribbon_neg[PO4_Process$ribbon_neg <= -5] = -5
PO4_Process$ribbon_pos[PO4_Process$ribbon_pos <= -5] = -5
PO4_Process$ribbon_neg[PO4_Process$ribbon_neg >= 5] = 5
PO4_Process$ribbon_pos[PO4_Process$ribbon_pos >= 5] = 5

# Figure Panel 3C
(Panel_3_PO4 <- PO4_Process %>% dplyr::filter(ribbon_neg < 5 | pH %notin% c("LOW", "AMB"),
                                              ribbon_pos > -5 | pH %notin% c("ELOW")) %>% 
    mutate(Communities = fct_relevel(Communities, c("forest", "Mixed", "encrusting")),
           pH = fct_relevel(pH, c("ELOW", "AMB", "LOW"))) %>% 
    ggplot(aes(x = nb_days, y = Estimate, color = pH)) + 
    geom_ribbon(aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos, fill = pH), alpha = .5, show.legend = F) +
    geom_point(size = 1, show.legend = F) + 
    facet_grid(Communities~Function) + 
    scale_x_continuous(name = "Number of days", limits = c(0,100)) +
    #geom_segment(aes(x = 0, y = 1, xend = 100, yend = 1), colour = "black", linetype = "dotted", size = .5) +
    scale_y_continuous(name = "", limits = c(-5,5)) +
    scale_color_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    scale_fill_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
    theme_classic() + ggtitle("6. PO4") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))

### Final Plot std by biomass ---

Functions_Communities <- Panel_1_CR + plot_spacer() + Panel_2_DR + Panel_2_GPP + plot_spacer() + 
  Panel_3_NH4 + Panel_3_NO3 + Panel_3_PO4 + plot_layout(nrow = 1, widths = c(3,1,3,3,1,3,3,3), guides = "collect") &
  theme(legend.position = "bottom")

### Save dataset
Transplants_Changes_Summary <- rbind(CR_Process, DR_Process, GPP_Process, NH4_Process, NO3_Process, PO4_Process)
# openxlsx::write.xlsx(Transplants_Changes_Summary, file = "Outputs/Summary/Transplants_Changes_model.xlsx", rowNames = FALSE)

ggsave(Functions_Communities, file = "Outputs/Figures/Processes_Panels/Functions_3.png", width = 42, 
       height = 16, units = "cm", dpi = 300)