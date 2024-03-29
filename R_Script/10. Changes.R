### Changes over time due to pH change
rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)

# Load data
Biomass   <- read_excel("Outputs/Summary/Biomass_Transplants_data.xlsx")
Functions <- read_excel("Outputs/Summary/Summary_Process_BenthFun.xlsx") %>% dplyr::filter(Main_Exp == "Transplants")
Nutrients <- read_excel("Outputs/Summary/Nutrients_Transplants_data.xlsx")

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

### Need to correct by the PAR!




# Compile the dataset
dataset_change      <- rbind(dataset_change_cal, dataset_change_npp, dataset_change_tot) %>% arrange(Tile) %>% 
  select(Tile, pH, Time, nb_days, Process, output, output_std)
dataset_change_init <- dataset_change %>% dplyr::filter(Time == "T0") %>% 
  rename(output_init = output, output_std_init = output_std, Time_init = Time) %>% select(-c(nb_days, Time_init))
dataset_change      <- dataset_change %>% left_join(dataset_change_init, by = c("Tile", "pH", "Process")) %>% 
  mutate(change_std = output_std / output_std_init) 

# No calcifying organisms but small calcification detected
dataset_change$change_std[dataset_change$change_std == Inf] = 0

dataset_change_tot = dataset_change %>% group_by(pH, Process, Time, nb_days) %>% 
  summarise(change_std_avg = mean(change_std), change_std_sd = sd(change_std))

unique(dataset_change_tot$Process)
dataset_change_tot %>% dplyr::filter(change_std_sd <= change_std_avg, Process == "net photosynthesis rate") %>% 
  ggplot(aes(x = nb_days, y = change_std_avg, shape = pH, color = Process)) + geom_point() + facet_wrap(~Process)
