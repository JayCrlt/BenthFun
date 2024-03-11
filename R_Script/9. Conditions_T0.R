options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

## Datasets
source("R_Script/3. Alkalinity.R")
Alkalinity_dataset_T0 = Alkalinity_dataset %>% dplyr::filter(`Stage incubation` == "t0") %>% 
  select(`pH condition`, `Stage experiment`, `analysis date`, correction, `Batch sd`) %>% 
  arrange(`Stage experiment`)

Tile_concerned <- read_excel("Data/1. Diving log/Diving_log_BenthFun.xlsx", 
                             col_types = c("date", "text", "text", "date", "date", 
                                           "date", "text", "text", "numeric", "numeric"),
                             sheet = "Corrected") %>% 
  mutate(., Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S")) %>% 
  dplyr::filter(is.na(`Tile_N°`)) %>% dplyr::select(c(Diving_Date, Label, `Tile_N°`, Start_incubation, Stop_Alkalinity, Temperature, pH_mV))

Nutrients <- read_excel("Data/6. Nutrients/Nutrients.xlsx", sheet = "Sheet1") %>% select(-c(`DATA ANALYSIS`, Package))

## Reformatage
label_decomposition <- str_split(Tile_concerned$Label, fixed("_"))
for (i in 1:length(label_decomposition)) {
  Tile_concerned$`pH condition`[i] <- label_decomposition[[i]][3]
  Tile_concerned$`Stage experiment`[i] <- label_decomposition[[i]][1]
  Tile_concerned$`Stage incubation`[i] <- label_decomposition[[i]][2]
  Tile_concerned$`Tile concerned`[i] <- paste(label_decomposition[[i]][4], 
                                                  substr(label_decomposition[[i]][5], 1, nchar(label_decomposition[[i]][5])), sep = "_")}

Time = Tile_concerned %>% drop_na(Label) %>% group_by(`pH condition`, `Stage experiment`) %>% 
  dplyr::filter(`Stage experiment` != "PI") %>% 
  arrange(`Stage experiment`) %>% dplyr::select(Diving_Date, Label, Start_incubation, `pH condition`, `Stage experiment`, Temperature, pH_mV)

Alk_T0 <- Alkalinity_dataset_T0 %>% left_join(Time) %>% 
  select(Diving_Date, Start_incubation, `Stage experiment`, `pH condition`, correction, `Batch sd`, Temperature, pH_mV, Label) %>% data.frame() 
rows_to_remove <- c() ; for (i in seq(1, nrow(Alk_T0), by = 4)) {rows_to_remove <- c(rows_to_remove, i+1, i+2)}
Alk_T0 <- Alk_T0[-rows_to_remove, ]

label_decomposition <- str_split(Nutrients$Sample, fixed("_"))
for (i in 1:length(label_decomposition)) {
  Nutrients$`Stage incubation`[i] <- label_decomposition[[i]][2]
  Nutrients$`Tile concerned`[i] <- paste(label_decomposition[[i]][4], 
                                              substr(label_decomposition[[i]][5], 1, nchar(label_decomposition[[i]][5])), sep = "_")}
Nutrients = Nutrients %>% rename(pH.condition = pH, Stage.experiment = Phase, Label = Sample)

T0 = Alk_T0 %>% left_join(Nutrients, by = c("Label", "pH.condition", "Stage.experiment")) %>% 
  mutate(Sampling_Date = Diving_Date, `Starting_time_GMT+1` = Start_incubation - chron::times("01:00:00"),
         Experimental_set = rep(c("Set_1", "Set_2"), 18)) %>% 
  rename(Stage_experiment = Stage.experiment, pH_condition = pH.condition, Alk_mean = correction, Alk_sd = Batch.sd, 
         NH3_mmol.m3 = `NH3 (mmol m-3)`, PO4_mmol.m3 = `PO4 (mmol m-3)`, NO2_mmol.m3 = `NO2 (mmol m-3)`, NO3_mmol.m3 = `NO3 (mmol m-3)`, 
         SiO4_mmol.m3 = `SiO4 (mmol m-3)`) %>% mutate(Stage_experiment = recode(Stage_experiment, "Tn" = "Tn1")) %>% 
  select(Sampling_Date, `Starting_time_GMT+1`, Experiment, Stage_experiment, pH_condition, Experimental_set, Alk_mean, Alk_sd, NH3_mmol.m3, 
         PO4_mmol.m3, NO2_mmol.m3, NO3_mmol.m3, Temperature, pH_mV)

#xlsx::write.xlsx(T0, file = "Data/8. Initial_Conditions/T0_Env.xlsx", row.names = FALSE)