options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

### Diving_Log
Tile_concerned <- read_excel("Data/Spring_2023/Diving_log_Spring_2023_BenthFun.xlsx", 
                         col_types = c("date", "text", "text", "date", "date", 
                                       "date", "text", "text", "numeric", "numeric"),
                         sheet = "Corrected") %>% 
  mutate(., Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S")) %>% 
  drop_na(`Tile_N°`, Label) %>% dplyr::select(c(Label, `Tile_N°`, Start_incubation, Stop_Alkalinity))

# Issue with the first day
for (i in 1:length(Tile_concerned$Label)) {
if (Tile_concerned$Label[i] == "T0_t1_ELOW_tile_01") {
  Tile_concerned$Start_incubation[i] = format(as.POSIXct("10:43:00", format = "%H:%M:%S"), format = "%H:%M:%S") 
  } else if (Tile_concerned$Label[i] == "T0_t1_ELOW_tile_02") {
    Tile_concerned$Start_incubation[i] = format(as.POSIXct("10:41:00", format = "%H:%M:%S"), format = "%H:%M:%S") 
    } else if (Tile_concerned$Label[i] == "T0_t1_ELOW_tile_03") {
      Tile_concerned$Start_incubation[i] = format(as.POSIXct("10:42:00", format = "%H:%M:%S"), format = "%H:%M:%S") 
      } else {
        Tile_concerned$Start_incubation[i] = Tile_concerned$Start_incubation[i] } }

### Alkalinity
Alkalinity_dataset <- read_excel("Data/Spring_2023/Transplants/Alkalinity/BenthFun_Summary.xlsx") %>% 
  drop_na(`Batch mean`) %>% dplyr::filter(`Sample id` != "CRM1") %>% 
  dplyr::select(-c(`ALK batch`, `delta batch`, note))

label_decomposition <- str_split(Alkalinity_dataset$`Sample id`, fixed("_"))
Alkalinity_dataset <- Alkalinity_dataset %>% mutate(`pH condition` = NA, `Stage experiment` = NA, 
                                                    `Stage incubation` = NA, `Tile concerned` = NA)

for (i in 1:length(label_decomposition)) {
  Alkalinity_dataset$`pH condition`[i] <- label_decomposition[[i]][3]
  Alkalinity_dataset$`Stage experiment`[i] <- label_decomposition[[i]][1]
  Alkalinity_dataset$`Stage incubation`[i] <- label_decomposition[[i]][2]
  Alkalinity_dataset$`Tile concerned`[i] <- paste(label_decomposition[[i]][4], 
                                                  substr(label_decomposition[[i]][5], 1, nchar(label_decomposition[[i]][5])-1), sep = "_")
}

# Cleaned dataset Alkalinity
Alkalinity_dataset = Alkalinity_dataset %>% mutate(Label = substr(`Sample id`, 1, nchar(`Sample id`)-1)) %>% 
  left_join(., Tile_concerned) %>% dplyr::select(-Label) %>% 
  mutate(., Alkalinity_delta = rep(0, length(label_decomposition)), Alkalinity_delta_error = rep(0, length(label_decomposition)))
Alkalinity_dataset_cond = Alkalinity_dataset %>% group_by(`pH condition`) %>% group_split()

for (j in 1:length(Alkalinity_dataset_cond)) {
  for (i in 3:6) {
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta[i] = Alkalinity_dataset_cond[[j]]$correction[i] - Alkalinity_dataset_cond[[j]]$correction[1] 
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[i] = sqrt(Alkalinity_dataset_cond[[j]]$`Batch sd`[i]^2 + Alkalinity_dataset_cond[[j]]$`Batch sd`[1]^2)
    # Correct with the blank chamber
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta[i] = Alkalinity_dataset_cond[[j]]$Alkalinity_delta[i] - Alkalinity_dataset_cond[[j]]$Alkalinity_delta[6] 
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[i] = sqrt(Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[i]^2 + 
                                                                    Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[6]^2)
  }
  for (i in 7:10) {
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta[i] = Alkalinity_dataset_cond[[j]]$correction[i] - Alkalinity_dataset_cond[[j]]$correction[2] 
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[i] = sqrt(Alkalinity_dataset_cond[[j]]$`Batch sd`[i]^2 + Alkalinity_dataset_cond[[j]]$`Batch sd`[2]^2)
    # Correct with the blank chamber
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta[i] = Alkalinity_dataset_cond[[j]]$Alkalinity_delta[i] - Alkalinity_dataset_cond[[j]]$Alkalinity_delta[10] 
    Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[i] = sqrt(Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[i]^2 + 
                                                                    Alkalinity_dataset_cond[[j]]$Alkalinity_delta_error[10]^2)
  }
}

## Combine dataset again and clean dataset
Alkalinity_dataset_cond = Alkalinity_dataset_cond %>% bind_rows() %>% drop_na(`Tile_N°`) %>% 
  mutate(., Time_difference = difftime(as.POSIXct(Stop_Alkalinity, format = "%H:%M:%S"), 
                                       as.POSIXct(Start_incubation, format = "%H:%M:%S")),
         calcification_rate = - Alkalinity_delta / 2 * as.numeric(Time_difference, units = "hours"),
         calcifcation_rate_sd = Alkalinity_delta_error / 2 * as.numeric(Time_difference, units = "hours")) %>% 
  dplyr::select(`Stage experiment`, `pH condition`, `Tile_N°`, calcification_rate, calcifcation_rate_sd)

Alkalinity_dataset_cond$`pH condition`[Alkalinity_dataset_cond$`pH condition` == "AMB"] = "ambient pH conditions"
Alkalinity_dataset_cond$`pH condition`[Alkalinity_dataset_cond$`pH condition` == "ELOW"] = "extreme low pH conditions"
Alkalinity_dataset_cond$`pH condition`[Alkalinity_dataset_cond$`pH condition` == "LOW"] = "low pH conditions"
Alkalinity_dataset_cond$Process = rep("calcifcation rate", length(Alkalinity_dataset_cond$`Stage experiment`))