options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

# Working with O2 sensors outputs for transplants experiment
Folder <- "Outputs/Tables/Transplants"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))

files_O2 = list() ; for (i in 1:length(document_files)) {
  files_O2[[i]]           <- read_excel(paste(Folder, document_files[i], sep = "/"), 
                             col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) }
files_O2 <- files_O2 %>% bind_rows()

Summary_O2_transplant <- data.frame(Tile       = rep(files_O2$Tile, 3), 
                         Process    = rep(c("net photosynthesis rate", "gross photosynthesis rate", "dark respiration rate"), each = length(files_O2$Tile)),
                         avg_output = c(files_O2$net_photosynthesis_rate_avg, files_O2$gross_photosynthesis_rate_avg, files_O2$respiration_rate_avg),
                         sd         = c(files_O2$net_photosynthesis_rate_sd, files_O2$gross_photosynthesis_rate_sd, files_O2$respiration_rate_sd),
                         incub_time = rep(sub("(^[^–]+)–.*", "\\1", files_O2$Condition), 3), 
                         pH_cond    = rep(str_replace(files_O2$Condition, '.+–(.+)', '\\1'), 3)) 

## Define the ranking
Tile_ranking <- Summary_O2_transplant %>% dplyr::filter(incub_time == "T0 ", Process == "net photosynthesis rate") %>% arrange(-avg_output)
Summary_O2_transplant = Summary_O2_transplant %>% group_by(Tile) %>% mutate(Tile = factor(Tile, levels = rev(Tile_ranking$Tile))) %>% arrange(Tile) %>% 
  mutate(label = paste(Tile, Process, sep = " ")) # %>% mutate(label = factor(label, levels = rev(label))) %>% arrange(label) 

Summary_O2_transplant$pH_cond[Summary_O2_transplant$pH_cond == " AMB"] = "ambient pH conditions"
Summary_O2_transplant$pH_cond[Summary_O2_transplant$pH_cond == " ELOW"] = "extreme low pH conditions"
Summary_O2_transplant$pH_cond[Summary_O2_transplant$pH_cond == " LOW"] = "low pH conditions"

# Working with O2 sensors outputs for historical experiment
Folder <- "Outputs/Tables/Historic"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))

files_O2 = list() ; for (i in 1:length(document_files)) {
  files_O2[[i]]           <- read_excel(paste(Folder, document_files[i], sep = "/"), 
                                        col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) }
files_O2 <- files_O2 %>% bind_rows()

Summary_O2_historic <- data.frame(Tile       = rep(files_O2$Tile, 3), 
                                    Process    = rep(c("net photosynthesis rate", "gross photosynthesis rate", "dark respiration rate"), each = length(files_O2$Tile)),
                                    avg_output = c(files_O2$net_photosynthesis_rate_avg, files_O2$gross_photosynthesis_rate_avg, files_O2$respiration_rate_avg),
                                    sd         = c(files_O2$net_photosynthesis_rate_sd, files_O2$gross_photosynthesis_rate_sd, files_O2$respiration_rate_sd),
                                    incub_time = rep(sub("(^[^–]+)–.*", "\\1", files_O2$Condition), 3), 
                                    pH_cond    = rep(str_replace(files_O2$Condition, '.+–(.+)', '\\1'), 3)) 

## Define the ranking
Tile_ranking <- Summary_O2_historic %>% dplyr::filter(incub_time == "Tn1 ", Process == "net photosynthesis rate") %>% arrange(-avg_output)
Summary_O2_historic = Summary_O2_historic %>% group_by(Tile) %>% mutate(Tile = factor(Tile, levels = rev(Tile_ranking$Tile))) %>% arrange(Tile) %>% 
  mutate(label = paste(Tile, Process, sep = " ")) # %>% mutate(label = factor(label, levels = rev(label))) %>% arrange(label) 

Summary_O2_historic$pH_cond[Summary_O2_historic$pH_cond == " AMB"] = "ambient pH conditions"
Summary_O2_historic$pH_cond[Summary_O2_historic$pH_cond == " ELOW"] = "extreme low pH conditions"
Summary_O2_historic$pH_cond[Summary_O2_historic$pH_cond == " LOW"] = "low pH conditions"

# Merging everything
Summary_O2 = rbind(Summary_O2_historic, Summary_O2_transplant)
