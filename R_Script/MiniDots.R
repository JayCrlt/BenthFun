options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

# Working with O2 sensors outputs
Folder <- "Outputs/Tables"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))

files_O2 = list() ; for (i in 1:length(document_files)) {
  files_O2[[i]]           <- read_excel(paste(Folder, document_files[i], sep = "/")) }
files_O2 <- files_O2 %>% bind_rows()

Summary_O2 <- data.frame(Tile       = rep(files_O2$Tile, length(document_files)),
                         Process    = rep(rep(c("net photosynthesis rate", "gross photosynthesis rate", "dark respiration rate"), each = 18), 
                                          length(document_files)/3),
                         avg_output = c(files_O2$net_photosynthesis_rate_avg, files_O2$gross_photosynthesis_rate_avg, files_O2$respiration_rate_avg),
                         sd         = c(files_O2$net_photosynthesis_rate_sd, files_O2$gross_photosynthesis_rate_sd, files_O2$respiration_rate_sd),
                         incub_time = rep(sub("(^[^–]+)–.*", "\\1", files_O2$Condition), length(document_files)),
                         pH_cond    = rep(str_replace(files_O2$Condition, '.+–(.+)', '\\1'), length(document_files)))

## Define the ranking
Tile_ranking <- Summary_O2 %>% dplyr::filter(Process == "net photosynthesis rate") %>% arrange(-avg_output)
Summary_O2 = Summary_O2 %>% group_by(Tile) %>% mutate(Tile = factor(Tile, levels = rev(Tile_ranking$Tile))) %>% arrange(Tile) %>% 
  mutate(label = paste(Tile, Process, sep = " ")) %>% mutate(label = factor(label, levels = rev(label))) %>% arrange(label) 

Summary_O2$pH_cond[Summary_O2$pH_cond == " AMB"] = "ambient pH conditions"
Summary_O2$pH_cond[Summary_O2$pH_cond == " ELOW"] = "extreme low pH conditions"
Summary_O2$pH_cond[Summary_O2$pH_cond == " LOW"] = "low pH conditions"
