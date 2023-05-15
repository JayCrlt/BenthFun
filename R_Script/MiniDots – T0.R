rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

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

T0_minidots <- ggplot(Summary_O2, aes(avg_output, label)) +
  geom_errorbarh(aes(xmin = 0, xmax = avg_output, color = pH_cond), height = 0, position = position_dodge(width = .7), lty = 1) +
  geom_point(aes(fill = pH_cond, shape = Process), size = 3, position = position_dodge(width = .7)) +
  geom_vline(lty = 1, xintercept = 0) +
  theme_dark() + guides(fill="none") +
  scale_fill_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_color_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_shape_manual(values = c(21,22,23)) +
  guides(color = guide_legend(title = "Tiles located after \n incubation in"), shape = guide_legend(title = "Process measured")) +
  scale_y_discrete(labels = c("", "Tile 19", "", "", "Tile 04", "", "", "Tile 07", "", "", "Tile 05", "", "", "Tile 01", "", "", "Tile 06",
                              "", "", "Tile 28", "", "", "Tile 12", "", "", "Tile 02", "", "", "Tile 14", "", "", "Tile 09", "", "", "Tile 11",
                              "", "", "Tile 08", "", "", "Tile 03", "", "", "Tile 29", "", "", "Tile 10", "", "", "Tile 13", "", "", "Tile 18", ""),
                   name = "") +
  scale_x_continuous(name = expression(O[2]~"concentration (mg."*h^-1*")")) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        panel.grid.major = element_line(NA),
        panel.grid.minor = element_line(NA),
        axis.ticks.y = element_line(NA),
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) +
  ggtitle("Before transplantation (i.e., each tile has been incubated in ambient pH conditions)")
  
