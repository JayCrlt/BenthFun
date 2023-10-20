rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

# Load previous scripts
source("R_Script/3. Alkalinity.R")
source("R_Script/2. MiniDots.R")

# Merging both datasets
Alkalinity_dataset_cond <- Alkalinity_dataset_cond %>% 
  dplyr::select(., c(`Tile_N°`, Process, calcification_rate, calcifcation_rate_sd, `Stage experiment`, `pH condition`)) %>% 
  mutate(label = paste(`Tile_N°`, Process, sep = " ")) %>% 
  select(`Tile_N°`, Process, calcification_rate, calcifcation_rate_sd, `Stage experiment`, `pH condition`)
  
colnames(Alkalinity_dataset_cond) = colnames(Summary_O2)
Summary = rbind(Summary_O2, Alkalinity_dataset_cond) %>% select(-label)

# Rename Experiment condition
Summary$incub_time[Summary$incub_time == "T0 "] = "T0"
Summary$incub_time[Summary$incub_time == "T1 "] = "T1"
Summary$incub_time[Summary$incub_time == "T2 "] = "T2"
Summary$incub_time[Summary$incub_time == "T3 "] = "T3"
Summary$incub_time[Summary$incub_time %in% c("Tn", "Tn1 ")] = "Tn1"
Summary$incub_time[Summary$incub_time %in% c("Tn2","Tn2 ")] = "Tn2"
Summary$Main_Exp[Summary$incub_time %in% c("Tn1","Tn2")] = "Historical"
Summary$Main_Exp[is.na(Summary$Main_Exp)] = "Transplants"

# Split by experiment
Summary$Tile[is.na(Summary$Tile)] = "A5"
Summary = Summary %>% group_by(Main_Exp) %>% group_split()

## Define the ranking
Tile_ranking <- Summary_O2_historic %>% dplyr::filter(incub_time == "Tn1 ", Process == "net photosynthesis rate") %>% arrange(-avg_output)
Summary_O2_historic = Summary_O2_historic %>% group_by(Tile) %>% mutate(Tile = factor(Tile, levels = rev(Tile_ranking$Tile))) %>% arrange(Tile) %>% 
  mutate(label = paste(Tile, Process, sep = " "))

# Historical
Historic_minidots <- Summary[[1]] %>% dplyr::filter(., Process != "calcifcation rate") %>% 
  ggplot(., aes(avg_output, Tile)) +
  geom_errorbarh(aes(xmin = 0, xmax = avg_output, color = pH_cond), height = 0, position = position_dodge(width = .7), lty = 1) +
  geom_point(aes(fill = pH_cond, shape = Process), size = 3, position = position_dodge(width = .7), show.legend = F) +
  geom_vline(lty = 1, xintercept = 0) +
  theme_dark() + guides(fill="none") +
  scale_fill_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_color_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_shape_manual(values = c(21,22,23)) + facet_wrap(Process~incub_time, ncol = 2) +
  guides(color = guide_legend(title = "Tiles incubations from"), shape = guide_legend(title = "Process measured")) +
  scale_x_continuous(name = expression(O[2]~"concentration (mg."*h^-1*")")) +
  scale_y_discrete(name = "") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        #panel.grid.major = element_line(NA),
        #panel.grid.minor = element_line(NA),
        axis.ticks.y = element_line(NA),
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) 

Historic_Alkalinity <- Summary[[1]] %>% dplyr::filter(., Process == "calcifcation rate") %>% 
  ggplot(., aes(avg_output, Tile)) +
  geom_errorbarh(aes(xmin = 0, xmax = avg_output, color = pH_cond), height = 0, position = position_dodge(width = .7), lty = 1) +
  geom_point(aes(fill = pH_cond, shape = Process), size = 3, position = position_dodge(width = .7)) +
  geom_vline(lty = 1, xintercept = 0) +
  theme_dark() + guides(fill="none") +
  scale_fill_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_color_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_shape_manual(values = 24) + facet_wrap(Process~incub_time, ncol = 2) +
  guides(color = guide_legend(title = "Tiles incubations from"), shape = guide_legend(title = "Process measured")) +
  scale_x_continuous(name = expression(CaCO[3]~"production (µg."*h^-1*")")) +
  scale_y_discrete(name = "") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        #panel.grid.major = element_line(NA),
        #panel.grid.minor = element_line(NA),
        axis.ticks.y = element_line(NA),
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) 

Process_Historic <- Historic_minidots / Historic_Alkalinity + plot_layout(guides = "collect", heights = c(3,0.8)) +
  scale_shape_manual(name = "Process measured", values = c(21, 22, 23, 24), limits = c("dark respiration rate", "gross photosynthesis rate",
                                                                                       "net photosynthesis rate", "calcifcation rate"))
# Transplants
## Before we need to rename the tile

data_transplants <- Summary[[2]]
data_transplants$init_pH[data_transplants$pH_cond == "extreme low pH conditions"] = "E"
data_transplants$init_pH[data_transplants$pH_cond == "low pH conditions"] = "L"
data_transplants$init_pH[data_transplants$pH_cond == "ambient pH conditions"] = "A"
data_transplants$Tile = paste(data_transplants$init_pH, data_transplants$Tile, sep = "")

Transplants_minidots <- data_transplants %>% dplyr::filter(., Process != "calcifcation rate") %>% 
  ggplot(., aes(avg_output, Tile)) +
  geom_errorbarh(aes(xmin = 0, xmax = avg_output, color = pH_cond), height = 0, position = position_dodge(width = .7), lty = 1) +
  geom_point(aes(fill = pH_cond, shape = Process), size = 3, position = position_dodge(width = .7), show.legend = F) +
  geom_vline(lty = 1, xintercept = 0) +
  theme_dark() + guides(fill="none") +
  scale_fill_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_color_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_shape_manual(values = c(21,22,23)) + facet_wrap(Process~incub_time, ncol = 4) +
  guides(color = guide_legend(title = "Tiles incubations from"), shape = guide_legend(title = "Process measured")) +
  scale_x_continuous(name = expression(O[2]~"concentration (mg."*h^-1*")")) +
  scale_y_discrete(name = "") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        #panel.grid.major = element_line(NA),
        #panel.grid.minor = element_line(NA),
        axis.ticks.y = element_line(NA),
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) 

Transplants_Alkalinity <- data_transplants %>% dplyr::filter(., Process == "calcifcation rate") %>% 
  ggplot(., aes(avg_output, Tile)) +
  geom_errorbarh(aes(xmin = 0, xmax = avg_output, color = pH_cond), height = 0, position = position_dodge(width = .7), lty = 1) +
  geom_point(aes(fill = pH_cond, shape = Process), size = 3, position = position_dodge(width = .7)) +
  geom_vline(lty = 1, xintercept = 0) +
  theme_dark() + guides(fill="none") +
  scale_fill_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_color_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_shape_manual(values = 24) + facet_wrap(Process~incub_time, ncol = 4) +
  guides(color = guide_legend(title = "Tiles incubations from"), shape = guide_legend(title = "Process measured")) +
  scale_x_continuous(name = expression(CaCO[3]~"production (µg."*h^-1*")")) +
  scale_y_discrete(name = "") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        #panel.grid.major = element_line(NA),
        #panel.grid.minor = element_line(NA),
        axis.ticks.y = element_line(NA),
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) 

Process_Transplants <- Transplants_minidots / Transplants_Alkalinity + plot_layout(guides = "collect", heights = c(3,0.8)) +
  scale_shape_manual(name = "Process measured", values = c(21, 22, 23, 24), limits = c("dark respiration rate", "gross photosynthesis rate",
                                                                                       "net photosynthesis rate", "calcifcation rate"))

# Exporting important informations
# xlsx::write.xlsx(Summary_T0 %>% as.data.frame() %>% dplyr::select(-label), "Outputs/Summary/Summary_Process_T0.xlsx", row.names = FALSE)
ggsave(Process_Transplants, filename = "Process_Transplants_Total.png", path = "Outputs/Figures", device = "png", width = 12, height = 10) 
ggsave(Process_Historic, filename = "Process_Historic_Total", path = "Outputs/Figures", device = "png", width = 7.5, height = 10) 
