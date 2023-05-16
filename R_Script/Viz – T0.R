rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

# Load previous scripts
source("R_Script/Alkalinity – T0.R")
source("R_Script/MiniDots – T0.R")

# Merging both datasets
Alkalinity_dataset_cond <- Alkalinity_dataset_cond %>% 
  dplyr::select(., c(`Tile_N°`, Process, calcification_rate, calcifcation_rate_sd, `Stage experiment`, `pH condition`)) %>% 
  mutate(label = paste(`Tile_N°`, Process, sep = " "))
colnames(Alkalinity_dataset_cond) = colnames(Summary_O2)
Summary_T0 = rbind(Summary_O2, Alkalinity_dataset_cond)

## Define the ranking
Summary_T0 = Summary_T0 %>% group_by(Tile) %>% mutate(Tile = factor(Tile, levels = rev(Tile_ranking$Tile))) %>% arrange(Tile) %>% 
  mutate(label = paste(Tile, Process, sep = " ")) %>% mutate(label = factor(label, levels = rev(label))) %>% arrange(label) 

T0_minidots <- Summary_T0 %>% dplyr::filter(., Process != "calcifcation rate") %>% 
  ggplot(., aes(avg_output, label)) +
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
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) 

T0_Alkalinity <- Summary_T0 %>% dplyr::filter(., Process == "calcifcation rate") %>% 
  ggplot(., aes(avg_output, label)) +
  geom_errorbarh(aes(xmin = 0, xmax = avg_output, color = pH_cond), height = 0, position = position_dodge(width = .7), lty = 1) +
  geom_point(aes(fill = pH_cond, shape = Process), size = 3, position = position_dodge(width = .7)) +
  geom_vline(lty = 1, xintercept = 0) +
  theme_dark() + guides(fill="none") +
  scale_fill_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_color_manual(values = c("royalblue3", "gold", "red"), breaks = c("ambient pH conditions", "low pH conditions", "extreme low pH conditions")) +
  scale_shape_manual(values = 24) +
  guides(color = guide_legend(title = "Tiles located after \n incubation in"), shape = guide_legend(title = "Process measured")) +
  scale_y_discrete(labels = c("Tile 19", "Tile 04", "Tile 07", "Tile 05", "Tile 01", "Tile 06", "Tile 28", "Tile 12", "Tile 02", 
                              "Tile 14", "Tile 09", "Tile 11", "Tile 08", "Tile 03", "Tile 29", "Tile 10", "Tile 13", "Tile 18"),
                   name = "") +
  scale_x_continuous(name = expression(CaCO[3]~"production (µg."*h^-1*")")) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        panel.grid.major = element_line(NA),
        panel.grid.minor = element_line(NA),
        axis.ticks.y = element_line(NA),
        panel.border = element_rect(linetype = "solid", fill = NA, colour = "black")) 

Process_T0 <- T0_minidots + T0_Alkalinity + plot_layout(guides = "collect") +
  plot_annotation(title = 'Before transplantation', subtitle = "(i.e., each tile has been incubated in ambient pH conditions)") & 
  scale_shape_manual(name = "Process measured", values = c(21, 22, 23, 24), limits = c("dark respiration rate", "gross photosynthesis rate",
                                                                                       "net photosynthesis rate", "calcifcation rate"))

# Exporting important informations
xlsx::write.xlsx(Summary_T0 %>% as.data.frame() %>% dplyr::select(-label), "Outputs/Summary/Summary_Process_T0.xlsx", row.names = FALSE)
ggsave(Process_T0, filename = "Process_T0.png", path = "Outputs/Figures", device = "png", width = 12, height = 6) 
