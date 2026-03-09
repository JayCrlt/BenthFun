rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(ggridges)
`%notin%` = Negate(`%in%`)

# Load previous scripts and dataset
## Data supplementary 
source("R_Script/3. Alkalinity.R")
source("R_Script/2. MiniDots.R")

## Data Figure 1
pH_Long_term <- xlsx::read.xlsx("Data/5. Environmental Long term/pH/pH_Long_term.xlsx", sheetName = "Sheet1") %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M:%S"),
         across(c(pH, Temperature), as.numeric))
mean_pH <- data.frame(pH = c(6.438064, 7.6959125, 8.0215523), Site = c("Extreme low", "Low", "Ambient")) %>% 
  mutate(Site = factor(Site, levels = c("Extreme low", "Low", "Ambient"))) 

#### Supplementary ---
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

# Final Dataset
data_transplants <- data_transplants %>% select(-init_pH)
data_historical  <- Summary[[1]]
Summary <- rbind(data_transplants, data_historical)

# Change tile name
convert_tile <- function(tile) {
  tile_number <- str_extract(tile, "\\d+")
  tile_number <- str_pad(tile_number, width = 2, side = "left", pad = "0")
  new_tile <- paste0("tile_", tile_number)
  return(new_tile)}
Summary <- Summary %>% mutate(Tile = convert_tile(Tile)) %>% 
  mutate(pH_cond = recode(pH_cond, "extreme low pH conditions" = "ELOW",
                                   "low pH conditions" = "LOW",
                                   "ambient pH conditions" = "AMB"))

### Figure 1 ---

(Figure_1 = pH_Long_term %>% 
    mutate(Site = fct_recode(Site, "Extreme low" = "extreme_low", "Low" = "low", "Ambient" = "amb")) %>% 
    mutate(Site = factor(Site, levels = c("Ambient", "Low", "Extreme low"))) %>%
    ggplot(aes(x = pH, y = Site)) +
    geom_density_ridges(alpha=0.6, bandwidth=0.05, aes(fill = Site, color = Site), linewidth = 1) +
    geom_point(data = mean_pH, aes(x = pH, y = Site, fill = Site), shape = 21, color = "black", size = 4, show.legend = F) +
    scale_x_continuous(breaks = seq(5.5,8.5,0.5), limits = c(5.75,8.25), name = expression(pH[T])) +
    scale_color_manual(values=c("royalblue3", "goldenrod1", "firebrick2"), labels = c("Extreme low", "Low", "Ambient")) +
    scale_fill_manual(values=c("royalblue3", "goldenrod1", "firebrick2"), labels = c("Extreme low", "Low", "Ambient")) +
    theme_classic() + 
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))

# Exporting important informations
xlsx::write.xlsx(Summary %>% as.data.frame(), "Outputs/Summary/Summary_Process_BenthFun.xlsx", row.names = FALSE)
ggsave(Process_Transplants, filename = "Process_Transplants_Total.png", path = "Outputs/Figures/Processes_Panels", device = "png", width = 12, height = 10) 
ggsave(Process_Historic, filename = "Process_Historic_Total", path = "Outputs/Figures/Processes_Panels", device = "png", width = 7.5, height = 10) 
ggsave(Figure_1, file = "Outputs/Figures/pH_Context/Figure_1.png",  width = 15, height = 15, units = "cm", dpi = 300)