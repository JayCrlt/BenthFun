rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(ggridges) ; library(readxl)
## Figure 4

Historic_Change_Final <- read_excel("Data_Online/Final_data/Data_Figure_4.xlsx", sheet = 1)

# Define themes
theme_extreme_low <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "firebrick1", face = "bold"),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "firebrick1"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}
theme_ambient <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "cornflowerblue", face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_text(size = 16, vjust = -1),
        axis.title.y = element_text(size = 16, vjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "cornflowerblue"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}
theme_low <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "gold", face = "bold"),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "gold"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}

# Scaling
Historic_Change_Final$Estimate_scaled[Historic_Change_Final$Fct == "CR"] = 
  Historic_Change_Final$Estimate[Historic_Change_Final$Fct == "CR"] / 
  min(Historic_Change_Final$Estimate[Historic_Change_Final$Fct == "CR" & Historic_Change_Final$pH != "ELOW"]) * 4
Historic_Change_Final$Estimate_scaled[Historic_Change_Final$Fct %in% c("GP", "DR")] = 
  Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("GP", "DR")] / 
  min(Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("GP", "DR")]) * 2
Historic_Change_Final$Estimate_scaled[Historic_Change_Final$Fct %in% c("NH4", "NO3", "PO4")] = 
  Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("NH4", "NO3", "PO4")] / 
  max(Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("NH4", "NO3", "PO4")]) / 100

#Sqrt
Historic_Change_Final$Estimate_scaled_log = abs(log(Historic_Change_Final$Estimate_scaled + 1)) 

# Upscale
## PO4
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH != "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH != "ELOW"] * 20
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH == "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH == "ELOW"] * 1.2
## NO3
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NO3") & Historic_Change_Final$pH != "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NO3") & Historic_Change_Final$pH != "ELOW"] * 5
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NO3") & Historic_Change_Final$pH == "LOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NO3") & Historic_Change_Final$pH == "LOW"] * 0.8
## NH4
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NH4") & Historic_Change_Final$pH != "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NH4") & Historic_Change_Final$pH != "ELOW"] * 2
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NH4") & Historic_Change_Final$pH == "AMB"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NH4") & Historic_Change_Final$pH == "AMB"] * 0.9
## DR
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR")] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR")] * 0.75
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR") & Historic_Change_Final$pH != "AMB"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR") & Historic_Change_Final$pH != "AMB"] * 1.4
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR") & Historic_Change_Final$pH == "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR") & Historic_Change_Final$pH == "ELOW"] * 1.3
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR") & Historic_Change_Final$pH == "LOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("DR") & Historic_Change_Final$pH == "LOW"] * 0.8
## GP
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("GP") & Historic_Change_Final$pH == "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("GP") & Historic_Change_Final$pH == "ELOW"] * 1.25

# Colors
Historic_Change_Final$Color = c("#704f3a", "#6cb04c", "#a4cf8f", "#844da3", "#bf9ece", "#9d7bae",
                                "#90664b", "#497733", "#88c06d", "#bf9ece", "#bf9ece", "#bf9ece",
                                "#90664b", "#497733", "#a4cf8f", "#8c679c", "#844da3", "#bf9ece",
                                "#cbae9b", "#629f44", "#497733", "#775587", "#775587", "#8c679c",
                                "#ba947b", "#629f44", "#497733", "#8c679c", "#8c679c", "#8c679c",
                                "#ffffff", "#314f22", "#3f662c", "#563d98", "#563d98", "#563d98")

FOR_AMB_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Fleshy", "Mixed", "Calcifying")),
         Fct = fct_relevel(Fct, c("GP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Fleshy", pH == "AMB") %>% 
  mutate(ID = seq(1,6,1))
(FOR_AMB_HIS_plot = FOR_AMB_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = FOR_AMB_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

MIX_AMB_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Fleshy", "Mixed", "Calcifying")),
         Fct = fct_relevel(Fct, c("GP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Mixed", pH == "AMB") %>% 
  mutate(ID = seq(1,6,1))
(MIX_AMB_HIS_plot = MIX_AMB_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = MIX_AMB_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

ENC_AMB_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Fleshy", "Mixed", "Calcifying")),
         Fct = fct_relevel(Fct, c("GP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Calcifying", pH == "AMB") %>% 
  mutate(ID = seq(1,6,1))
(ENC_AMB_HIS_plot = ENC_AMB_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = ENC_AMB_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

FOR_LOW_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Fleshy", "Mixed", "Calcifying")),
         Fct = fct_relevel(Fct, c("GP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Fleshy", pH == "LOW") %>% 
  mutate(ID = seq(1,6,1))
(FOR_LOW_HIS_plot = FOR_LOW_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = FOR_LOW_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

MIX_LOW_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Fleshy", "Mixed", "Calcifying")),
         Fct = fct_relevel(Fct, c("GP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Mixed", pH == "LOW") %>% 
  mutate(ID = seq(1,6,1))
(MIX_LOW_HIS_plot = MIX_LOW_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = MIX_LOW_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

FOR_ELOW_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Fleshy", "Mixed", "Calcifying")),
         Fct = fct_relevel(Fct, c("GP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Fleshy", pH == "ELOW") %>% 
  mutate(ID = seq(1,6,1))
(FOR_ELOW_HIS_plot = FOR_ELOW_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,5), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = FOR_ELOW_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

Figure_4 <- FOR_AMB_HIS_plot + MIX_AMB_HIS_plot+ ENC_AMB_HIS_plot + FOR_LOW_HIS_plot + MIX_LOW_HIS_plot + plot_spacer() +
    FOR_ELOW_HIS_plot + plot_spacer() + plot_spacer()