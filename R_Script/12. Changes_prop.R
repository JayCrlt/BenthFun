rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)

## Raw Data Biomass
Biomass_raw <- read_excel("Outputs/Summary/Biomass_Transplants_data.xlsx") %>% left_join(
  data.frame(Communities = c(rep("Mixed", 6), rep("forest", 6), rep("encrusting", 6)),
                  Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                           "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                           "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28"))) %>%
  group_by(pH, Time, nb_days, Communities) %>%
  summarize(across(where(is.numeric), list(mean = ~mean(.), sd = ~sd(.)), .names = "{col}_{fn}")) %>%
  dplyr::select(Communities, pH, Time, nb_days, Biomass_overall_mean, Biomass_cal_mean, Biomass_npp_mean) %>% 
  dplyr::filter(Time == "T0") %>% ungroup()

## Model data Biomass
# Biomass_model_detailed <- rbind(weibull_AMB_enc, weibull_AMB_mix, weibull_AMB_for, 
#       weibull_LOW_enc, weibull_LOW_mix, weibull_LOW_for,
#       weibull_ELO_enc, weibull_ELO_mix, weibull_ELO_for) %>% data.frame() %>% 
#   mutate(Communities = rep(c(rep("encrusting", 13001), rep("Mixed", 13001), rep("forest", 13001)), 3),
#          pH = rep(c(rep("AMB", 13001*3), rep("LOW", 13001*3), rep("ELOW", 13001*3)))) %>% 
#   dplyr::filter(nb_days <= 126.5)
# openxlsx::write.xlsx(Biomass_model_detailed, file = "Outputs/Summary/Biomass_model_detailed.xlsx", rowNames = FALSE)
Biomass_model <- read_excel("Outputs/Summary/Biomass_model_detailed.xlsx")

# Model data Functions
Functions_model <- read_excel("Outputs/Summary/Transplants_Changes_model_non_cropped.xlsx")

# Look only at T3
Functions_model_T3 = Functions_model %>% dplyr::filter(nb_days == 126)
Biomass_model_T3   = Biomass_model %>% dplyr::filter(nb_days == 126) %>%
  mutate(across(where(is.numeric) & !ribbon_neg & !nb_days, ~ if_else(row_number() == n(), . + 0.015, .)))

# Define the biomass at T3
Biomass_model_rate = Biomass_model_T3 %>% left_join(Biomass_raw %>% dplyr::select(-nb_days, -Time), by = c("pH", "Communities")) %>% 
  mutate(Biomass_overall_mean_model = Biomass_overall_mean * Estimate,
         Biomass_cal_mean_model     = Biomass_cal_mean * Estimate,
         Biomass_npp_mean_model     = Biomass_npp_mean * Estimate) %>% 
  dplyr::select(-c(Estimate, Est.Error, Q2.5, Q97.5, Est_loess, ribbon_pos, ribbon_neg, Biomass_overall_mean, Biomass_cal_mean, Biomass_npp_mean))

# Adjust by biomass
dataset_model = Functions_model_T3 %>% left_join(Biomass_model_rate, by = c("nb_days", "pH", "Communities"))
dataset_model_CR  = dataset_model %>% dplyr::filter(Function == "CR") %>%
  mutate(Rate_Std_Sa    = Estimate * Biomass_cal_mean_model / 10 / 490) %>% 
  dplyr::select(Function, nb_days, Communities, pH, Rate_Std_Sa)
dataset_model_GPP = dataset_model %>% dplyr::filter(Function == "GPP") %>%
  mutate(Rate_Std_Sa    = Estimate * Biomass_npp_mean_model * 10 / 490) %>% 
  dplyr::select(Function, nb_days, Communities, pH, Rate_Std_Sa)
dataset_model_DR = dataset_model %>% dplyr::filter(Function == "DR") %>%
  mutate(Rate_Std_Sa    = Estimate * Biomass_overall_mean_model / 100 / 490) %>% 
  dplyr::select(Function, nb_days, Communities, pH, Rate_Std_Sa)
dataset_model_NUT = dataset_model %>% dplyr::filter(Function %in% c("NH3", "NO3", "PO4")) %>%
  mutate(Rate_Std_Sa    = Estimate * Biomass_overall_mean_model / 490) %>% 
  dplyr::select(Function, nb_days, Communities, pH, Rate_Std_Sa)

# combine to plot
data_T3_lolipop_plot = rbind(dataset_model_CR, dataset_model_DR, dataset_model_GPP, dataset_model_NUT) %>% 
  mutate(x_location = rep(c(2,1,3,6,5,7,10,9,11), 6))
data_T3_lolipop_plot$Rate_Std_Sa[data_T3_lolipop_plot$pH == "ELOW" & data_T3_lolipop_plot$Function == "CR"] = 0

CR = data_T3_lolipop_plot %>% dplyr::filter(Function == "CR") %>% 
  ggplot(aes(y = Rate_Std_Sa, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Calcification rate"), breaks = seq(0, 0.1, 0.02), limits = c(0, 0.1), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

DR = data_T3_lolipop_plot %>% dplyr::filter(Function == "DR") %>% 
  ggplot(aes(y = Rate_Std_Sa, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Dark respiration rate"), breaks = seq(0, 1, 0.2), limits = c(0, 1), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

GPP = data_T3_lolipop_plot %>% dplyr::filter(Function == "GPP") %>% 
  ggplot(aes(y = Rate_Std_Sa, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Gross Primary Production"), breaks = seq(0, 4, 1), limits = c(0, 4), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

NH4 = data_T3_lolipop_plot %>% dplyr::filter(Function == "NH3") %>% # min = -838
  mutate(Rate_Std_Sa = if_else(Rate_Std_Sa < -200, -200, Rate_Std_Sa)) %>% 
  ggplot(aes(y = Rate_Std_Sa, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("NH4"), breaks = seq(-200, 0, 50), limits = c(-200, 0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

NO3 = data_T3_lolipop_plot %>% dplyr::filter(Function == "NO3") %>% 
  ggplot(aes(y = Rate_Std_Sa, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("NO3"), breaks = seq(-40, 0, 10), limits = c(-40, 0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

PO4 = data_T3_lolipop_plot %>% dplyr::filter(Function == "PO4") %>% # min = -18.3
  mutate(Rate_Std_Sa = if_else(Rate_Std_Sa < -10, -10, Rate_Std_Sa)) %>% 
  ggplot(aes(y = Rate_Std_Sa, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("PO4"), breaks = seq(-10, 0, 2), limits = c(-10, 0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

Figure_T3_Functions_Area = CR + DR + GPP + NH4 + NO3 + PO4 + plot_layout(ncol = 3)

### Propiortional change
change_prop <- Functions_model %>% dplyr::filter(nb_days %in% c(0, 100)) %>% group_by(nb_days) %>% group_split()
change_prop <- data.frame(Communities = change_prop[[2]]$Communities, 
                          pH          = change_prop[[2]]$pH, 
                          Function    = change_prop[[2]]$Function,
                          Change_fold = change_prop[[2]]$Estimate / change_prop[[1]]$Estimate,
                          x_location  = rep(c(2,1,3,6,5,7,10,9,11), 6))
change_prop$Change_fold[1:3] = NA

CR_prop = change_prop %>% dplyr::filter(Function == "CR") %>% 
  ggplot(aes(y = Change_fold, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Calcification rate"), breaks = seq(0, 3, 1), limits = c(0, 3), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

DR_prop = change_prop %>% dplyr::filter(Function == "DR") %>% 
  ggplot(aes(y = Change_fold, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Dark respiration rate"), breaks = seq(0, 75, 15), limits = c(0, 75), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

GPP_prop = change_prop %>% dplyr::filter(Function == "GPP") %>% 
  ggplot(aes(y = Change_fold, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Gross Photosynthesis rate"), breaks = seq(0, 10, 2), limits = c(0, 10), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

change_prop$Change_fold[28:30] = change_prop$Change_fold[28:30]/100
NH4_prop = change_prop %>% dplyr::filter(Function == "NH3") %>% 
  ggplot(aes(y = Change_fold, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("NH4"), breaks = seq(5, 50, 10), limits = c(0, 50), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

change_prop$Change_fold[37:39] = change_prop$Change_fold[37:39]/7
NO3_prop = change_prop %>% dplyr::filter(Function == "NO3") %>% 
  ggplot(aes(y = Change_fold, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("NO3"), breaks = seq(0, 80, 10), limits = c(0, 80), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

change_prop$Change_fold[46:48] = change_prop$Change_fold[46:48]/75
PO4_prop = change_prop %>% dplyr::filter(Function == "PO4") %>% 
  ggplot(aes(y = Change_fold, x = x_location)) + 
  geom_segment(yend = 0, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 0, xend = 11, yend = 0), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("PO4"), breaks = seq(0, 4, 0.5), limits = c(0, 4), 
                     expand = c(0.02,0)) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

Figure_3B = CR_prop + DR_prop + GPP_prop + NH4_prop + NO3_prop + PO4_prop + plot_layout(ncol = 6)
ggsave(Functions_Communities, file = "Outputs/Figures/Processes_Panels/Functions_3.png", width = 42, 
       height = 16, units = "cm", dpi = 300)