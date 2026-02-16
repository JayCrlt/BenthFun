rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(ggridges) ; library(readxl)
## Figure 2

data <- read_excel("Data_Online/Data_Figure_2.xlsx", sheet = 1)
# Function to create plot by community
plot_community <- function(data, community_name) {
  comm_data <- data %>% filter(Communities == community_name)
  comm_AMB <- comm_data %>% filter(pH == "AMB")
  comm_LOW <- comm_data %>% filter(pH == "LOW")
  comm_ELO <- comm_data %>% filter(pH == "ELOW")

  ggplot() + ggtitle(community_name) +
    geom_ribbon(data = comm_AMB, aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos),
                alpha = 0.5, size = 0.1, fill = "royalblue3", show.legend = FALSE) +
    geom_ribbon(data = comm_LOW, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos),
                alpha = 0.5, size = 0.1, fill = "goldenrod1", show.legend = FALSE) +
    geom_ribbon(data = comm_ELO, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos),
                alpha = 0.5, size = 0.1, fill = "firebrick2", show.legend = FALSE) +
    geom_line(data = comm_AMB, aes(x = nb_days, y = Estimate), color = "cornflowerblue") +
    geom_line(data = comm_LOW, aes(x = nb_days, y = Estimate), color = "gold") +
    geom_line(data = comm_ELO, aes(x = nb_days, y = Estimate), color = "firebrick1") +
    geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = 0.5) +
    scale_y_continuous(name = expression("Standardized biomass change"),
                       breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3), expand = c(0.02,0)) +
    scale_x_continuous(name = "", breaks = c(0, 14, 42, 126), labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
    theme_classic() +
    theme(axis.text       = element_text(size = 14),
          axis.title      = element_text(size = 16),
          legend.text     = element_text(size = 14),
          legend.title    = element_blank(),
          legend.position = "bottom")}

# Create plots
plot_fleshy    <- plot_community(data, "Fleshy")
plot_mixed     <- plot_community(data, "Mixed")
plot_calcified <- plot_community(data, "Calcifying")

# Define values at T3
data_lolipop <- data.frame(pH = rep(c("AMB", "LOW", "ELO"), each = 3),
                           Communities = rep(c("Forest", "Mixed", "Encrusting"), 3),
                           Biomass = c(data$Estimate[data$nb_days == 126 & data$pH == "AMB" & data$Communities == "Fleshy"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "AMB" & data$Communities == "Mixed"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "AMB" & data$Communities == "Calcifying"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "LOW" & data$Communities == "Fleshy"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "LOW" & data$Communities == "Mixed"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "LOW" & data$Communities == "Calcifying"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "ELOW" & data$Communities == "Fleshy"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "ELOW" & data$Communities == "Mixed"],
                                       data$Estimate[data$nb_days == 126 & data$pH == "ELOW" & data$Communities == "Calcifying"]),
                           x_location = c(9,10,11,5,6,7,1,2,3))
data_lolipop$Biomass[data_lolipop$Biomass < 0] = 0

# Lollipop plot
D <- ggplot(data_lolipop, aes(y = Biomass, x = x_location)) +
  geom_segment(aes(x = x_location, xend = x_location, y = 1, yend = Biomass, color = pH), size = 0.8, show.legend = F) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", size = 0.5) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3"),
                     labels = c("Extreme Low", "Low", "Ambient")) +
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"),
                    labels = c("Extreme Low", "Low", "Ambient")) +
  scale_shape_manual(values=c(21, 23, 24),
                     labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Biomass change at"~T[3]),
                     breaks = seq(0, 1.2, 0.2),
                     limits = c(0, 1.2),
                     labels = c("-1.0", "-0.8", "-0.6", "-0.4", "-0.2", "0.0", "+0.2")) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_blank(),
        legend.position = "bottom")

Figure_2 <- plot_fleshy + plot_mixed + plot_calcified + D + plot_layout(guides = "collect", nrow = 1, widths = c(4,4,4,2))