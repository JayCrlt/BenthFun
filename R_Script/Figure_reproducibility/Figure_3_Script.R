rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(forcats) ; library(readxl)
## Figure 3

data <- read_excel("Data_Online/Data_Figure_3.xlsx", sheet = 1)
# Prepare data
data_plot <- data %>%
  filter(`Number of days` == max(`Number of days`)) %>% 
  mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB")),
         Comm = factor(Comm, levels = c("Fleshy", "Mixed", "Calcifying")),
         Function = factor(Function, levels = c("CR","DR","GP","NH4","NO3","PO4")),
         y_value = as.numeric(pH) * 3 - (3 - as.numeric(Comm)))

## CR
CR_1 = data_plot %>% filter(Function == "CR" & pH != "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_fill_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_continuous(breaks = c(1, 2, 3), 
                     sec.axis = sec_axis(~ ., name = "Calcification rate", breaks = c(1, 2, 3), labels = c(1.8, 3.5, 5.3))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

CR_2 = data_plot %>% filter(Function == "CR" & pH == "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("ELOW" = "firebrick2")) +
  scale_fill_manual(values = c("ELOW" = "firebrick2")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_continuous(breaks = c(-75, -50), 
                     sec.axis = sec_axis(~ ., name = "Calcification rate", breaks = c(-75), labels = c(-130))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

## DR
DR_1 = data_plot %>% filter(Function == "DR" & pH != "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_fill_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_continuous(breaks = c(1, 2, 3), 
                     sec.axis = sec_axis(~ ., name = "Dark respiration", breaks = c(1, 3), labels = c(1.6, 4.8))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

DR_2 = data_plot %>% filter(Function == "DR" & pH == "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("ELOW" = "firebrick2")) +
  scale_fill_manual(values = c("ELOW" = "firebrick2")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_continuous(breaks = c(5, 10), 
                     sec.axis = sec_axis(~ ., name = "Dark respiration", breaks = c(5, 10), labels = c(8, 16))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

## GP
GP_1 = data_plot %>% filter(Function == "GP" & pH != "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_fill_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     sec.axis = sec_axis(~ ., name = "Gross Photosynthetic Rate", breaks = c(1, 3), labels = c(6.8, 20.4))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

GP_2 = data_plot %>% filter(Function == "GP" & pH == "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("ELOW" = "firebrick2")) +
  scale_fill_manual(values = c("ELOW" = "firebrick2")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_continuous(breaks = c(4, 5, 6), 
                     sec.axis = sec_axis(~ ., name = "Gross Photosynthetic Rate", breaks = c(5), labels = c(34))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# NH4
NH4_1 = data_plot %>% filter(Function == "NH4" & pH != "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_fill_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_reverse(limits = c(15, 0),breaks = c(15, 5), sec.axis = 
                    sec_axis(~ ., name = "NH4 uptake", breaks = c(15, 5), labels = c(-180, -60))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

NH4_2 = data_plot %>% filter(Function == "NH4" & pH == "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("ELOW" = "firebrick2")) +
  scale_fill_manual(values = c("ELOW" = "firebrick2")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_reverse(limits = c(100, 400), breaks = c(100, 400), sec.axis = sec_axis(~ ., name = "NH4 uptake", breaks = c(100, 400), labels = c(-1200, -4800))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# NO3
NO3_1 = data_plot %>% filter(Function == "NO3" & pH != "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_fill_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_reverse(limits = c(-5, 40),breaks = c(10, 40), sec.axis = 
                    sec_axis(~ ., name = "NO3 uptake", breaks = c(10, 40), labels = c(-30, -120))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

NO3_2 = data_plot %>% filter(Function == "NO3" & pH == "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("ELOW" = "firebrick2")) +
  scale_fill_manual(values = c("ELOW" = "firebrick2")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_reverse(limits = c(100, 400), breaks = c(100, 400), sec.axis = sec_axis(~ ., name = "NO3 uptake", breaks = c(100, 400), labels = c(-300, -1200))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# PO4
PO4_1 = data_plot %>% filter(Function == "PO4" & pH != "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_fill_manual(values = c("LOW" = "goldenrod1", "AMB" = "royalblue3")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_reverse(limits = c(0, 2),breaks = c(0.5, 1.5), sec.axis = 
                    sec_axis(~ ., name = "PO4 uptake", breaks = c(0.5, 1.5), labels = c(-1, -3))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

PO4_2 = data_plot %>% filter(Function == "PO4" & pH == "ELOW") %>% 
  ggplot(aes(y = y_value, x = `Estimate ratio`)) +
  geom_linerange(aes(xmin = `Estimate ratio` - `Est.Error ratio`,  xmax = `Estimate ratio` + `Est.Error ratio`, color = pH), 
                 size = 1.2, show.legend = F) +
  geom_point(aes(shape = Comm, fill = pH), color = "black", size = 3, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("ELOW" = "firebrick2")) +
  scale_fill_manual(values = c("ELOW" = "firebrick2")) +
  scale_shape_manual(values = c("Fleshy" = 21, "Mixed" = 23, "Calcifying" = 24)) +
  scale_x_reverse(limits = c(100, 400), breaks = c(100, 400), sec.axis = sec_axis(~ ., name = "PO4 uptake", breaks = c(100, 400), labels = c(-200, -800))) +
  scale_y_continuous(limits = c(1,9), breaks = NULL) + labs(x = "Change-fold", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
