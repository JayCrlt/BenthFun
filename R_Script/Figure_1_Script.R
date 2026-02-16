rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(ggridges) ; library(xlsx)
## Figure 1

(Figure_1 = xlsx::read.xlsx("Data_Online/Data_Figure_1.xlsx", sheetName = "Sheet1") %>% 
  mutate(Site = fct_recode(Site, "Extreme low" = "extreme_low", "Low" = "low", "Ambient" = "amb")) %>% 
  mutate(Site = factor(Site, levels = c("Ambient", "Low", "Extreme low"))) %>%
  ggplot(aes(x = pH, y = Site)) +
  geom_density_ridges(alpha=0.6, bandwidth=0.05, aes(fill = Site, color = Site), linewidth = 1) +
  #geom_point(data = mean_pH, aes(x = pH, y = Site, fill = Site), shape = 21, color = "black", size = 4, show.legend = F) +
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