# Load packages and set reproducibility
library(dplyr) ; library(tidyr) ; library(stringr)
set.seed(123) 

############################
### Table species change ###
############################

# Load data
load("Data/7. Covers/Cover_Transplant_Front.RData") ; load("Data/7. Covers/Cover_Transplant_Back.RData")
Cover_Transplant = rbind(Cover_Transplant_Front, Cover_Transplant_Back) %>% 
  left_join(data.frame(Communities = c(rep("Mixed", 6), rep("forest", 6), rep("encrusting", 6)),
                       Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                                "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                                "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28")))

Cover_Transplant_summary = Cover_Transplant %>% 
  mutate(taxonomy = ifelse(Species == "non-alive", "tile", taxonomy)) %>%
  group_by(Tile, Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders, Communities) %>%
  summarise(Cover = sum(Cover, na.rm = TRUE), .groups = "drop") |> 
  group_by(Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders, Communities) |> 
  summarise(mean = sum(Cover)/4, sd = sd(Cover)) %>% ungroup() %>% 
  select(Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders, Communities, mean, sd) %>%
  mutate(Time_pH = paste0(Time, "_", pH, "_", Communities)) %>% select(-Time, -pH, - Communities) %>%
  pivot_wider(names_from = Time_pH, values_from = c(mean, sd)) %>%
  mutate(across(.cols = starts_with("mean_"), .fns = ~ {sd_col <- get(sub("mean_", "sd_", cur_column()))
      ifelse(is.na(.x) | is.na(sd_col), "0.0 ± 0.0", sprintf("%.1f ± %.1f", .x, sd_col))}, 
      .names = "{sub('mean_', '', .col)}")) %>%
  select(Species, taxonomy, calcareous, primary.producers, filter.feeders, 
         T0_ELOW_encrusting, T0_ELOW_Mixed, T0_ELOW_forest, T1_ELOW_encrusting, T1_ELOW_Mixed, T1_ELOW_forest, 
         T2_ELOW_encrusting, T2_ELOW_Mixed, T2_ELOW_forest, T3_ELOW_encrusting, T3_ELOW_Mixed, T3_ELOW_forest, 
         T0_LOW_encrusting, T0_LOW_Mixed, T0_LOW_forest, T1_LOW_encrusting, T1_LOW_Mixed, T1_LOW_forest, 
         T2_LOW_encrusting, T2_LOW_Mixed, T2_LOW_forest, T3_LOW_encrusting, T3_LOW_Mixed, T3_LOW_forest, 
         T0_AMB_encrusting, T0_AMB_Mixed, T0_AMB_forest, T1_AMB_encrusting, T1_AMB_Mixed, T1_AMB_forest, 
         T2_AMB_encrusting, T2_AMB_Mixed, T2_AMB_forest, T3_AMB_encrusting, T3_AMB_Mixed, T3_AMB_forest) %>% rowwise() %>%
  filter(!all(c_across(all_of(6:17)) == "0.0 ± 0.0")) %>% ungroup() %>%
  mutate(taxonomy = factor(taxonomy, 
    levels = c("tile", "Turf", "Chlorophyta", "Phaeophyceae", "Rhodophyta", "Porifera", "Bryozoa", "Polychaeta", 
               "Crustacea", "Mollusca", "Tunicates"))) %>% arrange(taxonomy, Species)

# Output
# xlsx::write.xlsx(Cover_Transplant_summary, file = "Outputs/Summary/Cover_Transplant.xlsx")

############################
### Community definition ###
############################

# Prepare the data
Cover_Transplant_T0 = rbind(Cover_Transplant_Front, Cover_Transplant_Back) %>% 
  left_join(data.frame(Communities = c(rep("forest", 6), rep("Mixed", 6), rep("encrusting", 6)),
                       Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                                "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                                "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28"))) |> filter(Time == "T0")

comm_matrix = Cover_Transplant_T0 %>% group_by(Tile, Time, Species) %>%
  summarise(Cover = sum(Cover), .groups = "drop") |> filter(Time == "T0") %>% 
  pivot_wider(names_from = Species, values_from = Cover, values_fill = 0) %>%
  column_to_rownames("Tile") |> select(-Time)
comm_matrix_num <- comm_matrix[, colSums(comm_matrix) > 0]

Cover_Transplant_T0_ordered <- Cover_Transplant_T0 %>% filter(Tile %in% rownames(comm_matrix_num)) %>%
  arrange(match(Tile, rownames(comm_matrix_num)))

# NMDS
nmds <- metaMDS(comm_matrix_num, distance = "bray", k = 3, trymax = 100, trace = F) ; nmds$stress
nmds_df <- as.data.frame(nmds$points) |> tibble::rownames_to_column(var = "Tile") |> 
  left_join(Cover_Transplant_T0_ordered, by = "Tile")
# PERMANOVA
Cover_Transplant_T0 = Cover_Transplant |> filter(Time == "T0")  %>% 
  distinct(Tile, Communities, pH) %>% filter(Tile %in% rownames(comm_matrix_num)) %>%
  arrange(match(Tile, rownames(comm_matrix_num)))  
adonis2(comm_matrix_num ~ Communities + pH, data = Cover_Transplant_T0, permutations = 999, method = "bray")

# Plot
NMDS_1vs2 = nmds_df |> 
  mutate(Communities = recode(Communities, "encrusting" = "Calcified", "Mixed" = "Mixed", "forest" = "Fleshy")) |> 
  ggplot(aes(x = MDS1, y = MDS2, shape = pH, fill = Communities)) +
  geom_point(size = 4, color = "black", alpha = 0.7, show.legend = c(fill = F, color = F, shape = T)) +
  stat_ellipse(aes(group = Communities, fill = Communities, color = Communities),
               type = "t", level = 0.75, geom = "polygon", alpha = 0.2) +
  labs(title = "NMDS of T0 Community Composition", color = "Communities", shape = "pH") +
  scale_fill_manual(values= c("#D89446", "#D3D846", "#8AD846"), labels = c("Calcified", "Mixed", "Fleshy")) +
  scale_color_manual(values=c("#D89446", "#D3D846", "#8AD846"), labels = c("Calcified", "Mixed", "Fleshy")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Ambient", "Low", "Extreme Low")) + theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom",
        panel.border    = element_rect(color = "black", fill = NA, linewidth = 1))

NMDS_2vs3 = nmds_df |> 
  mutate(Communities = recode(Communities, "encrusting" = "Calcified", "Mixed" = "Mixed", "forest" = "Fleshy")) |> 
  ggplot(aes(x = MDS2, y = MDS3, shape = pH, fill = Communities)) +
  geom_point(size = 4, color = "black", alpha = 0.7, show.legend = c(fill = F, color = F, shape = T)) +
  stat_ellipse(aes(group = Communities, fill = Communities, color = Communities),
               type = "t", level = 0.75, geom = "polygon", alpha = 0.2) +
  labs(color = "Communities", shape = "pH") +
  scale_fill_manual(values= c("#D89446", "#D3D846", "#8AD846"), labels = c("Calcified", "Mixed", "Fleshy")) +
  scale_color_manual(values=c("#D89446", "#D3D846", "#8AD846"), labels = c("Calcified", "Mixed", "Fleshy")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Ambient", "Low", "Extreme Low")) + theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom",
        panel.border    = element_rect(color = "black", fill = NA, linewidth = 1))

# Final plot
stress_text <- paste0("NMDS stress = ", round(nmds$stress, 3))
permanova_text <- "PERMANOVA: F = 1.698, R² = 0.343, p = 0.022"
combined_NMDS <- (NMDS_1vs2 + NMDS_2vs3) + plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"), caption = paste(stress_text, "|", permanova_text))
