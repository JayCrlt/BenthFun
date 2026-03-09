# Load packages and set reproducibility
library(dplyr) ; library(tidyr) ; library(stringr)
set.seed(123) 

############################
### Table species change ###
############################

# Load data
load("Data/7. Covers/Cover_Transplant_Front.RData") ; load("Data/7. Covers/Cover_Transplant_Back.RData")
Cover_Transplant = rbind(Cover_Transplant_Front, Cover_Transplant_Back) |> 
  left_join(data.frame(Communities = c(rep("Mixed", 6), rep("forest", 6), rep("encrusting", 6)),
                       Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                                "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                                "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28")))

Cover_Transplant_summary = Cover_Transplant |> 
  mutate(taxonomy = ifelse(Species == "non-alive", "tile", taxonomy)) |>
  group_by(Tile, Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders, Communities) |>
  summarise(Cover = sum(Cover, na.rm = TRUE), .groups = "drop") |> 
  group_by(Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders, Communities) |> 
  summarise(mean = sum(Cover)/4, sd = sd(Cover)) |> ungroup() |> 
  select(Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders, Communities, mean, sd) |>
  mutate(Time_pH = paste0(Time, "_", pH, "_", Communities)) |> select(-Time, -pH, - Communities) |>
  pivot_wider(names_from = Time_pH, values_from = c(mean, sd)) |>
  mutate(across(.cols = starts_with("mean_"), .fns = ~ {sd_col <- get(sub("mean_", "sd_", cur_column()))
      ifelse(is.na(.x) | is.na(sd_col), "0.0 ± 0.0", sprintf("%.3f ± %.3f", .x, sd_col))}, 
      .names = "{sub('mean_', '', .col)}")) |>
  select(Species, taxonomy, calcareous, primary.producers, filter.feeders, 
         T0_ELOW_encrusting, T0_ELOW_Mixed, T0_ELOW_forest, T1_ELOW_encrusting, T1_ELOW_Mixed, T1_ELOW_forest, 
         T2_ELOW_encrusting, T2_ELOW_Mixed, T2_ELOW_forest, T3_ELOW_encrusting, T3_ELOW_Mixed, T3_ELOW_forest, 
         T0_LOW_encrusting, T0_LOW_Mixed, T0_LOW_forest, T1_LOW_encrusting, T1_LOW_Mixed, T1_LOW_forest, 
         T2_LOW_encrusting, T2_LOW_Mixed, T2_LOW_forest, T3_LOW_encrusting, T3_LOW_Mixed, T3_LOW_forest, 
         T0_AMB_encrusting, T0_AMB_Mixed, T0_AMB_forest, T1_AMB_encrusting, T1_AMB_Mixed, T1_AMB_forest, 
         T2_AMB_encrusting, T2_AMB_Mixed, T2_AMB_forest, T3_AMB_encrusting, T3_AMB_Mixed, T3_AMB_forest) |> rowwise() |>
  filter(!all(c_across(all_of(6:17)) == "0.0 ± 0.0")) |> ungroup() |>
  mutate(taxonomy = factor(taxonomy, 
    levels = c("tile", "Turf", "Chlorophyta", "Phaeophyceae", "Rhodophyta", "Porifera", "Bryozoa", "Polychaeta", 
               "Crustacea", "Mollusca", "Tunicates"))) |> arrange(taxonomy, Species)

# Output
# xlsx::write.xlsx(Cover_Transplant_summary, file = "Outputs/Summary/Cover_Transplant.xlsx")

# To run after script 8. Covers.R
# sp_amb = Tile_cover_LOW |> 
#   left_join(data.frame(Communities = c(rep("forest", 6), rep("Mixed", 6), rep("encrusting", 6)),
#                        Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
#                                 "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
#                                 "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28"))) |> 
#   group_by(pH, taxonomy, Species, Time, Communities) |> summarise(mean = mean(Cover), sd = sd(Cover)) |> 
#   filter(Time %in% c("T0", "T3"))
# Tile_cover_LOW |> group_by(pH, taxonomy, Time, Tile) |> summarise(Cover = sum(Cover)) |> 
#   group_by(pH, taxonomy, Time) |> summarise(mean = mean(Cover), sd = sd(Cover)) |> View()

############################
### Community definition ###
############################

# Prepare the data
Cover_Transplant_T0 = rbind(Cover_Transplant_Front, Cover_Transplant_Back) |> 
  left_join(data.frame(Communities = c(rep("forest", 6), rep("Mixed", 6), rep("encrusting", 6)),
                       Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                                "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                                "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28"))) |> filter(Time == "T0")

comm_matrix = Cover_Transplant_T0 |> group_by(Tile, Time, Species) |>
  summarise(Cover = sum(Cover), .groups = "drop") |> filter(Time == "T0") |> 
  pivot_wider(names_from = Species, values_from = Cover, values_fill = 0) |>
  column_to_rownames("Tile") |> select(-Time)
comm_matrix_num <- comm_matrix[, colSums(comm_matrix) > 0]

Cover_Transplant_T0_ordered <- Cover_Transplant_T0 |> filter(Tile %in% rownames(comm_matrix_num)) |>
  arrange(match(Tile, rownames(comm_matrix_num)))

# NMDS
nmds <- metaMDS(comm_matrix_num, distance = "bray", k = 3, trymax = 100, trace = F) ; nmds$stress
nmds_df <- as.data.frame(nmds$points) |> tibble::rownames_to_column(var = "Tile") |> 
  left_join(Cover_Transplant_T0_ordered, by = "Tile")
# PERMANOVA
Cover_Transplant_T0 = Cover_Transplant |> filter(Time == "T0")  |> 
  distinct(Tile, Communities, pH) |> filter(Tile %in% rownames(comm_matrix_num)) |>
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

# Output
# ggsave(combined_NMDS, filename = "Fig_S7_Review.png", 
# path = "Outputs/Figures/Final_Figures/PNG/", device = "png", width = 8, height = 4.5) 

##########################
### Modeling functions ###
##########################

# Load and prepare data
load("Data/9. Processes/dataset_change.RData")
training_data <- expand.grid(nb_days = 100, Communities = c("Mixed", "forest", "encrusting"), pH = c("ELOW", "LOW", "AMB"))
# calcification
dataset_change_CR = dataset_change %>% dplyr::filter(Process == "calcifcation rate") %>% 
  dplyr::filter(pH != "ELOW" | Time != "T3")
CR_model <- brm(bf(change_std ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
  data = dataset_change_CR, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(CR_model) ; training_data_CR = cbind(training_data, predict(CR_model, training_data)) |> mutate(Process = "CR")
# dark respiration
dataset_change_DR = dataset_change %>% dplyr::filter(Process == "dark respiration rate") %>% 
  dplyr::filter(pH != "ELOW" | Time != "T2")
DR_model  <- brm(bf(change_std ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                 data = dataset_change_DR, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                 control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(DR_model) ; training_data_DR = cbind(training_data, predict(DR_model, training_data)) |> mutate(Process = "DR")
training_data_DR |> mutate(Estimate = Estimate*1.6, Q2.5 = Estimate - Est.Error*0.5*1.6, Q97.5 = Estimate + Est.Error*0.5*1.6)
# GPP
dataset_change_GPP = dataset_change %>% dplyr::filter(Process == "gross photosynthesis rate")
GPP_model  <- brm(bf(change_std ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                 data = dataset_change_GPP, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                 control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(GPP_model) ; training_data_GPP = cbind(training_data, predict(GPP_model, training_data)) |> mutate(Process = "GPP")
training_data_GPP |> mutate(Estimate = Estimate*6.8, Q2.5 = Estimate - Est.Error*0.5*6.8, Q97.5 = Estimate + Est.Error*0.5*6.8)
# NH4
dataset_change_NH4 = dataset_change %>% dplyr::filter(Process == "NH3")
NH4_model  <- brm(bf(abs(change_std) ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                  data = dataset_change_NH4, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NH4_model) ; training_data_NH4 = cbind(training_data, predict(NH4_model, training_data)) |> mutate(Process = "NH4")
training_data_NH4 |> mutate(Estimate = Estimate*12, Q2.5 = Estimate - Est.Error*0.5*12, Q97.5 = Estimate + Est.Error*0.5*12)
# NO3
dataset_change_NO3 = dataset_change %>% dplyr::filter(Process == "NO3") 
NO3_model  <- brm(bf(abs(change_std) ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                  data = dataset_change_NO3, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NO3_model) ; training_data_NO3 = cbind(training_data, predict(NO3_model, training_data)) |> mutate(Process = "NO3")
training_data_NO3 |> mutate(Estimate = Estimate*3, Q2.5 = Estimate - Est.Error*0.5*3, Q97.5 = Estimate + Est.Error*0.5*3)
# PO4
dataset_change_PO4 = dataset_change %>% dplyr::filter(Process == "PO4") 
PO4_model  <- brm(bf(abs(change_std) ~ nb_days * pH + (nb_days | Communities), sigma ~ pH),
                  data = dataset_change_PO4, family = gaussian(),cores = 4, chains = 4, iter = 10000, warmup = 2000,
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(PO4_model) ; training_data_PO4 = cbind(training_data, predict(PO4_model, training_data)) |> mutate(Process = "PO4")
training_data_PO4 |> mutate(Estimate = Estimate*.5, Q2.5 = Estimate - Est.Error*0.5*.5, Q97.5 = Estimate + Est.Error*0.5*.5)
# Combine everything
training_data = rbind(training_data_CR, training_data_DR, training_data_GPP, 
                      training_data_NH4, training_data_NO3, training_data_PO4) |>
  select(-c(Q2.5, Q97.5)) |> mutate(CI_error = 0.5*Est.Error, Estimate = if_else(Process %in% c("NH4", "NO3", "PO4"),
      -Estimate, Estimate)) |> select(-Est.Error) |> relocate(CI_error, .after = Estimate)

training_data |> mutate(CI_Low = Estimate - CI_error, CI_Sup = Estimate + CI_error) |> dplyr::filter(Process == 'PO4')

Fig_3_Review = training_data |> filter(pH == "ELOW") |> 
  ggplot(aes(y = pH, x = Estimate, xmin = Estimate - CI_error, xmax = Estimate + CI_error,
  shape = Communities, fill  = pH, color = pH)) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_linerange(position = position_dodge(width = 0.5), size = 0.7, stroke = 0.8) +
  geom_point(position = position_dodge(width = 0.5), size = 3, stroke = 0.8, color = "black") +
  facet_wrap(~ Process, ncol = 6, scales = "free_x") +
  scale_fill_manual(values = c("ELOW" = "firebrick2", "LOW"  = "goldenrod1", "AMB"  = "royalblue3"),
                    labels = c("Extreme Low", "Low", "Ambient")) +
  scale_color_manual(values = c("ELOW" = "firebrick2", "LOW"  = "goldenrod1", "AMB"  = "royalblue3")) +
  scale_shape_manual(values = c("forest" = 23, "Mixed" = 21, "encrusting" = 24),
                     labels = c("Fleshy Macroalgae-dominance", "Mixed Macroalgae-dominance", "Calcifying Macroalgae-dominance")) + theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        strip.text      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom",
        panel.border    = element_rect(color = "black", fill = NA, linewidth = 1)) +
  labs(y = "pH zone", x = "Effect size (Estimate ± CI)")

ggsave(Fig_3_Review, filename = "Fig_3_Review_2.png", 
       path = "Outputs/Figures/Final_Figures/PNG/", device = "png", width = 12, height = 2.5) 

########################
### Species richness ###
########################

head(Tile_cover)
species_richness <- Tile_cover %>% filter(Cover != 0) %>% group_by(Tile, Time, pH) %>% 
  summarise(Species_richness = n_distinct(Species), .groups = "drop") %>% 
  left_join( data.frame(Communities = c(rep("Mixed", 6), rep("Fleshy", 6), rep("Calcified", 6)),
                        Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                                 "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                                 "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28")), by = "Tile") %>% 
  left_join(data.frame(Time = c("T0", "T1", "T2", "T3"), nb_days = c(0, 14, 42, 126)), by = "Time") %>% 
  group_by(Tile) %>% mutate(Rel_richness = Species_richness / Species_richness[Time == "T0"]) %>% ungroup()

model_split_data <- species_richness %>% group_by(pH, Communities) %>% group_split()

model_weibull = list(NA,NA,NA,NA,NA,NA,NA,NA,NA) ; for (i in 1:9) {
  model_weibull[[i]] <- brm(bf(Rel_richness ~ a - b * nb_days^c + 0, a ~ 1, b ~ 1, c ~ 1, nl = TRUE), iter = 10000, warmup = 2000,
                            data = model_split_data[[i]], family = gaussian(), cores = 4, chains = 4,
                            prior = c(prior(normal(1, 0.01), nlpar = "a"), prior(normal(1, 1), nlpar = "b"), prior(normal(1, 1), nlpar = "c")),
                            control = list(adapt_delta = 0.95, max_treedepth = 10))}

training_data <- data.frame(nb_days = seq(0, 130, 0.1)) 
weibull_AMB_mix <- cbind(training_data, predict(model_weibull[[9]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_AMB_enc <- cbind(training_data, predict(model_weibull[[7]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_AMB_for <- cbind(training_data, predict(model_weibull[[8]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_LOW_mix <- cbind(training_data, predict(model_weibull[[6]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_LOW_enc <- cbind(training_data, predict(model_weibull[[5]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_LOW_for <- cbind(training_data, predict(model_weibull[[4]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_ELO_mix <- cbind(training_data, predict(model_weibull[[3]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))
weibull_ELO_enc <- cbind(training_data, predict(model_weibull[[1]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))
weibull_ELO_for <- cbind(training_data, predict(model_weibull[[2]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))


# Clean the modeling average
weibull_AMB_mix$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_AMB_mix, span = 0.05), newdata = weibull_AMB_mix) 
weibull_AMB_enc$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_AMB_enc, span = 0.05), newdata = weibull_AMB_enc) 
weibull_AMB_for$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_AMB_for, span = 0.05), newdata = weibull_AMB_for) 
weibull_LOW_mix$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_LOW_mix, span = 0.05), newdata = weibull_LOW_mix) 
weibull_LOW_enc$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_LOW_enc, span = 0.05), newdata = weibull_LOW_enc) 
weibull_LOW_for$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_LOW_for, span = 0.05), newdata = weibull_LOW_for) 
weibull_ELO_mix$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_ELO_mix, span = 0.05), newdata = weibull_ELO_mix) 
weibull_ELO_enc$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_ELO_enc, span = 0.05), newdata = weibull_ELO_enc) 
weibull_ELO_for$Est_loess = predict(loess(Estimate ~ nb_days, data = weibull_ELO_for, span = 0.05), newdata = weibull_ELO_for)

# Define the ribbon
weibull_AMB_mix <- weibull_AMB_mix %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_AMB_enc <- weibull_AMB_enc %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_AMB_for <- weibull_AMB_for %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_LOW_mix <- weibull_LOW_mix %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_LOW_enc <- weibull_LOW_enc %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_LOW_for <- weibull_LOW_for %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_ELO_mix <- weibull_ELO_mix %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_ELO_enc <- weibull_ELO_enc %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)
weibull_ELO_for <- weibull_ELO_for %>% mutate(ribbon_pos = Estimate + Est.Error, ribbon_neg = Estimate - Est.Error)

# Clean the positive ribbon
weibull_AMB_mix$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_AMB_mix, span = 0.05), newdata = weibull_AMB_mix) 
weibull_AMB_enc$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_AMB_enc, span = 0.05), newdata = weibull_AMB_enc) 
weibull_AMB_for$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_AMB_for, span = 0.05), newdata = weibull_AMB_for) 
weibull_LOW_mix$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_LOW_mix, span = 0.05), newdata = weibull_LOW_mix) 
weibull_LOW_enc$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_LOW_enc, span = 0.05), newdata = weibull_LOW_enc) 
weibull_LOW_for$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_LOW_for, span = 0.05), newdata = weibull_LOW_for) 
weibull_ELO_mix$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_ELO_mix, span = 0.05), newdata = weibull_ELO_mix) 
weibull_ELO_enc$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_ELO_enc, span = 0.05), newdata = weibull_ELO_enc) 
weibull_ELO_for$ribbon_pos = predict(loess(ribbon_pos ~ nb_days, data = weibull_ELO_for, span = 0.05), newdata = weibull_ELO_for) 

# Clean the negative ribbon
weibull_AMB_mix$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_AMB_mix, span = 0.05), newdata = weibull_AMB_mix) 
weibull_AMB_enc$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_AMB_enc, span = 0.05), newdata = weibull_AMB_enc) 
weibull_AMB_for$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_AMB_for, span = 0.05), newdata = weibull_AMB_for) 
weibull_LOW_mix$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_LOW_mix, span = 0.05), newdata = weibull_LOW_mix) 
weibull_LOW_enc$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_LOW_enc, span = 0.05), newdata = weibull_LOW_enc) 
weibull_LOW_for$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_LOW_for, span = 0.05), newdata = weibull_LOW_for) 
weibull_ELO_mix$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_ELO_mix, span = 0.05), newdata = weibull_ELO_mix) 
weibull_ELO_enc$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_ELO_enc, span = 0.05), newdata = weibull_ELO_enc) 
weibull_ELO_for$ribbon_neg = predict(loess(ribbon_neg ~ nb_days, data = weibull_ELO_for, span = 0.05), newdata = weibull_ELO_for) 

# Fix negative ribbons at zero if negative
weibull_ELO_mix$ribbon_neg[weibull_ELO_mix$ribbon_neg < 0] = 0
weibull_ELO_enc$ribbon_neg[weibull_ELO_enc$ribbon_neg < 0] = 0
weibull_ELO_for$ribbon_neg[weibull_ELO_for$ribbon_neg < 0] = 0

A = ggplot() + ggtitle("Fleshy macroalgae-dominated communities") + 
  geom_ribbon(data = weibull_AMB_for, aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "royalblue3", show.legend = F) + 
  geom_ribbon(data = weibull_LOW_for, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "goldenrod1", show.legend = F) + 
  geom_ribbon(data = weibull_ELO_for, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "firebrick2", show.legend = F) + 
  geom_line(data = weibull_AMB_for, aes(x = nb_days, y = Estimate), color = "cornflowerblue") + 
  geom_line(data = weibull_LOW_for, aes(x = nb_days, y = Estimate), color = "gold") + 
  geom_line(data = weibull_ELO_for, aes(x = nb_days, y = Estimate), color = "firebrick1") + 
  theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_y_continuous(name = expression("Standardized species richness change"), 
                     breaks = seq(0, 1.2, 0.2), limits = c(0, 1.2), expand = c(0.02,0)) + 
  scale_x_continuous(name = "", breaks = c(0, 14, 42, 126), labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom")

B = ggplot() + ggtitle("Mixed macroalgae-dominated communities") + 
  geom_ribbon(data = weibull_AMB_mix, aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "royalblue3", show.legend = F) + 
  geom_ribbon(data = weibull_LOW_mix, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "goldenrod1", show.legend = F) + 
  geom_ribbon(data = weibull_ELO_mix, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "firebrick2", show.legend = F) + 
  geom_line(data = weibull_AMB_mix, aes(x = nb_days, y = Estimate), color = "cornflowerblue") + 
  geom_line(data = weibull_LOW_mix, aes(x = nb_days, y = Estimate), color = "gold") + 
  geom_line(data = weibull_ELO_mix, aes(x = nb_days, y = Estimate), color = "firebrick1") + 
  theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_y_continuous(name = "", breaks = seq(0, 1.2, 0.2), limits = c(0, 1.2), expand = c(0.02,0)) + 
  scale_x_continuous(name = "", breaks = c(0, 14, 42, 126), labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_text(size = 0),
        legend.position = "bottom")

C = ggplot() + ggtitle("Calcifying macroalgae-dominated communities") + 
  geom_ribbon(data = weibull_AMB_enc, aes(x = nb_days, y = Estimate, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "royalblue3", show.legend = F) + 
  geom_ribbon(data = weibull_LOW_enc, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "goldenrod1", show.legend = F) + 
  geom_ribbon(data = weibull_ELO_enc, aes(x = nb_days, y = Est_loess, ymin = ribbon_neg, ymax = ribbon_pos), 
              alpha = .5, size = .1, fill = "firebrick2", show.legend = F) + 
  geom_line(data = weibull_AMB_enc, aes(x = nb_days, y = Estimate), color = "cornflowerblue") + 
  geom_line(data = weibull_LOW_enc, aes(x = nb_days, y = Estimate), color = "gold") + 
  geom_line(data = weibull_ELO_enc, aes(x = nb_days, y = Estimate), color = "firebrick1") + 
  theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_y_continuous(name = "", breaks = seq(0, 1.2, 0.2), limits = c(0, 1.2), expand = c(0.02,0)) + 
  scale_x_continuous(name = "", breaks = c(0, 14, 42, 126), labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_text(size = 0),
        legend.position = "bottom")

(fig_S7 <- A + B + C )

ggsave(fig_S7, filename = "fig_S7_Review.png", 
       path = "Outputs/Figures/Final_Figures/PNG/", device = "png", width = 30, 
       height = 12, units = "cm", dpi = 300) 
