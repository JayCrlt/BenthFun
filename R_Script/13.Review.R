## Table species change
load("Data/7. Covers/Cover_Transplant_Front.RData") ; load("Data/7. Covers/Cover_Transplant_Back.RData")
Cover_Transplant = rbind(Cover_Transplant_Front, Cover_Transplant_Back) %>% 
  mutate(taxonomy = ifelse(Species == "non-alive", "tile", taxonomy)) %>%
  group_by(Tile, Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders) %>%
  summarise(Cover = sum(Cover, na.rm = TRUE), .groups = "drop") |> 
  group_by(Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders) |> 
  summarise(mean = sum(Cover)/12, sd = sd(Cover)) %>% ungroup() %>% 
  select(Time, pH, Species, taxonomy, calcareous, primary.producers, filter.feeders, mean, sd) %>%
  mutate(Time_pH = paste0(Time, "_", pH)) %>% select(-Time, -pH) %>%
  pivot_wider(names_from = Time_pH, values_from = c(mean, sd)) %>%
  mutate(across(.cols = starts_with("mean_"), .fns = ~ {sd_col <- get(sub("mean_", "sd_", cur_column()))
      ifelse(is.na(.x) | is.na(sd_col), "0.0 ± 0.0", sprintf("%.1f ± %.1f", .x, sd_col))}, 
      .names = "{sub('mean_', '', .col)}")) %>%
  select(Species, taxonomy, calcareous, primary.producers, filter.feeders, T0_ELOW, T0_LOW, T0_AMB, T1_ELOW, T1_LOW, T1_AMB,
    T2_ELOW, T2_LOW, T2_AMB, T3_ELOW, T3_LOW, T3_AMB) %>% rowwise() %>%
  filter(!all(c_across(all_of(6:17)) == "0.0 ± 0.0")) %>% ungroup() %>%
  relocate(Species, taxonomy, calcareous, primary.producers, filter.feeders, T0_ELOW, T1_ELOW, T2_ELOW, T3_ELOW,
    T0_LOW, T1_LOW, T2_LOW, T3_LOW, T0_AMB, T1_AMB, T2_AMB, T3_AMB) %>%
  mutate(taxonomy = factor(taxonomy, 
    levels = c("tile", "Turf", "Chlorophyta", "Phaeophyceae", "Rhodophyta", "Porifera", "Bryozoa", "Polychaeta", 
               "Crustacea", "Mollusca", "Tunicates"))) %>% arrange(taxonomy, Species)
# Output
xlsx::write.xlsx(Cover_Transplant, file = "Outputs/Summary/Cover_Transplant.xlsx")