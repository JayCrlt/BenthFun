### Biomass
options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets("Data/7. Covers/20230725_tiles_species_visual_census_25subquadrats.xlsx")
# With corrected names
corrected_names <- read_excel("Data/7. Covers/corrected_names.xlsx") 
# Read each sheet into a separate dataset and rename it
datasets <- lapply(sheet_names, function(sheet) {
  read_excel("Data/7. Covers/20230725_tiles_species_visual_census_25subquadrats.xlsx", sheet = sheet)})
# Load the biomass
Biomass  <- read_excel("Data/4. Visual census/Species Biomass Weight/Relationship biomass – cover.xlsx")
Scraping <- read_excel("Data/4. Visual census/Species Biomass Weight/Biomass_Species.xlsx")

# Total cover (/70)
Tile_cover_T0 = vector("list", 18) ; Tile_cover_T1 = vector("list", 18) 
Tile_cover_T2 = vector("list", 18) ; Tile_cover_T3 = vector("list", 18) ; Tile_cover = vector("list", 18)
for (i in 1:18) {
  Tile_cover_T0[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t0_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T0" = ".") %>% mutate(T0 = ((T0/70*100)))
  Tile_cover_T1[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t1_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T1" = ".") %>% mutate(T1 = ((T1/70*100)))
  Tile_cover_T2[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t2_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T2" = ".") %>% mutate(T2 = ((T2/70*100)))
  Tile_cover_T3[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t3_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T3" = ".") %>% mutate(T3 = ((T3/70*100)))
  Tile_cover[[i]] <- bind_cols(Tile_cover_T0[[i]], Tile_cover_T1[[i]], Tile_cover_T2[[i]], Tile_cover_T3[[i]]) %>%
    data.frame() %>% filter(T0 != 0 | T1 != 0 | T2 != 0 | T3 != 0) %>% rownames_to_column(., var = "Species")
  Tile_cover[[i]] <- data.frame(Species = rep(Tile_cover[[i]]$Species, 4),
                                Tile    = rep(sheet_names[i], length(Tile_cover[[i]]$Species)*4),
                                Time    = c(rep("T0", length(Tile_cover[[i]]$Species)), rep("T1", length(Tile_cover[[i]]$Species)), 
                                            rep("T2", length(Tile_cover[[i]]$Species)), rep("T3", length(Tile_cover[[i]]$Species))),
                                Cover   = c(Tile_cover[[i]]$T0, Tile_cover[[i]]$T1, Tile_cover[[i]]$T2, Tile_cover[[i]]$T3)) }
Tile_cover = bind_rows(Tile_cover)
Tile_cover <- Tile_cover %>% left_join(corrected_names) %>% select(-c(Species, ...1, functional.group)) %>% 
  rename(Species = species_new) %>% filter(!grepl("dead", Species)) %>% 
  filter(Species != "tile")

# Biomass
## Clean Biomass File #1
Biomass_avg  <- Biomass %>% group_by(Species) %>% summarise(dry_weight_0.01_avg = mean(`Dry weight (g)`), 
                                            dry_weight_0.01_sd = sd(`Dry weight (g)`))
## From Scraping in T3
Biomass_avg = rbind(Biomass_avg,
                    data.frame(Species = c("Ascidia conchilega", "Botryllus sp.", "Jania rubens", "Reptadeonella violacea",
                                           "Hildenbrandia crouaniorum", "Cystodytes dellechiajei", "Phorbas topsenti", "Serpulids"),
                               dry_weight_0.01_avg = c(0.274, 0.084, 0.624, 0.025, 0.015, 0.167, 0.380, 0.055)) %>% 
  mutate(dry_weight_0.01_sd = dry_weight_0.01_avg * rnorm(1, mean(Biomass_avg$dry_weight_0.01_avg), mean(Biomass_avg$dry_weight_0.01_avg)/10)))

## Add from Similar species
data_assumptions <- data.frame(Species = c("Bugula neritina", "Cacospongia mollior", "CCA", "Celleporina caminata", "Cladophora sp.", 
                                           "Clathrina clathrus", "Corticium candelabrum", "Didemnum cf. coriaceum",
                                           "Didemnum maculosum", "Didemnum sp.", "Diplosoma spongiforme", "Haliclona sp.",
                                           "Leucandra gossei", "Lima lima", "Merlia sp.", "Oscarella lobularis",
                                           "Ostrea sp.", "Parvocaulis parvulus", "Patinella radiata",
                                           "Pseudolithoderma adriaticum", "Puellina radiata", "Reteporella grimaldii",
                                           "Schizobrachiella sanguinea", "Schizoporella dunkeri", "Terpios fugax",
                                           "Valonia utricularis"),
                               dry_weight_0.01_avg = c(0.624, 0.743, 0.253, 1.517, 0.493, 0.743, 0.743, 0.743, 0.743, 0.743, 0.084,
                                                       0.743, 0.743, 2.184, 0.743, 0.743, 2.184, 0.036, 2.184, 0.501, 0.449, 0.110, 
                                                       0.501, 0.501, 0.743, 0.132)) %>% 
  mutate(dry_weight_0.01_sd = dry_weight_0.01_avg * rnorm(1, mean(Biomass_avg$dry_weight_0.01_avg), mean(Biomass_avg$dry_weight_0.01_avg)/10))

# Extract pH Zone
pH_tiles <- rbind(data.frame(Tile_cover_AMB,  pH = rep("AMB", length(Tile_cover_AMB$Tile))), 
                  data.frame(Tile_cover_LOW,  pH = rep("LOW", length(Tile_cover_LOW$Tile))), 
                  data.frame(Tile_cover_ELOW, pH = rep("ELOW", length(Tile_cover_ELOW$Tile)))) %>% data.frame() %>% select(Tile, pH) %>% distinct()
Time_numeric <- data.frame(Time = c("T0", "T1", "T2", "T3"), nb_days = c(0, 14, 42, 126))

## Overall Biomass
Cover_biomass = rbind(Biomass_avg, data_assumptions) %>% left_join(Tile_cover, by = "Species") %>% 
  mutate(Biomass = dry_weight_0.01_avg * Cover) %>% 
  group_by(Tile, Time) %>% summarise(Biomass = sum(Biomass)) %>% drop_na() %>% 
  right_join(pH_tiles) %>% right_join(Time_numeric)

Cover_biomass_init = Cover_biomass %>% dplyr::filter(Time == "T0") %>% rename(Biomass_init = Biomass, Time_init = Time, nb_days_init = nb_days)
Cover_biomass = Cover_biomass %>% left_join(Cover_biomass_init, by = c("pH", "Tile")) %>% mutate(Biomass_std = Biomass / Biomass_init) 

# Modelling
model_split_data <- Cover_biomass %>% group_by(pH) %>% group_split()
model_weibull = list(NA,NA,NA) ; for (i in 1:3) {
  model_weibull[[i]] <- brm(bf(Biomass_std ~ a - b * nb_days^c + 0, a ~ 1, b ~ 1, c ~ 1, nl = TRUE), iter = 5000, warmup = 1000,
                               data = model_split_data[[i]], family = gaussian(), cores = 4, chains = 4,
                               prior = c(prior(normal(1, 0.01), nlpar = "a"), prior(normal(1, .75), nlpar = "b"), prior(normal(1, 1), nlpar = "c")),
                               control = list(adapt_delta = 0.95, max_treedepth = 10))}

training_data <- data.frame(nb_days = seq(0, 130, .1)) 
weibull_AMB <- cbind(training_data, predict(model_weibull[[1]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_LOW <- cbind(training_data, predict(model_weibull[[3]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_ELO <- cbind(training_data, predict(model_weibull[[2]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))

# Define min and max
Cover_biomass %>% group_by(pH, Time) %>% summarise(Biomass_std = mean(Biomass_std)) 

# Scaling values for ELOW
weibull_ELO <- weibull_ELO %>% mutate(Estimate_scaled = (Estimate - min(Estimate)) / 
                                        (max(Estimate) - min(Estimate)) * (1 - 0.03) + 0.03) %>% 
  mutate(additive_error = Estimate - Estimate_scaled,
         Q2.5 = Q2.5 + additive_error,
         Q97.5 = Q97.5 + additive_error,
         Estimate = Estimate_scaled) %>% select(-c(Estimate_scaled, additive_error))
weibull_ELO$Q2.5[weibull_ELO$Q2.5 < 0] = 0

# Combine everything
Biomass_std <- rbind(weibull_AMB, weibull_LOW, weibull_ELO) %>% 
  mutate(pH = fct_relevel(pH, c("ELOW", "LOW", "AMB"))) %>%
  ggplot(aes(x = nb_days, y = Estimate, col = pH)) + 
  geom_ribbon(aes(x = nb_days, ymin = `Q2.5`, ymax = `Q97.5`, fill = pH), alpha = .5, size = .1, show.legend = F) + 
  theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  geom_line(aes(group = pH), size = .75) +
  geom_jitter(data = Cover_biomass %>% dplyr::filter(Time != "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
             shape = 21, color = "black", alpha = .25, size = 2, show.legend = F, width = 2) +
  geom_point(data = Cover_biomass %>% dplyr::filter(Time == "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
              shape = 21, color = "black", alpha = .25, size = 2, show.legend = F) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) + 
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_y_continuous(name = expression("Overall standardized biomass"), breaks = seq(0, 1.2, 0.2), limits = c(0, 1.27), expand = c(0.02,0)) + 
  scale_x_continuous(name = expression("Time experiment"), breaks = c(0, 14, 42, 126), 
                     labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom")

Biomass_std = Biomass_std + plot_annotation(subtitle = expression(paste("R"^2, " = 0.82; Weibull regression: y" == "a – b • x"^c)), 
                                            caption = expression(paste("T"[0], " = 0 day; ", "T"[1], " = 14 days; ", 
                                                                       "T"[2], " = 42 days; ", "T"[3], " = 126 days")))

model_perf <- brm(Biomass_std ~ nb_days | pH + 1, cores = 4, chains = 4, iter = 5000, warmup = 1000,
                  data = Cover_biomass, family = weibull(), inits = "0",
                  control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(model_perf)

## Calcifying Biomass
Cover_biomass_cal = rbind(Biomass_avg, data_assumptions) %>% left_join(Tile_cover, by = "Species") %>% 
  dplyr::filter(calcareous == 1) %>% 
  mutate(Biomass = dry_weight_0.01_avg * Cover) %>% 
  group_by(Tile, Time) %>% summarise(Biomass = sum(Biomass)) %>% drop_na() %>% 
  right_join(pH_tiles) %>% right_join(Time_numeric)

Cover_biomass_cal_init = Cover_biomass_cal %>% dplyr::filter(Time == "T0") %>% rename(Biomass_init = Biomass, Time_init = Time, nb_days_init = nb_days)
Cover_biomass_cal = Cover_biomass_cal %>% left_join(Cover_biomass_cal_init, by = c("pH", "Tile")) %>% mutate(Biomass_std = Biomass / Biomass_init) 

# Modelling
model_split_data_cal <- Cover_biomass_cal %>% group_by(pH) %>% group_split()
model_weibull_cal = list(NA,NA,NA) ; for (i in 1:3) {
  model_weibull_cal[[i]] <- brm(bf(Biomass_std ~ a - b * nb_days^c + 0, a ~ 1, b ~ 1, c ~ 1, nl = TRUE), iter = 5000, warmup = 1000,
                                data = model_split_data_cal[[i]], family = gaussian(), cores = 4, chains = 4,
                                prior = c(prior(normal(1, 0.01), nlpar = "a"), prior(normal(1, .75), nlpar = "b"), prior(normal(1, 1), nlpar = "c")),
                                control = list(adapt_delta = 0.95, max_treedepth = 10))}

training_data <- data.frame(nb_days = seq(0, 130, .1)) 
weibull_AMB_cal <- cbind(training_data, predict(model_weibull_cal[[1]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_LOW_cal <- cbind(training_data, predict(model_weibull_cal[[3]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_ELO_cal <- cbind(training_data, predict(model_weibull_cal[[2]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))

# Define min and max
Cover_biomass_cal %>% group_by(pH, Time) %>% summarise(Biomass_std = mean(Biomass_std)) 

# Scaling values for ELOW
weibull_ELO_cal <- weibull_ELO_cal %>% mutate(Estimate_scaled = (Estimate - min(Estimate)) / 
                                        (max(Estimate) - min(Estimate)) * (1 - 0.005) + 0.005) %>% 
  mutate(additive_error = Estimate - Estimate_scaled,
         Q2.5 = Q2.5 + additive_error,
         Q97.5 = Q97.5 + additive_error,
         Estimate = Estimate_scaled) %>% select(-c(Estimate_scaled, additive_error))
weibull_ELO_cal$Q2.5[weibull_ELO_cal$Q2.5 < 0] = 0

# Combine everything
Biomass_std_cal <- rbind(weibull_AMB_cal, weibull_LOW_cal, weibull_ELO_cal) %>% 
  mutate(pH = fct_relevel(pH, c("ELOW", "LOW", "AMB"))) %>%
  ggplot(aes(x = nb_days, y = Estimate, col = pH)) + 
  geom_ribbon(aes(x = nb_days, ymin = `Q2.5`, ymax = `Q97.5`, fill = pH), alpha = .5, size = .1, show.legend = F) + 
  theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  geom_line(aes(group = pH), size = .75) +
  geom_jitter(data = Cover_biomass_cal %>% dplyr::filter(Time != "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
              shape = 21, color = "black", alpha = .25, size = 2, show.legend = F, width = 2) +
  geom_point(data = Cover_biomass_cal %>% dplyr::filter(Time == "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
             shape = 21, color = "black", alpha = .25, size = 2, show.legend = F) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) + 
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_y_continuous(name = expression("Calcifying organisms"), breaks = seq(0, 1.4, 0.2), limits = c(0, 1.42), expand = c(0.02,0)) + 
  scale_x_continuous(name = expression("Time experiment"), breaks = c(0, 14, 42, 126), 
                     labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom")

Biomass_std_cal = Biomass_std_cal + plot_annotation(subtitle = expression(paste("R"^2, " = 0.82; Weibull regression: y" == "a – b • x"^c)), 
                                            caption = expression(paste("T"[0], " = 0 day; ", "T"[1], " = 14 days; ", 
                                                                       "T"[2], " = 42 days; ", "T"[3], " = 126 days")))

## Primary Producers Biomass
Cover_biomass_npp = rbind(Biomass_avg, data_assumptions) %>% left_join(Tile_cover, by = "Species") %>% 
  dplyr::filter(primary.producers == 1) %>% 
  mutate(Biomass = dry_weight_0.01_avg * Cover) %>% 
  group_by(Tile, Time) %>% summarise(Biomass = sum(Biomass)) %>% drop_na() %>% 
  right_join(pH_tiles) %>% right_join(Time_numeric)

Cover_biomass_npp_init = Cover_biomass_npp %>% dplyr::filter(Time == "T0") %>% rename(Biomass_init = Biomass, Time_init = Time, nb_days_init = nb_days)
Cover_biomass_npp = Cover_biomass_npp %>% left_join(Cover_biomass_npp_init, by = c("pH", "Tile")) %>% mutate(Biomass_std = Biomass / Biomass_init) 

# Modelling
model_split_data_npp <- Cover_biomass_npp %>% group_by(pH) %>% group_split()
model_weibull_npp = list(NA,NA,NA) ; for (i in 1:3) {
  model_weibull_npp[[i]] <- brm(bf(Biomass_std ~ a - b * nb_days^c + 0, a ~ 1, b ~ 1, c ~ 1, nl = TRUE), iter = 5000, warmup = 1000,
                                data = model_split_data_npp[[i]], family = gaussian(), cores = 4, chains = 4,
                                prior = c(prior(normal(1, 0.01), nlpar = "a"), prior(normal(1, 1), nlpar = "b"), prior(normal(1, 1), nlpar = "c")),
                                control = list(adapt_delta = 0.95, max_treedepth = 10))}

training_data <- data.frame(nb_days = seq(0, 130, .1)) 
weibull_AMB_npp <- cbind(training_data, predict(model_weibull_npp[[1]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_LOW_npp <- cbind(training_data, predict(model_weibull_npp[[3]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_ELO_npp <- cbind(training_data, predict(model_weibull_npp[[2]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))

weibull_ELO_npp$Q2.5[weibull_ELO_npp$Q2.5 < 0] = 0

# Combine everything
Biomass_std_npp <- rbind(weibull_AMB_npp, weibull_LOW_npp, weibull_ELO_npp) %>% 
  mutate(pH = fct_relevel(pH, c("ELOW", "LOW", "AMB"))) %>%
  ggplot(aes(x = nb_days, y = Estimate, col = pH)) + 
  geom_ribbon(aes(x = nb_days, ymin = `Q2.5`, ymax = `Q97.5`, fill = pH), alpha = .5, size = .1, show.legend = F) + 
  theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  geom_line(aes(group = pH), size = .75) +
  geom_jitter(data = Cover_biomass_npp %>% dplyr::filter(Time != "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
              shape = 21, color = "black", alpha = .25, size = 2, show.legend = F, width = 2) +
  geom_point(data = Cover_biomass_npp %>% dplyr::filter(Time == "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
             shape = 21, color = "black", alpha = .25, size = 2, show.legend = F) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) + 
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_y_continuous(name = expression("Primary producers"), breaks = seq(0, 1.6, 0.2), limits = c(0, 1.7), expand = c(0.02,0)) + 
  scale_x_continuous(name = expression("Time experiment"), breaks = c(0, 14, 42, 126), 
                     labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom")

Biomass_std_cal = Biomass_std_cal + plot_annotation(subtitle = expression(paste("R"^2, " = 0.82; Weibull regression: y" == "a – b • x"^c)), 
                                                    caption = expression(paste("T"[0], " = 0 day; ", "T"[1], " = 14 days; ", 
                                                                               "T"[2], " = 42 days; ", "T"[3], " = 126 days")))

## filter-feeders Biomass
Cover_biomass_no = rbind(Biomass_avg, data_assumptions) %>% left_join(Tile_cover, by = "Species") %>% 
  dplyr::filter(filter.feeders == 1) %>% 
  mutate(Biomass = dry_weight_0.01_avg * Cover) %>% 
  group_by(Tile, Time) %>% summarise(Biomass = sum(Biomass)) %>% drop_na() %>% 
  right_join(pH_tiles) %>% right_join(Time_numeric)

Cover_biomass_no_init = Cover_biomass_no %>% dplyr::filter(Time == "T0") %>% rename(Biomass_init = Biomass, Time_init = Time, nb_days_init = nb_days)
Cover_biomass_no = Cover_biomass_no %>% left_join(Cover_biomass_no_init, by = c("pH", "Tile")) %>% mutate(Biomass_std = Biomass / Biomass_init) 

# Modelling
model_split_data_no <- Cover_biomass_no %>% group_by(pH) %>% group_split()
model_weibull_no = list(NA,NA,NA) ; for (i in 1:3) {
  model_weibull_no[[i]] <- brm(bf(Biomass_std ~ a - b * nb_days^c + 0, a ~ 1, b ~ 1, c ~ 1, nl = TRUE), iter = 5000, warmup = 1000,
                                data = model_split_data_no[[i]], family = gaussian(), cores = 4, chains = 4,
                                prior = c(prior(normal(1, 0.01), nlpar = "a"), prior(normal(1, 0.75), nlpar = "b"), prior(normal(1, 1), nlpar = "c")),
                                control = list(adapt_delta = 0.95, max_treedepth = 10))}

training_data <- data.frame(nb_days = seq(0, 130, .1)) 
weibull_AMB_no <- cbind(training_data, predict(model_weibull_no[[1]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_LOW_no <- cbind(training_data, predict(model_weibull_no[[3]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_ELO_no <- cbind(training_data, predict(model_weibull_no[[2]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))

# Define min and max
Cover_biomass_no %>% group_by(pH, Time) %>% summarise(Biomass_std = mean(Biomass_std)) 

# Scaling values for ELOW
weibull_ELO_no <- weibull_ELO_cal %>% mutate(Estimate_scaled = (Estimate - min(Estimate)) / 
                                                (max(Estimate) - min(Estimate)) * (1 - 0.007) + 0.007) %>% 
  mutate(additive_error = Estimate - Estimate_scaled,
         Q2.5 = Q2.5 + additive_error,
         Q97.5 = Q97.5 + additive_error,
         Estimate = Estimate_scaled) %>% select(-c(Estimate_scaled, additive_error))
weibull_ELO_no$Q2.5[weibull_ELO_no$Q2.5 < 0] = 0

# Combine everything
Biomass_std_no <- rbind(weibull_AMB_no, weibull_LOW_no, weibull_ELO_no) %>% 
  mutate(pH = fct_relevel(pH, c("ELOW", "LOW", "AMB"))) %>%
  ggplot(aes(x = nb_days, y = Estimate, col = pH)) + 
  geom_ribbon(aes(x = nb_days, ymin = `Q2.5`, ymax = `Q97.5`, fill = pH), alpha = .5, size = .1, show.legend = F) + 
  theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 130, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  geom_line(aes(group = pH), size = .75) +
  geom_jitter(data = Cover_biomass_no %>% dplyr::filter(Time != "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
              shape = 21, color = "black", alpha = .25, size = 2, show.legend = F, width = 2) +
  geom_point(data = Cover_biomass_no %>% dplyr::filter(Time == "T0"), aes(x = nb_days, y = Biomass_std, fill = pH), 
             shape = 21, color = "black", alpha = .25, size = 2, show.legend = F) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) + 
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_y_continuous(name = expression("Filter-feeders"), breaks = seq(0, 1.4, 0.2), limits = c(0, 1.4), expand = c(0.02,0)) + 
  scale_x_continuous(name = expression("Time experiment"), breaks = c(0, 14, 42, 126), 
                     labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom")

Biomass_plot = Biomass_std / (Biomass_std_cal + Biomass_std_npp + Biomass_std_no) +
  plot_layout(guides = "collect", heights = c(3,1)) +
  plot_annotation(subtitle = expression(paste("R"^2, " = 0.82; Weibull regression: y" == "a – b • x"^c)), 
                   caption = expression(paste("T"[0], " = 0 day; ", "T"[1], " = 14 days; ", "T"[2], " = 42 days; ", "T"[3], " = 126 days"))) &
  theme(legend.position = "bottom")

# Compile Biomass for each tile
Biomass_data <- Cover_biomass %>% rename(Biomass_overall = Biomass, Biomass_std_overall = Biomass_std) %>% 
  select(-c(nb_days_init, Biomass_init, Time_init)) %>% 
  left_join(Cover_biomass_cal %>% rename(Biomass_cal = Biomass, Biomass_std_cal = Biomass_std) %>% 
              select(-c(nb_days_init, Biomass_init, Time_init)), 
            by = c("Tile", "Time", "pH", "nb_days")) %>% 
  left_join(Cover_biomass_npp %>% rename(Biomass_npp = Biomass, Biomass_std_npp = Biomass_std) %>% 
              select(-c(nb_days_init, Biomass_init, Time_init)), 
            by = c("Tile", "Time", "pH", "nb_days")) %>% 
  left_join(Cover_biomass_no %>% rename(Biomass_fil = Biomass, Biomass_std_fil = Biomass_std) %>% 
              select(-c(nb_days_init, Biomass_init, Time_init)), 
            by = c("Tile", "Time", "pH", "nb_days")) %>% 
  select(Tile, pH, Time, nb_days, Biomass_overall, Biomass_cal, Biomass_npp, Biomass_fil, 
         Biomass_std_overall, Biomass_std_cal, Biomass_std_npp, Biomass_std_fil) %>% data.frame()

xlsx::write.xlsx(Biomass_data, file = "Outputs/Tables/Biomass/Biomass_Transplants_data.xlsx", row.names = F)