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
Zone_pH  <- data_frame(Tile = sheet_names, pH = c(rep("ELOW", 6), rep("LOW", 6), rep("AMB", 6)))

# Work with different communities
mixtes = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29") # Mixt assemblages
forest = c("tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14") # Forest assemblages
encrus = c("tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28") # Encrusting assemblages

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
Tile_cover <- Tile_cover %>% left_join(corrected_names) %>% select(-c(Species, functional.group)) %>% 
  rename(Species = species_new) %>% filter(!grepl("dead", Species)) %>% 
  filter(Species != "tile") %>% left_join(Zone_pH)
Tile_cover_AMB  <- Tile_cover %>% dplyr::filter(pH == "AMB")
Tile_cover_LOW  <- Tile_cover %>% dplyr::filter(pH == "LOW")
Tile_cover_ELOW <- Tile_cover %>% dplyr::filter(pH == "ELOW")

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

## Define according to communities
Comm = data.frame(Communities = c(rep("Mixed", 6), rep("forest", 6), rep("encrusting", 6)),
                 Tile = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29",
                 "tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14",
                 "tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28"))
Cover_biomass = Cover_biomass %>% left_join(Comm)

# Modelling
model_split_data <- Cover_biomass %>% group_by(pH, Communities) %>% group_split()
model_weibull = list(NA,NA,NA,NA,NA,NA,NA,NA,NA) ; for (i in 1:9) {
  model_weibull[[i]] <- brm(bf(Biomass_std ~ a - b * nb_days^c + 0, a ~ 1, b ~ 1, c ~ 1, nl = TRUE), iter = 10000, warmup = 2000,
                            data = model_split_data[[i]], family = gaussian(), cores = 4, chains = 4,
                            prior = c(prior(normal(1, 0.01), nlpar = "a"), prior(normal(1, 1), nlpar = "b"), prior(normal(1, 1), nlpar = "c")),
                            control = list(adapt_delta = 0.95, max_treedepth = 10))}

training_data <- data.frame(nb_days = seq(0, 130, 0.01)) 
weibull_AMB_mix <- cbind(training_data, predict(model_weibull[[1]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_AMB_enc <- cbind(training_data, predict(model_weibull[[2]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_AMB_for <- cbind(training_data, predict(model_weibull[[3]], training_data)) %>% mutate(pH = rep("AMB",  dim(training_data)[1]))
weibull_LOW_mix <- cbind(training_data, predict(model_weibull[[7]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_LOW_enc <- cbind(training_data, predict(model_weibull[[8]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_LOW_for <- cbind(training_data, predict(model_weibull[[9]], training_data)) %>% mutate(pH = rep("LOW",  dim(training_data)[1]))
weibull_ELO_mix <- cbind(training_data, predict(model_weibull[[4]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))
weibull_ELO_enc <- cbind(training_data, predict(model_weibull[[5]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))
weibull_ELO_for <- cbind(training_data, predict(model_weibull[[6]], training_data)) %>% mutate(pH = rep("ELOW", dim(training_data)[1]))

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

# Fix positive ribbons at zero if higher than 1.2
weibull_AMB_enc$ribbon_pos[weibull_AMB_enc$ribbon_pos >= 1.2] = 1.2
  
A = ggplot() + ggtitle("Forest communities") + 
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
  scale_y_continuous(name = expression("Standardized biomass change"), 
                     breaks = seq(0, 1.2, 0.2), limits = c(0, 1.2), expand = c(0.02,0)) + 
  scale_x_continuous(name = "", breaks = c(0, 14, 42, 126), labels = c("T0", "T1", "T2", "T3"), expand = c(0.02,0)) +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        legend.position = "bottom")

B = ggplot() + ggtitle("Mixed communities") + 
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

C = ggplot() + ggtitle("Encrusting communities") + 
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

# Define values at T3
data_lolipop <- data.frame(pH = rep(c("AMB", "LOW", "ELO"), each = 3),
                           Communities = rep(c("Forest", "Mixed", "Encrusting"), 3),
                           Biomass = c(weibull_AMB_for$Estimate[weibull_AMB_for$nb_days == 126],
                                       weibull_AMB_mix$Estimate[weibull_AMB_mix$nb_days == 126],
                                       weibull_AMB_enc$Estimate[weibull_AMB_enc$nb_days == 126],
                                       weibull_LOW_for$Estimate[weibull_LOW_for$nb_days == 126],
                                       weibull_LOW_mix$Estimate[weibull_LOW_mix$nb_days == 126],
                                       weibull_LOW_enc$Estimate[weibull_LOW_enc$nb_days == 126],
                                       weibull_ELO_for$Estimate[weibull_ELO_for$nb_days == 126],
                                       weibull_ELO_mix$Estimate[weibull_ELO_mix$nb_days == 126],
                                       weibull_ELO_enc$Estimate[weibull_ELO_enc$nb_days == 126]),
                           x_location = c(9,10,11,5,6,7,1,2,3))
data_lolipop$Biomass[data_lolipop$Biomass < 0] = 0

D = ggplot(data_lolipop, aes(y = Biomass, x = x_location)) + 
  geom_segment(yend = 1, aes(color = pH), show.legend = F) +
  geom_segment(aes(x = 0, y = 1, xend = 11, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  geom_point(aes(shape = Communities, fill = pH), size = 3, color = "black", show.legend = F) +
  scale_color_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_fill_manual(values=c("royalblue3", "firebrick2", "goldenrod1"), labels = c("Ambient", "Extreme Low", "Low")) +
  scale_shape_manual(values=c(21, 23, 24), labels = c("Encrusting", "Mixed", "Forest")) +
  scale_y_continuous(name = expression("Biomass change at"~T[3]), breaks = seq(0, 1.2, 0.2), limits = c(0, 1.2), 
                     expand = c(0.02,0), labels = c("-1.0", "-0.8", "-0.6", "-0.4", "-0.2", "0.0", "+0.2")) +
  scale_x_continuous(name = "", limits = c(0, 12), expand = c(0.02,0)) +
  theme_classic() +
  theme(axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 16),
        legend.text     = element_text(size = 14),
        legend.title    = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.x     = element_text(size = 0),
        legend.position = "bottom")

Figure_2 <- A + B + C + D + plot_layout(guides = "collect", nrow = 1, widths = c(4,4,4,2))

### Define properly the model
# Define a mixed-effects model with splines and random slopes
model_perf <- brm(Biomass_std ~ (nb_days | pH) + (nb_days | Communities) + 0,
                  data = Cover_biomass, family = weibull(), cores = 4, chains = 4, iter = 10000,
                  warmup = 2000, inits = "0", control = list(adapt_delta = 0.95, max_treedepth = 10))
# Generate predictions
training_data <- data.frame(nb_days = seq(0, 130, 0.05))
predictions <- data.frame(nb_days = rep(training_data$nb_days, 9),
                          pH = factor(rep(rep(c("AMB", "LOW", "ELOW"), each = nrow(training_data)), 3)),
                          Communities = factor(rep(c("forest", "Mixed", "encrusting"), each = nrow(training_data)*3)))
predictions$Biomass_std <- predict(model_perf, newdata = predictions, allow_new_levels = TRUE)
# Performance
bayes_R2(model_perf)
#openxlsx::write.xlsx(predictions, file = "Outputs/Summary/biomass_change_model.xlsx", rowNames = FALSE)
#openxlsx::write.xlsx(Cover_biomass, file = "Outputs/Summary/Cover_biomass.xlsx", rowNames = FALSE)

ggsave(Figure_2, file = "Outputs/Figures/Biomass/Biomass_changes_communities.png", width = 30, 
       height = 9, units = "cm", dpi = 300)