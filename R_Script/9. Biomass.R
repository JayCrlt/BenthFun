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
Tile_cover <- Tile_cover %>% left_join(corrected_names) %>% select(-Species) %>% 
  rename(Species = species_new) %>% filter(!grepl("dead", Species)) %>% 
  filter(Species != "tile")
ggplot(Tile_cover, aes(x = Time, y = Cover, fill = Species)) + geom_bar(stat = "identity") + facet_wrap(~Tile)

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
pH_tiles <- rbind(Tile_cover_AMB, Tile_cover_LOW, Tile_cover_ELOW) %>% data.frame() %>% select(Tile, pH) %>% distinct()
Time_numeric <- data.frame(Time = c("T0", "T1", "T2", "T3"), nb_days = c(0, 14, 42, 126))

## Biomass
Cover_biomass = rbind(Biomass_avg, data_assumptions) %>% left_join(Tile_cover, by = "Species") %>% 
  mutate(Biomass = dry_weight_0.01_avg * Cover) %>% 
  group_by(Tile, Time) %>% summarise(Biomass = sum(Biomass)) %>% drop_na() %>% 
  right_join(pH_tiles) %>% right_join(Time_numeric)

Cover_biomass_init = Cover_biomass %>% dplyr::filter(Time == "T0") %>% rename(Biomass_init = Biomass, Time_init = Time, nb_days_init = nb_days)
Cover_biomass = Cover_biomass %>% left_join(Cover_biomass_init, by = c("pH", "Tile")) %>% mutate(Biomass_std = Biomass / Biomass_init) 

i = 1
(weibull_fit <- nls(Biomass_std ~ alpha - beta * nb_days^gamma,
                   data = model_split_data[[i]],
                   start = list(alpha = 1, beta = 0.1, gamma = 1)))

# Modelling
model_split_data <- Cover_biomass %>% group_by(pH) %>% group_split()
model_weibull = list(NA,NA,NA) ; for (i in 1:3) {
  model_weibull[[i]] <- brm(bf(Biomass_std ~ a - b * nb_days^c + 0, a ~ 1, b ~ 1, c ~ 1, nl = TRUE), iter = 5000, warmup = 1000,
                               data = model_split_data[[i]], family = gaussian(), cores = 4, chains = 4,
                               prior = c(prior(normal(1, 0.01), nlpar = "a"), prior(normal(1, 1), nlpar = "b"), prior(normal(1, 1), nlpar = "c")),
                               control = list(adapt_delta = 0.95, max_treedepth = 10))
}

training_data <- data.frame(nb_days = seq(0,142,1)) 
weibull_AMB <- cbind(training_data, predict(model_weibull[[1]], training_data)) %>% mutate(pH = rep("AMB",  143))
weibull_LOW <- cbind(training_data, predict(model_weibull[[3]], training_data)) %>% mutate(pH = rep("LOW",  143))
weibull_ELO <- cbind(training_data, predict(model_weibull[[2]], training_data)) %>% mutate(pH = rep("ELOW", 143))

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
weibull_tot <- rbind(weibull_AMB, weibull_LOW, weibull_ELO)

ggplot(weibull_tot, aes(x = nb_days, y = Estimate)) + geom_line(aes(group = pH)) #+ geom_point(aes(color = pH, y = Biomass_std))

weibull_tot %>% mutate(pH = fct_relevel(pH, c("ELOW", "LOW", "AMB"))) %>%
ggplot(aes(x = nb_days, y = Estimate, col = pH)) + 
  geom_ribbon(aes(x = nb_days, ymin = `Q2.5`, ymax = `Q97.5`, fill = pH), alpha = .5) + theme_classic() +
  geom_segment(aes(x = 0, y = 1, xend = 142, yend = 1), colour = "black", linetype = "dotted", size = .5) +
  geom_smooth(method = 'nls', se=F, formula = y~a-b*x^c, start = c(a=1, b=1, c=1)) + 
  geom_line(aes(group = pH), size = 1) +
  scale_color_manual(values=c("firebrick2","goldenrod1","royalblue3")) + 
  scale_fill_manual(values=c("firebrick2","goldenrod1","royalblue3")) +
  scale_y_continuous(name = expression("Std biomass")) + 
  scale_x_continuous(name = expression("number of days (days)"))
 
  fit2 <- brm(Biomass_std ~ nb_days | pH + 1, cores = 4, chains = 4, iter = 5000, warmup = 1000,
              data = model_split_data[[1]], family = weibull(), inits = "0",
              control = list(adapt_delta = 0.95, max_treedepth = 10))

test_LOW <- cbind(model_split_data[[1]], predict(fit2, model_split_data[[1]]))
ggplot(test_LOW, aes(x = nb_days, y = Estimate)) + geom_line(aes(group = Tile)) + geom_point(aes(color = pH, y = Biomass_std))
    

xlsx::write.xlsx(Cover_biomass %>% data.frame(), file = "Cover_biomass.xlsx")
getwd()
