### Biomass
options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)
source("R_Script/8. Covers_communities.R")

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets("Data/7. Covers/20230725_historic_tiles_species_visual_census_25subquadrats.xlsx")
# Read each sheet into a separate dataset and rename it
datasets <- lapply(sheet_names, function(sheet) {
  read_excel("Data/7. Covers/20230725_historic_tiles_species_visual_census_25subquadrats.xlsx", sheet = sheet)})

# With corrected names
corrected_names <- read_excel("Data/7. Covers/corrected_names.xlsx", sheet = "Historic") 
# Load the biomass
Biomass  <- read_excel("Data/4. Visual census/Species Biomass Weight/Relationship biomass – cover.xlsx")
Scraping <- read_excel("Data/4. Visual census/Species Biomass Weight/Biomass_Species.xlsx")
Zone_pH  <- data_frame(Tile = sheet_names, pH = c(rep("ELOW", 6), rep("LOW", 6), rep("AMB", 6)))

# Define the taxonomic group
table(corrected_names$taxonomy)

#######################################################################################################################
#######################################################################################################################
#######################################
###### ---- COVER & BIOMASS ---- ######
#######################################
#######################################################################################################################
#######################################################################################################################

# Total cover for each tile Light
Tile_cover_T0 = vector("list", 18) ; Tile_cover_T1 = vector("list", 18) ; Tile_cover = vector("list", 18)
for (i in 1:18) {
  Tile_cover_T0[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t1_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T0" = ".") %>% mutate(T0 = ((T0/25*100) / sum(T0/25*100)) * 100)
  Tile_cover_T1[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t3_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T1" = ".") %>% mutate(T1 = ((T1/25*100) / sum(T1/25*100)) * 100)
  Tile_cover[[i]] <- bind_cols(Tile_cover_T0[[i]], Tile_cover_T1[[i]]) %>%
    data.frame() %>% filter(T0 != 0 | T1 != 0) %>% rownames_to_column(., var = "Species")
  Tile_cover[[i]] <- data.frame(Species = rep(Tile_cover[[i]]$Species, 2),
                                Tile    = rep(sheet_names[i], length(Tile_cover[[i]]$Species)*2),
                                Time    = c(rep("T0", length(Tile_cover[[i]]$Species)), rep("T1", length(Tile_cover[[i]]$Species))),
                                Cover   = c(Tile_cover[[i]]$T0, Tile_cover[[i]]$T1)) }
Tile_cover = bind_rows(Tile_cover) %>% filter(!grepl("dead", Species)) 
Tile_cover <- Tile_cover %>% left_join(corrected_names) %>% select(-c(Species, functional.group)) %>% 
  rename(Species = species_new) %>% filter(!grepl("dead", Species)) %>% 
  filter(Species != "tile") %>% left_join(Zone_pH)

# Create a lookup table for taxonomy levels
taxonomy_order <- c("tile", "Turf", "Chlorophyta", "Phaeophyceae", "Rhodophyta", 
                    "Porifera", "Bryozoa", "Polychaeta", "Crustacea", "Mollusca", "Tunicates")

Tile_cover <- Tile_cover %>% mutate(taxonomy_numeric = match(taxonomy, taxonomy_order),
                                    combined_factor = paste(taxonomy, Species, sep = ":")) %>%
  mutate(Species = fct_reorder(Species, as.numeric(factor(combined_factor)))) %>%
  select(-combined_factor)
         
Tile_cover_AMB  <- Tile_cover %>% dplyr::filter(pH == "AMB")
Tile_cover_LOW  <- Tile_cover %>% dplyr::filter(pH == "LOW")
Tile_cover_ELOW <- Tile_cover %>% dplyr::filter(pH == "ELOW")

# We will add a new information with the Zone
Zone_pH       <- data_frame(Tile = sheet_names, pH = c(rep("ELOW", 6), rep("LOW", 6), rep("AMB", 6)))
Tile_cover    <- Tile_cover %>% left_join(Zone_pH) 
Tile_cover$pH <- factor(Tile_cover$pH, levels = c("ELOW", "LOW", "AMB"))

col_AMB = Tile_cover_AMB %>% distinct(Species, Color) %>% arrange(Species)
A = ggplot(data = Tile_cover_AMB, aes(x = factor(Time, levels = c("T0", "T1")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) + facet_grid(~Tile) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = col_AMB$Color) +
  ggtitle("Ambient – Front") + theme_ambient(panel_background_color = "white")

col_LOW = Tile_cover_LOW %>% distinct(Species, Color) %>% arrange(Species)
B = ggplot(data = Tile_cover_LOW, aes(x = factor(Time, levels = c("T0", "T1")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) + facet_grid(~Tile) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = col_LOW$Color) +
  ggtitle("Ambient – Front") + theme_low(panel_background_color = "white")

col_ELO = Tile_cover_ELOW %>% distinct(Species, Color) %>% arrange(Species)
C = ggplot(data = Tile_cover_ELOW, aes(x = factor(Time, levels = c("T0", "T1")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) + facet_grid(~Tile) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = col_ELO$Color) +
  ggtitle("Ambient – Front") + theme_extreme_low(panel_background_color = "white")

# Front cover (/50)
Tile_cover_T0 = vector("list", 18) ; Tile_cover_T1 = vector("list", 18) ; Tile_cover = vector("list", 18)
for (i in 1:18) {
  Tile_cover_T0[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t1_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T0" = ".") %>% mutate(T0 = ((T0/50*100)))
  Tile_cover_T1[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t3_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T1" = ".") %>% mutate(T1 = ((T1/50*100)))
  
  Tile_cover[[i]] <- bind_cols(Tile_cover_T0[[i]], Tile_cover_T1[[i]]) %>%
    data.frame() %>% filter(T0 != 0 | T1 != 0) %>% rownames_to_column(., var = "Species")
  Tile_cover[[i]] <- data.frame(Species = rep(Tile_cover[[i]]$Species, 2),
                                Tile    = rep(sheet_names[i], length(Tile_cover[[i]]$Species)*2),
                                Time    = c(rep("T0", length(Tile_cover[[i]]$Species)), rep("T1", length(Tile_cover[[i]]$Species))),
                                Cover   = c(Tile_cover[[i]]$T0, Tile_cover[[i]]$T1)) }
Tile_cover = bind_rows(Tile_cover) %>% filter(!grepl("dead", Species)) 
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

## Historic new_species
data_assumptions_his <- data.frame(Species = c("Schizomavella mamillata", "Turbicellepora magnicostata", "Halimeda tuna",
                                               "Lobophora variegata", "Sphacelaria cirrosa", "Haliclona sp.", "Leucandra gossei",
                                               "Neogoniolithon brassica florida", "Didemnum pseudofulgens", "Polysyncraton lacazei",
                                               "Symplegma brakenhielmi"),
                                   dry_weight_0.01_avg = c(0.501, 0.501, 0.449, 0.648, 0.648, 0.743, 0.976, 0.976, 0.743, 0.743, 0.743)) %>% 
  mutate(dry_weight_0.01_sd = dry_weight_0.01_avg * rnorm(1, mean(Biomass_avg$dry_weight_0.01_avg), mean(Biomass_avg$dry_weight_0.01_avg)/10))

# Extract pH Zone
pH_tiles <- rbind(data.frame(Tile_cover_AMB,  pH = rep("AMB", length(Tile_cover_AMB$Tile))), 
                  data.frame(Tile_cover_LOW,  pH = rep("LOW", length(Tile_cover_LOW$Tile))), 
                  data.frame(Tile_cover_ELOW, pH = rep("ELOW", length(Tile_cover_ELOW$Tile)))) %>% data.frame() %>% select(Tile, pH) %>% distinct()
Time_numeric <- data.frame(Time = c("T0", "T1"), nb_days = c(0, 97))

## Overall Biomass
Biomass_tot <- rbind(Biomass_avg, data_assumptions, data_assumptions_his)
Cover_biomass = Biomass_tot %>% left_join(Tile_cover, by = "Species") %>% right_join(pH_tiles) %>% 
  mutate(Biomass = if_else(Species == "Turf" & pH == "ELOW", dry_weight_0.01_avg * Cover /10, dry_weight_0.01_avg * Cover)) %>% 
  group_by(Tile, pH, Time) %>% summarise(Biomass = sum(Biomass)) %>% drop_na() %>% right_join(Time_numeric)

Cover_biomass %>% dplyr::filter(pH == "LOW") %>% 
  ggplot(aes(x = nb_days, y = Biomass, color = Tile)) + geom_line() + facet_grid(~pH)

A + B + C + ggplot(Cover_biomass, aes(x = nb_days, y = Biomass, color = Tile)) + geom_line() + facet_grid(~pH) +
  patchwork::plot_layout(ncol = 2)

### Time to talk with communities
Tile_cover$Comm[Tile_cover$Tile == "tile_05amb"] = "Encrusting"
Tile_cover$Comm[Tile_cover$Tile %in% c("tile_01am", "tile_03amb", "tile_07amb", "tile_08amb", 
                "tile_05low", "tile_01low", "tile_02low")] = "Mixed"
Tile_cover$Comm[is.na(Tile_cover$Comm)] = "Forest"

Tile_Comm <- table(Tile_cover$Tile, Tile_cover$Comm) %>% data.frame() %>% dplyr::filter(Freq > 0) %>% 
  data.frame() %>% rename("Tile" = "Var1", "Comm" = "Var2") %>% 
  dplyr::select(-Freq)

### For biomass, there is no significant change over time
Biomass_hist = Cover_biomass %>% full_join(Tile_Comm) %>% group_by(pH, Time, nb_days, Comm) %>% 
  summarise(Biomass_avg = mean(Biomass), Biomass_sd = sd(Biomass)) %>% 
  mutate(ymin = Biomass_avg - Biomass_sd, ymax = Biomass_avg + Biomass_sd,
         pH = as.factor(pH), pH = fct_relevel(pH, c("ELOW", "LOW", "AMB")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting"))) %>%  
  ggplot(aes(x = Time, y = Biomass_avg, fill = pH)) + 
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge") +
  scale_y_continuous(name = "Average biomass (g)") +
  facet_grid(~Comm) +
  scale_color_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  scale_fill_manual(values=c("firebrick2", "goldenrod1", "royalblue3"), labels = c("Extreme Low", "Low", "Ambient")) +
  theme_ambient(panel_background_color = "white") + 
  theme(axis.text        = element_text(size = 14),
        axis.title       = element_text(size = 16),
        legend.text      = element_text(size = 14))

# Total cover for each tile Light
Tile_cover_T0 = vector("list", 18) ; Tile_cover_T1 = vector("list", 18) ; Tile_cover = vector("list", 18)
for (i in 1:18) {
  Tile_cover_T0[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t1_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T0" = ".") %>% mutate(T0 = ((T0/25*100) / sum(T0/25*100)) * 100)
  Tile_cover_T1[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t3_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T1" = ".") %>% mutate(T1 = ((T1/25*100) / sum(T1/25*100)) * 100)
  Tile_cover[[i]] <- bind_cols(Tile_cover_T0[[i]], Tile_cover_T1[[i]]) %>%
    data.frame() %>% filter(T0 != 0 | T1 != 0) %>% rownames_to_column(., var = "Species")
  Tile_cover[[i]] <- data.frame(Species = rep(Tile_cover[[i]]$Species, 2),
                                Tile    = rep(sheet_names[i], length(Tile_cover[[i]]$Species)*2),
                                Time    = c(rep("T0", length(Tile_cover[[i]]$Species)), rep("T1", length(Tile_cover[[i]]$Species))),
                                Cover   = c(Tile_cover[[i]]$T0, Tile_cover[[i]]$T1)) }
Tile_cover = bind_rows(Tile_cover) %>% filter(!grepl("dead", Species)) 
Tile_cover <- Tile_cover %>% left_join(corrected_names) %>% select(-c(Species, functional.group)) %>% 
  rename(Species = species_new) %>% filter(!grepl("dead", Species)) %>% 
  filter(Species != "tile") %>% left_join(Zone_pH)

# For taxonomic composition
Cover_hist = Tile_cover %>% full_join(Tile_Comm) %>% group_by(pH, Time, taxonomy, Comm) %>% 
  summarise(Cover_avg = sum(Cover)) %>%
  mutate(Cover_avg = case_when(
    pH == "ELOW" ~ Cover_avg / 6,
    pH == "LOW" ~ Cover_avg / 3,
    pH == "AMB" & Comm == "Mixed" ~ Cover_avg / 4,
    TRUE ~ Cover_avg)) %>% 
  mutate(Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         pH = fct_relevel(pH, c("ELOW", "LOW", "AMB"))) 

# Colors
taxonomy_order <- c("Turf", "Chlorophyta", "Phaeophyceae", "Rhodophyta", "Porifera", 
                    "Bryozoa", "Polychaeta", "Crustacea", "Tunicates")
color_order    <- c("#cae4b9", "#72b744", "#9a6618", "#e47fba", "#ff9b32", "#ffcc00", "#3d85c6", 
                    "#a2c4c9", "#8571b8")
data_col = data.frame(taxonomy = taxonomy_order, Color = color_order)
data_col$taxonomy   <- factor(data_col$taxonomy, levels = taxonomy_order)
Cover_hist$taxonomy <- factor(Cover_hist$taxonomy, levels = taxonomy_order)

Cover_Hist = Cover_hist %>% 
  mutate(Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         pH = fct_relevel(pH, c("AMB", "LOW", "ELOW"))) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1")), y = Cover_avg, fill = taxonomy)) +
  geom_col(position = "stack", color = "black", show.legend = T) +
  scale_x_discrete() + facet_grid(pH~Comm) +
  labs(x = "Time", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$taxonomy %in% unique(Cover_simplified$taxonomy)]) + 
  theme_ambient(panel_background_color = "white") + 
  theme(axis.text        = element_text(size = 14),
        axis.title       = element_text(size = 16),
        legend.text      = element_text(size = 14))

Cover_historic = Biomass_hist + Cover_Hist & theme(legend.position = "bottom")

#######################################################################################################################
#######################################################################################################################
#################################
###### ---- NUTRIENTS ---- ######
#################################
#######################################################################################################################
#######################################################################################################################

### Data
Nutrients  <- read_excel("Data/6. Nutrients/Nutrients.xlsx", sheet = "Sheet1") %>% select(-c(`DATA ANALYSIS`, Package))
Diving_log <- read_excel("Data/1. Diving log/Diving_log_BenthFun.xlsx", 
                         col_types = c("date", "text", "text", "date", "date", 
                                       "date", "text", "text", "numeric", "numeric"),
                         sheet = "Corrected") %>% 
  mutate(., Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S"))

# Transplants
Transplants_nut <- Nutrients %>% dplyr::filter(Experiment == "Historic") %>% group_by(Phase,pH) %>% group_split()

{for (j in 1:6) {
  T0 <- Transplants_nut[[j]][1:2,] %>%
    mutate(Sample = case_when(row_number() == 1 ~ "T0", row_number() == 2 ~ "T0", TRUE ~ as.character(Sample))) %>%
    group_by(Experiment, Phase, pH, Sample) %>% summarise_all(c(mean))
  Blank <- Transplants_nut[[j]][9:10,] %>% 
    mutate(Sample = case_when(row_number() == 1 ~ "Blank", row_number() == 2 ~ "Blank", TRUE ~ as.character(Sample))) %>%
    group_by(Experiment, Phase, pH, Sample) %>% summarise_all(c(mean))
  Transplants_nut[[j]] <- rbind(T0, Transplants_nut[[j]][3:8,], Blank)
  for (i in 1:7) {Transplants_nut[[j]][i+1,5:9] = Transplants_nut[[j]][i+1,5:9] - Transplants_nut[[j]][1, 5:9] - 
    Transplants_nut[[j]][8, 5:9]} # final - initial - blank_correction
  Transplants_nut[[j]] <- Transplants_nut[[j]][2:7,]}
Transplants_nut = Transplants_nut %>% bind_rows()
for (i in 1:36) {
  Transplants_nut$pH[i] <- strsplit(Transplants_nut$Sample, "_")[[i]][3]
  Transplants_nut$Tile[i] <- paste(strsplit(Transplants_nut$Sample, "_")[[i]][4:5], collapse = "_")}}

### Communities
Transplants_nut$Comm[Transplants_nut$Tile == "tile_06" & Transplants_nut$pH == "AMB"] = "Encrusting"
Transplants_nut$Comm[Transplants_nut$Tile %in% c("tile_01", "tile_03", "tile_04", "tile_05") & 
                       Transplants_nut$pH == "AMB"] = "Mixed"
Transplants_nut$Comm[Transplants_nut$Tile %in% c("tile_01", "tile_02", "tile_05") & 
                       Transplants_nut$pH == "LOW"] = "Mixed"
Transplants_nut$Comm[is.na(Transplants_nut$Comm)] = "Forest"
Transplants_nut <- Transplants_nut %>%
  mutate(Phase = if_else(Phase == "Tn", "T0", Phase),
         Phase = if_else(Phase == "Tn2", "T1", Phase))

# Biomass
Cover_biomass = Cover_biomass %>% mutate(Tile = substr(Tile, 1, 7))
Cover_biomass$Tile[Cover_biomass$Tile == "tile_05" & Cover_biomass$pH == "AMB"] = "tile_06"
Cover_biomass$Tile[Cover_biomass$Tile == "tile_07" & Cover_biomass$pH == "AMB"] = "tile_04"
Cover_biomass$Tile[Cover_biomass$Tile == "tile_08" & Cover_biomass$pH == "AMB"] = "tile_05"

Transplants_nut = Transplants_nut %>% rename("Time" = "Phase") %>% 
  full_join(Cover_biomass, by = join_by(pH, Tile, Time))

Transplants_nut_nmol_g <- Transplants_nut %>%
  mutate(`NH3 (mmol m-3)` = `NH3 (mmol m-3)`*1000 / Biomass,
         `NO3 (mmol m-3)` = `NO3 (mmol m-3)`*1000 / Biomass,
         `PO4 (mmol m-3)` = `PO4 (mmol m-3)`*1000 / Biomass) 

# To help the model, we add the pH value
Transplants_nut_nmol_g <- data.frame(pH_value = c(6.438064, 7.6959125, 8.0215523), 
                                     pH = c("ELOW", "LOW", "AMB")) %>% 
  mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% right_join(Transplants_nut_nmol_g)

# Model_data
Transplants_nut_nmol_g_T0 <- Transplants_nut_nmol_g %>% dplyr::filter(Time == "T0") %>% 
  rename("NH3_init" = `NH3 (mmol m-3)`, "NO3_init" = `NO3 (mmol m-3)`, "PO4_init" = `PO4 (mmol m-3)`) %>% 
  select(pH, Tile, Comm, NH3_init, NO3_init, PO4_init)

Transplants_nut_model <- Transplants_nut_nmol_g %>% full_join(Transplants_nut_nmol_g_T0) %>% 
  mutate(NH3_Change = `NH3 (mmol m-3)` / NH3_init, 
         NO3_Change = `NO3 (mmol m-3)` / NO3_init,
         PO4_Change = `PO4 (mmol m-3)` / PO4_init)

##### Model functions
training_data <- expand.grid(nb_days = seq(0,100,.05),
                             Comm = c("Mixed", "Forest", "Encrusting"), pH_value = c(6.438064, 7.6959125, 8.0215523))
# NH3
NH3_model_his <- brm(NH3_Change ~ (nb_days + 0 | Comm) + (nb_days + 0 | pH_value) +0, init = "0",
                 data = Transplants_nut_model, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NH3_model_his) # R2 = 56%
training_data_NH3 = cbind(training_data, predict(NH3_model_his, training_data))
training_data_NH3 = training_data_NH3 %>% 
  full_join(data.frame(pH_value = c(6.438064, 7.6959125, 8.0215523), 
                       pH = c("ELOW", "LOW", "AMB")) %>% 
              mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))))
# change back the sign
training_data_NH3$Estimate = -training_data_NH3$Estimate 
(NH3_plot = ggplot(training_data_NH3, aes(y = Estimate, x = nb_days, color = pH, shape = Comm)) + 
    geom_point() + scale_y_continuous(limits = c(-3,0)))

# NO3
Transplants_nut_model_NO3 = Transplants_nut_model %>% dplyr::filter(NO3_Change > - 5, `NO3 (mmol m-3)` < 2) %>% 
  mutate(NO3_Change = abs(NO3_Change))
NO3_model_his <- brm(NO3_Change ~ (nb_days + 0 | Comm) + (nb_days + 0 | pH_value) +0, init = "0",
                     data = Transplants_nut_model_NO3, family = weibull(), cores = 4, chains = 4, iter = 10000,
                     warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(NO3_model_his) # R2 = 76%
training_data_NO3 = cbind(training_data, predict(NO3_model_his, training_data))
training_data_NO3 = training_data_NO3 %>% 
  full_join(data.frame(pH_value = c(6.438064, 7.6959125, 8.0215523), 
                       pH = c("ELOW", "LOW", "AMB")) %>% 
              mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))))
# change back the sign
training_data_NO3$Estimate = -training_data_NO3$Estimate 
(NO3_plot = ggplot(training_data_NO3, aes(y = Estimate, x = nb_days, color = pH, shape = Comm)) + 
    geom_point() + scale_y_continuous(limits = c(-3,0)))

# PO4
Transplants_nut_model_PO4 = Transplants_nut_model %>% dplyr::filter(PO4_Change > - 5 & PO4_Change < 10,
                                                                    `PO4 (mmol m-3)` < 0,
                                                                    Sample != "Tn_t1_AMB_tile_04") %>% 
  mutate(PO4_Change = abs(PO4_Change))
PO4_model_his <- brm(PO4_Change ~ (nb_days + 0 | Comm) + (nb_days + 0 | pH_value) +0, init = "0",
                     data = Transplants_nut_model_PO4, family = weibull(), cores = 4, chains = 4, iter = 10000,
                     warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(PO4_model_his) # R2 = 41%
training_data_PO4 = cbind(training_data, predict(PO4_model_his, training_data))
training_data_PO4 = training_data_PO4 %>% 
  full_join(data.frame(pH_value = c(6.438064, 7.6959125, 8.0215523), 
                       pH = c("ELOW", "LOW", "AMB")) %>% 
              mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))))
# change back the sign
training_data_PO4$Estimate = -training_data_PO4$Estimate
(PO4_plot = ggplot(training_data_PO4, aes(y = Estimate, x = nb_days, color = pH, shape = Comm)) + 
    geom_point() + scale_y_continuous(limits = c(-5,0)))

### Define the Modeled value

######### NH3 ----
training_data_NH3_values = training_data_NH3 %>% dplyr::filter(nb_days == 100)
cste = 3.7 ; Sa = 1
(T0 = Transplants_nut_model %>% dplyr::filter(Time == "T0") %>% drop_na() %>% 
    group_by(pH, Comm) %>% 
    summarise(mean_T0 = mean(`NH3 (mmol m-3)` * cste / Sa)))
training_data_NH3_values =
  training_data_NH3_values %>% dplyr::filter(Comm %notin% c("Encrusting", "Mixed") | pH != "ELOW",
                                             Comm != "Encrusting" | pH != "LOW")
# Estimate
training_data_NH3_values$Estimate[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Encrusting"] = 
  -training_data_NH3_values$Estimate[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_NH3_values$Estimate[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Forest"] = 
  -training_data_NH3_values$Estimate[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_NH3_values$Estimate[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Mixed"] = 
  -training_data_NH3_values$Estimate[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_NH3_values$Estimate[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Forest"] = 
  -training_data_NH3_values$Estimate[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_NH3_values$Estimate[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Mixed"] = 
  -training_data_NH3_values$Estimate[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_NH3_values$Estimate[training_data_NH3_values$pH == "ELOW" & training_data_NH3_values$Comm == "Forest"] = 
  -training_data_NH3_values$Estimate[training_data_NH3_values$pH == "ELOW" & training_data_NH3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# Est.Error
training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Encrusting"] = 
  -training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Forest"] = 
  -training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Mixed"] = 
  -training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "AMB" & training_data_NH3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Forest"] = 
  -training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Mixed"] = 
  -training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "LOW" & training_data_NH3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "ELOW" & training_data_NH3_values$Comm == "Forest"] = 
  -training_data_NH3_values$Est.Error[training_data_NH3_values$pH == "ELOW" & training_data_NH3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# dataset
Nut_NH3 = data.frame(pH         = training_data_NH3_values$pH,
                     Comm       = training_data_NH3_values$Comm,
                     Estimate   = training_data_NH3_values$Estimate,
                     Est.Error  = training_data_NH3_values$Est.Error,
                     ribbon_neg = training_data_NH3_values$Estimate - training_data_NH3_values$Est.Error,
                     ribbon_pos = training_data_NH3_values$Estimate + training_data_NH3_values$Est.Error)

######### NO3 ----
training_data_NO3_values = training_data_NO3 %>% dplyr::filter(nb_days == 100)
cste = 3.7 ; Sa = 1
(T0 = Transplants_nut_model_NO3 %>% dplyr::filter(Time == "T0") %>% drop_na() %>% 
    group_by(pH, Comm) %>% 
    summarise(mean_T0 = mean(`NO3 (mmol m-3)` * cste / Sa)))
training_data_NO3_values =
  training_data_NO3_values %>% dplyr::filter(Comm %notin% c("Encrusting", "Mixed") | pH != "ELOW",
                                           Comm != "Encrusting" | pH != "LOW")
# Estimate
training_data_NO3_values$Estimate[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Encrusting"] = 
  -training_data_NO3_values$Estimate[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_NO3_values$Estimate[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Forest"] = 
  -training_data_NO3_values$Estimate[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_NO3_values$Estimate[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Mixed"] = 
  -training_data_NO3_values$Estimate[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_NO3_values$Estimate[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Forest"] = 
  -training_data_NO3_values$Estimate[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_NO3_values$Estimate[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Mixed"] = 
  -training_data_NO3_values$Estimate[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_NO3_values$Estimate[training_data_NO3_values$pH == "ELOW" & training_data_NO3_values$Comm == "Forest"] = 
  -training_data_NO3_values$Estimate[training_data_NO3_values$pH == "ELOW" & training_data_NO3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# Est.Error
training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Encrusting"] = 
  -training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Forest"] = 
  -training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Mixed"] = 
  -training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "AMB" & training_data_NO3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Forest"] = 
  -training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Mixed"] = 
  -training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "LOW" & training_data_NO3_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "ELOW" & training_data_NO3_values$Comm == "Forest"] = 
  -training_data_NO3_values$Est.Error[training_data_NO3_values$pH == "ELOW" & training_data_NO3_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# dataset
Nut_NO3 = data.frame(pH         = training_data_NO3_values$pH,
                     Comm       = training_data_NO3_values$Comm,
                     Estimate   = training_data_NO3_values$Estimate,
                     Est.Error  = training_data_NO3_values$Est.Error,
                     ribbon_neg = training_data_NO3_values$Estimate - training_data_NO3_values$Est.Error,
                     ribbon_pos = training_data_NO3_values$Estimate + training_data_NO3_values$Est.Error)

######### PO4 ----
training_data_PO4_values = training_data_PO4 %>% dplyr::filter(nb_days == 100)
cste = 3.7 ; Sa = 1
(T0 = Transplants_nut_model_PO4 %>% dplyr::filter(Time == "T0") %>% drop_na() %>% 
    group_by(pH, Comm) %>% 
    summarise(mean_T0 = mean(`PO4 (mmol m-3)` * cste / Sa)))
training_data_PO4_values = 
  training_data_PO4_values %>% dplyr::filter(Comm %notin% c("Encrusting", "Mixed") | pH != "ELOW",
                                           Comm != "Encrusting" | pH != "LOW")
# Estimate
training_data_PO4_values$Estimate[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Encrusting"] = 
  -training_data_PO4_values$Estimate[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_PO4_values$Estimate[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Forest"] = 
  -training_data_PO4_values$Estimate[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_PO4_values$Estimate[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Mixed"] = 
  -training_data_PO4_values$Estimate[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_PO4_values$Estimate[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Forest"] = 
  -training_data_PO4_values$Estimate[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_PO4_values$Estimate[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Mixed"] = 
  -training_data_PO4_values$Estimate[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_PO4_values$Estimate[training_data_PO4_values$pH == "ELOW" & training_data_PO4_values$Comm == "Forest"] = 
  -training_data_PO4_values$Estimate[training_data_PO4_values$pH == "ELOW" & training_data_PO4_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# Est.Error
training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Encrusting"] = 
  -training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Forest"] = 
  -training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Mixed"] = 
  -training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "AMB" & training_data_PO4_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Forest"] = 
  -training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Mixed"] = 
  -training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "LOW" & training_data_PO4_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "ELOW" & training_data_PO4_values$Comm == "Forest"] = 
  -training_data_PO4_values$Est.Error[training_data_PO4_values$pH == "ELOW" & training_data_PO4_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# dataset
Nut_PO4 = data.frame(pH         = training_data_PO4_values$pH,
                     Comm       = training_data_PO4_values$Comm,
                     Estimate   = training_data_PO4_values$Estimate,
                     Est.Error  = training_data_PO4_values$Est.Error,
                     ribbon_neg = training_data_PO4_values$Estimate - training_data_PO4_values$Est.Error,
                     ribbon_pos = training_data_PO4_values$Estimate + training_data_PO4_values$Est.Error)

##### Nutrients uptakes final
Nut_uptakes <- rbind(Nut_NH3, Nut_NO3, Nut_PO4) %>% mutate(Nut = c(rep("NH4", 6), rep("NO3", 6), rep("PO4", 6))) # Unit = nmol/g/h
Nut_uptakes <- read_excel("Outputs/Summary/Nutrients_uptakes_historic.xlsx")

### For biomass, there is no significant change over time
Nut_uptakes %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("ELOW", "LOW", "AMB")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting"))) %>%  
  ggplot(aes(x = Nut, y = log(abs(Estimate)), fill = log(abs(Estimate)))) + 
  coord_polar() +
  geom_col(position = "dodge", color = "black", width = 1) + 
  scale_y_continuous(name = "Nutrients uptakes (-log scale)") +
  facet_grid(pH~Comm) +
  theme_ambient(panel_background_color = "white") + 
  theme(axis.text        = element_text(size = 14),
        axis.title       = element_text(size = 16),
        legend.text      = element_text(size = 14))

#######################################################################################################################
#######################################################################################################################
#################################
###### ---- PROCESSES ---- ######
#################################
#######################################################################################################################
#######################################################################################################################

# Load data
Functions <- read_excel("Outputs/Summary/Summary_Process_BenthFun.xlsx") %>% dplyr::filter(Main_Exp == "Historical")
PAR_tiles <- read_excel("Outputs/Summary/PAR_Historic.xlsx") 

# Fix some values
Functions$avg_output[Functions$Process == "dark respiration rate" & Functions$avg_output > 0] = 0
Functions$avg_output[Functions$Process == "net photosynthesis rate"] = 
  Functions$avg_output[Functions$Process == "gross photosynthesis rate"] -
  Functions$avg_output[Functions$Process == "dark respiration rate"]

Biomass_calc = Biomass_tot %>% left_join(Tile_cover, by = "Species") %>% right_join(pH_tiles) %>% dplyr::filter(calcareous == 1) %>% 
  mutate(Biomass = if_else(Species == "Turf" & pH == "ELOW", dry_weight_0.01_avg * Cover /10, dry_weight_0.01_avg * Cover)) %>% 
  group_by(Tile, pH, Time) %>% summarise(Biomass_cal = sum(Biomass)) %>% drop_na() %>% right_join(Time_numeric)
Biomass_overall = Biomass_tot %>% left_join(Tile_cover, by = "Species") %>% right_join(pH_tiles) %>% 
  mutate(Biomass = if_else(Species == "Turf" & pH == "ELOW", dry_weight_0.01_avg * Cover /10, dry_weight_0.01_avg * Cover)) %>% 
  group_by(Tile, pH, Time) %>% summarise(Biomass_overall = sum(Biomass)) %>% drop_na() %>% right_join(Time_numeric)
Biomass_npp = Biomass_tot %>% left_join(Tile_cover, by = "Species") %>% right_join(pH_tiles) %>% dplyr::filter(primary.producers == 1) %>% 
  mutate(Biomass = if_else(Species == "Turf" & pH == "ELOW", dry_weight_0.01_avg * Cover /10, dry_weight_0.01_avg * Cover)) %>% 
  group_by(Tile, pH, Time) %>% summarise(Biomass_npp = sum(Biomass)) %>% drop_na() %>% right_join(Time_numeric)

Biomass_his <- Biomass_overall %>% 
  full_join(Biomass_calc, by = join_by(Tile, pH, Time, nb_days)) %>% 
  full_join(Biomass_npp, by = join_by(Tile, pH, Time, nb_days)) %>% 
  mutate_at(vars(-Tile, -pH, -Time, -nb_days), ~replace_na(., 0)) %>%
  mutate(Tile = substr(Tile, 1, 7))

# Std with biomass
Functions$Tile[Functions$Tile == "tile_09" & Functions$incub_time == "Tn1"] = "tile_05"
dataset_change_cal <- Functions %>% dplyr::filter(Process == "calcifcation rate") %>% 
  rename("Time" = "incub_time", "pH" = "pH_cond") %>%
  mutate(Time = if_else(Time == "Tn1", "T0", Time),
         Time = if_else(Time == "Tn2", "T1", Time)) %>% 
  left_join(Biomass_his %>% dplyr::select(-c(Biomass_overall, Biomass_npp)),
            by = c("Tile", "pH", "Time")) %>% mutate(avg_output = avg_output / Biomass_cal) %>% select(-Biomass_cal)
dataset_change_npp <- Functions %>% dplyr::filter(Process %in% c("gross photosynthesis rate", "net photosynthesis rate")) %>% 
  rename("Time" = "incub_time", "pH" = "pH_cond") %>%
  mutate(Time = if_else(Time == "Tn1", "T0", Time),
         Time = if_else(Time == "Tn2", "T1", Time)) %>% 
  left_join(Biomass_his %>% dplyr::select(-c(Biomass_overall, Biomass_cal)),
            by = c("Tile", "pH", "Time")) %>% mutate(avg_output = avg_output / Biomass_npp) %>% select(-Biomass_npp)
dataset_change_tot <- Functions %>% dplyr::filter(Process == c("dark respiration rate")) %>% 
  rename("Time" = "incub_time", "pH" = "pH_cond") %>%
  mutate(Time = if_else(Time == "Tn1", "T0", Time),
         Time = if_else(Time == "Tn2", "T1", Time)) %>% 
  left_join(Biomass_his %>% dplyr::select(-c(Biomass_npp, Biomass_cal)),
            by = c("Tile", "pH", "Time")) %>% mutate(avg_output = avg_output / Biomass_overall) %>% select(-Biomass_overall)

##### PAR CORRECTION ----
### Need to correct by the PAR!
ELO_PAR        <- 3.13*(1-1.61*exp(-0.07*seq(0,600,1)))
scaled_ELO_PAR <- data.frame(PAR_intensity = seq(0,600,1), correction = 
                               ((ELO_PAR - min(ELO_PAR)) / (max(ELO_PAR) - min(ELO_PAR))) * 100)
LOW_PAR        <- 6.94*(1-1.32*exp(-0.07*seq(0,600,1)))
scaled_LOW_PAR <- data.frame(PAR_intensity = seq(0,600,1), correction = 
                               ((LOW_PAR - min(LOW_PAR)) / (max(LOW_PAR) - min(LOW_PAR))) * 100)
AMB_PAR        <- 5.13*(1-1.51*exp(-0.08*seq(0,600,1)))
scaled_AMB_PAR <- data.frame(PAR_intensity = seq(0,600,1), correction = 
                               ((AMB_PAR - min(AMB_PAR)) / (max(AMB_PAR) - min(AMB_PAR))) * 100)

## Compile the dataset
dataset_change      <- rbind(dataset_change_cal, dataset_change_npp, dataset_change_tot) %>% arrange(Tile) %>% 
  select(Tile, pH, Time, nb_days, Process, avg_output, sd) %>% left_join(PAR_tiles, by = c("Tile", "pH", "Time"))
# ELO correction by PAR
dataset_change_ELO_corrected <- dataset_change %>% 
  dplyr::filter(pH == "ELOW", Process %in% c("calcifcation rate", "gross photosynthesis rate",
                                             "net photosynthesis rate", "dark respiration rate")) %>% 
  mutate(PAR_intensity = round(PAR_intensity, 0)) %>% 
  left_join(scaled_ELO_PAR) %>% mutate(avg_output = (avg_output / correction) * 100) %>% select(-correction)
dataset_change_ELO <- rbind(dataset_change_ELO_corrected, dataset_change %>% 
                              dplyr::filter(pH == "ELOW", Process %notin% 
                                              c("calcifcation rate", "gross photosynthesis rate",
                                                "net photosynthesis rate", "dark respiration rate")))
# LOW correction by PAR
dataset_change_LOW_corrected <- dataset_change %>% 
  dplyr::filter(pH == "LOW", Process %in% c("calcifcation rate", "gross photosynthesis rate",
                                            "net photosynthesis rate", "dark respiration rate")) %>% 
  mutate(PAR_intensity = round(PAR_intensity, 0)) %>% 
  left_join(scaled_LOW_PAR) %>% mutate(avg_output = (avg_output / correction) * 100) %>% select(-correction)
dataset_change_LOW <- rbind(dataset_change_LOW_corrected, dataset_change %>% 
                              dplyr::filter(pH == "LOW", Process %notin% 
                                              c("calcifcation rate", "gross photosynthesis rate",
                                                "net photosynthesis rate", "dark respiration rate")))
# AMB correction by PAR
dataset_change_AMB_corrected <- dataset_change %>% 
  dplyr::filter(pH == "AMB", Process %in% c("calcifcation rate", "gross photosynthesis rate",
                                            "net photosynthesis rate", "dark respiration rate")) %>% 
  mutate(PAR_intensity = round(PAR_intensity, 0)) %>% 
  left_join(scaled_AMB_PAR) %>% mutate(avg_output = (avg_output / correction) * 100) %>% select(-correction)
dataset_change_AMB <- rbind(dataset_change_AMB_corrected, dataset_change %>% 
                              dplyr::filter(pH == "AMB", Process %notin% 
                                              c("calcifcation rate", "gross photosynthesis rate",
                                                "net photosynthesis rate", "dark respiration rate")))

# Dataset change
dataset_change  <- rbind(dataset_change_ELO, dataset_change_LOW, dataset_change_AMB)

##### Calcification from ELOW ----
# No calcifying organisms 
dataset_change$avg_output[dataset_change$Process == "calcifcation rate" & dataset_change$pH == "ELOW"] = 0
dataset_change$avg_output[dataset_change$Process == "calcifcation rate" & dataset_change$pH == "ELOW"] = 0

##### RATIO WITH T0 ----
dataset_change_init <- dataset_change %>% dplyr::filter(Time == "T0") %>% 
  rename(output_init = avg_output, Time_init = Time) %>% 
  select(-c(nb_days, Time_init, PAR_intensity, sd))
dataset_change      <- dataset_change %>% left_join(dataset_change_init, by = c("Tile", "pH", "Process")) %>% 
  mutate(change_std = avg_output / output_init) 
dataset_change$change_std[dataset_change$change_std == "NaN"] = 1

dataset_change_tot = dataset_change %>% group_by(pH, Process, Time, nb_days) %>% 
  summarise(change_std_avg = mean(change_std), change_std_sd = sd(change_std))

dataset_change = dataset_change %>% mutate(pH = fct_relevel(pH, c("ELOW", "LOW", "AMB")))
dataset_change_viz = dataset_change
dataset_change_viz$change_std[dataset_change_viz$change_std > 5] = 5
dataset_change_viz$change_std[dataset_change_viz$change_std < -5] = -5
dataset_change_viz %>%  
  ggplot(aes(x = nb_days, y = change_std, fill = Process, group = nb_days)) + 
  geom_boxplot(outliers = FALSE) + 
  geom_jitter(color = "black", shape = 21, alpha = .7) +
  facet_grid(pH~Process)

### Communities
dataset_change$Tile[dataset_change$Tile == "tile_05" & dataset_change$pH == "AMB"] = "tile_06"
dataset_change$Tile[dataset_change$Tile == "tile_07" & dataset_change$pH == "AMB"] = "tile_04"
dataset_change$Tile[dataset_change$Tile == "tile_08" & dataset_change$pH == "AMB"] = "tile_05"

dataset_change$Comm[dataset_change$Tile == "tile_06" & dataset_change$pH == "AMB"] = "Encrusting"
dataset_change$Comm[dataset_change$Tile %in% c("tile_01", "tile_03", "tile_04", "tile_05") & 
                      dataset_change$pH == "AMB"] = "Mixed"
dataset_change$Comm[dataset_change$Tile %in% c("tile_01", "tile_02", "tile_05") & 
                      dataset_change$pH == "LOW"] = "Mixed"
dataset_change$Comm[is.na(dataset_change$Comm)] = "Forest"

##### Model functions
training_data <- expand.grid(nb_days = seq(0,100,.05),
                             Comm = c("Mixed", "Forest", "Encrusting"), 
                             pH = c("ELOW", "LOW", "AMB")) %>% 
  full_join(data.frame(pH_value = c(6.438064, 7.6959125, 8.0215523), 
                       pH = c("ELOW", "LOW", "AMB")) %>% 
              mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))))

dataset_change = dataset_change %>% 
  full_join(data.frame(pH_value = c(6.438064, 7.6959125, 8.0215523), 
                       pH = c("ELOW", "LOW", "AMB")) %>% 
              mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))))

# calcification
dataset_change_CR = dataset_change %>% dplyr::filter(Process == "calcifcation rate") %>% 
  mutate(change_std = abs(change_std) + 1e-26) %>% dplyr::filter(change_std < 10)
CR_model  <- brm(change_std ~ (nb_days + 0 | Comm) + (nb_days + 0 | pH_value) + 0, init = "0",
                 data = dataset_change_CR, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(CR_model) # R2 = 77%
training_data_CR = cbind(training_data, predict(CR_model, training_data))
# change back the sign
(CR_plot = ggplot(training_data_CR, aes(y = Estimate, x = nb_days, color = pH, shape = Comm)) + 
    geom_point()+ scale_y_continuous(limits = c(-3,3)))

# Dark respiration
dataset_change_DR = dataset_change %>% dplyr::filter(Process == "dark respiration rate") %>% 
  dplyr::filter(change_std > 0)
DR_model  <- brm(change_std ~ (nb_days + 0 | Comm) + (nb_days + 0 | pH_value) + 0, init = "0",
                 data = dataset_change_DR, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(DR_model) # R2 = 39%
training_data_DR = cbind(training_data, predict(DR_model, training_data))
(DR_plot = ggplot(training_data_DR, aes(y = Estimate, x = nb_days, color = pH, shape = Comm)) + 
    geom_point()+ scale_y_continuous(limits = c(0,2)))

# GPP
dataset_change_GPP = dataset_change %>% dplyr::filter(Process == "gross photosynthesis rate") %>% 
  mutate(change_std = abs(change_std) + 1e-26) %>% dplyr::filter(change_std < 10)
GPP_model <- brm(change_std ~ (nb_days + 0 | Comm) + (nb_days + 0 | pH_value) +0, init = "0",
                 data = dataset_change_GPP, family = weibull(), cores = 4, chains = 4, iter = 10000,
                 warmup = 2000, control = list(adapt_delta = 0.95, max_treedepth = 10))
bayes_R2(GPP_model) # R2 = 47%
training_data_GPP = cbind(training_data, predict(GPP_model, training_data))
(GPP_plot = ggplot(training_data_GPP, aes(y = Estimate, x = nb_days, color = pH, shape = Comm)) + 
    geom_point() + scale_y_continuous(limits = c(0,5)))

### Define the Modeled value

######### Dark respiration rate ----
training_data_CR_values = training_data_CR %>% dplyr::filter(nb_days == 100)

(T0 = dataset_change %>% dplyr::filter(Process == "calcifcation rate") %>% dplyr::filter(Time == "T0") %>% drop_na() %>% 
    group_by(pH, Comm) %>% 
    summarise(mean_T0 = mean(avg_output)))
training_data_CR_values =
  training_data_CR_values %>% dplyr::filter(Comm %notin% c("Encrusting", "Mixed") | pH != "ELOW",
                                            Comm != "Encrusting" | pH != "LOW")

# Estimate
training_data_CR_values$Estimate[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Encrusting"] = 
  training_data_CR_values$Estimate[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_CR_values$Estimate[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Forest"] = 
  training_data_CR_values$Estimate[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_CR_values$Estimate[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Mixed"] = 
  training_data_CR_values$Estimate[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_CR_values$Estimate[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Forest"] = 
  training_data_CR_values$Estimate[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_CR_values$Estimate[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Mixed"] = 
  training_data_CR_values$Estimate[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_CR_values$Estimate[training_data_CR_values$pH == "ELOW" & training_data_CR_values$Comm == "Forest"] = 
  training_data_CR_values$Estimate[training_data_CR_values$pH == "ELOW" & training_data_CR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# Est.Error
training_data_CR_values$Est.Error[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Encrusting"] = 
  training_data_CR_values$Est.Error[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_CR_values$Est.Error[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Forest"] = 
  training_data_CR_values$Est.Error[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_CR_values$Est.Error[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Mixed"] = 
  training_data_CR_values$Est.Error[training_data_CR_values$pH == "AMB" & training_data_CR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_CR_values$Est.Error[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Forest"] = 
  training_data_CR_values$Est.Error[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_CR_values$Est.Error[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Mixed"] = 
  training_data_CR_values$Est.Error[training_data_CR_values$pH == "LOW" & training_data_CR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_CR_values$Est.Error[training_data_CR_values$pH == "ELOW" & training_data_CR_values$Comm == "Forest"] = 
  training_data_CR_values$Est.Error[training_data_CR_values$pH == "ELOW" & training_data_CR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

######### Dark respiration rate ----
training_data_DR_values = training_data_DR %>% dplyr::filter(nb_days == 100)
cste = 113.2469 # from mg/L to umol/L
(T0 = dataset_change %>% dplyr::filter(Process == "dark respiration rate") %>% dplyr::filter(Time == "T0") %>% drop_na() %>% 
    group_by(pH, Comm) %>% 
    summarise(mean_T0 = mean(avg_output * cste / Sa)))
training_data_DR_values =
  training_data_DR_values %>% dplyr::filter(Comm %notin% c("Encrusting", "Mixed") | pH != "ELOW",
                                            Comm != "Encrusting" | pH != "LOW")

# Estimate
training_data_DR_values$Estimate[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Encrusting"] = 
  -training_data_DR_values$Estimate[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_DR_values$Estimate[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Forest"] = 
  -training_data_DR_values$Estimate[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_DR_values$Estimate[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Mixed"] = 
  -training_data_DR_values$Estimate[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_DR_values$Estimate[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Forest"] = 
  -training_data_DR_values$Estimate[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_DR_values$Estimate[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Mixed"] = 
  -training_data_DR_values$Estimate[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_DR_values$Estimate[training_data_DR_values$pH == "ELOW" & training_data_DR_values$Comm == "Forest"] = 
  -training_data_DR_values$Estimate[training_data_DR_values$pH == "ELOW" & training_data_DR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# Est.Error
training_data_DR_values$Est.Error[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Encrusting"] = 
  -training_data_DR_values$Est.Error[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_DR_values$Est.Error[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Forest"] = 
  -training_data_DR_values$Est.Error[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_DR_values$Est.Error[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Mixed"] = 
  -training_data_DR_values$Est.Error[training_data_DR_values$pH == "AMB" & training_data_DR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_DR_values$Est.Error[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Forest"] = 
  -training_data_DR_values$Est.Error[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_DR_values$Est.Error[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Mixed"] = 
  -training_data_DR_values$Est.Error[training_data_DR_values$pH == "LOW" & training_data_DR_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_DR_values$Est.Error[training_data_DR_values$pH == "ELOW" & training_data_DR_values$Comm == "Forest"] = 
  -training_data_DR_values$Est.Error[training_data_DR_values$pH == "ELOW" & training_data_DR_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

######### Photosynthesis rate ----
training_data_GPP_values = training_data_GPP %>% dplyr::filter(nb_days == 100)
cste = 113.2469 # from mg/L to umol/L
(T0 = dataset_change %>% dplyr::filter(Process == "gross photosynthesis rate") %>% dplyr::filter(Time == "T0") %>% drop_na() %>% 
    group_by(pH, Comm) %>% 
    summarise(mean_T0 = mean(avg_output * cste / Sa)))
training_data_GPP_values =
  training_data_GPP_values %>% dplyr::filter(Comm %notin% c("Encrusting", "Mixed") | pH != "ELOW",
                                             Comm != "Encrusting" | pH != "LOW")

# Estimate
training_data_GPP_values$Estimate[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Encrusting"] = 
  training_data_GPP_values$Estimate[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_GPP_values$Estimate[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Forest"] = 
  training_data_GPP_values$Estimate[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_GPP_values$Estimate[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Mixed"] = 
  training_data_GPP_values$Estimate[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_GPP_values$Estimate[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Forest"] = 
  training_data_GPP_values$Estimate[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_GPP_values$Estimate[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Mixed"] = 
  training_data_GPP_values$Estimate[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_GPP_values$Estimate[training_data_GPP_values$pH == "ELOW" & training_data_GPP_values$Comm == "Forest"] = 
  training_data_GPP_values$Estimate[training_data_GPP_values$pH == "ELOW" & training_data_GPP_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# Est.Error
training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Encrusting"] = 
  training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Encrusting"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Encrusting"]
training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Forest"] = 
  training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Forest"]
training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Mixed"] = 
  training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "AMB" & training_data_GPP_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "AMB" & T0$Comm == "Mixed"]
training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Forest"] = 
  training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Forest"]
training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Mixed"] = 
  training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "LOW" & training_data_GPP_values$Comm == "Mixed"] *
  T0$mean_T0[T0$pH == "LOW" & T0$Comm == "Mixed"]
training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "ELOW" & training_data_GPP_values$Comm == "Forest"] = 
  training_data_GPP_values$Est.Error[training_data_GPP_values$pH == "ELOW" & training_data_GPP_values$Comm == "Forest"] *
  T0$mean_T0[T0$pH == "ELOW" & T0$Comm == "Forest"]

# datasets
CR_hist = data.frame(pH         = training_data_CR_values$pH,
                     Comm       = training_data_CR_values$Comm,
                     Estimate   = training_data_CR_values$Estimate,
                     Est.Error  = training_data_CR_values$Est.Error,
                     ribbon_neg = training_data_CR_values$Estimate - training_data_CR_values$Est.Error,
                     ribbon_pos = training_data_CR_values$Estimate + training_data_CR_values$Est.Error)

DR_hist = data.frame(pH         = training_data_DR_values$pH,
                     Comm       = training_data_DR_values$Comm,
                     Estimate   = training_data_DR_values$Estimate,
                     Est.Error  = training_data_DR_values$Est.Error,
                     ribbon_neg = training_data_DR_values$Estimate - training_data_DR_values$Est.Error,
                     ribbon_pos = training_data_DR_values$Estimate + training_data_DR_values$Est.Error)

GPP_hist = data.frame(pH         = training_data_GPP_values$pH,
                      Comm       = training_data_GPP_values$Comm,
                      Estimate   = training_data_GPP_values$Estimate,
                      Est.Error  = training_data_GPP_values$Est.Error,
                      ribbon_neg = training_data_GPP_values$Estimate - training_data_GPP_values$Est.Error,
                      ribbon_pos = training_data_GPP_values$Estimate + training_data_GPP_values$Est.Error)


##### Nutrients uptakes final
Processes_hist <- rbind(CR_hist, DR_hist, GPP_hist) %>% mutate(Nut = c(rep("CR", 6), rep("DR", 6), rep("GPP", 6))) # Unit = umol/g/h
Processes_hist <- read_excel("Outputs/Summary/Processes_historic.xlsx")

#######################################################################################################################
#######################################################################################################################
###########################
###### ---- VIZ ---- ######
###########################
#######################################################################################################################
#######################################################################################################################

Historic_Change_Final <- rbind(Processes_hist, Nut_uptakes) %>% data.frame()
# Scaling
Historic_Change_Final$Estimate_scaled[Historic_Change_Final$Fct == "CR"] = 
  Historic_Change_Final$Estimate[Historic_Change_Final$Fct == "CR"] / 
  min(Historic_Change_Final$Estimate[Historic_Change_Final$Fct == "CR" & Historic_Change_Final$pH != "ELOW"]) * 4
Historic_Change_Final$Estimate_scaled[Historic_Change_Final$Fct %in% c("GPP", "DR")] = 
  Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("GPP", "DR")] / 
  min(Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("GPP", "DR")]) * 2
Historic_Change_Final$Estimate_scaled[Historic_Change_Final$Fct %in% c("NH4", "NO3", "PO4")] = 
  Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("NH4", "NO3", "PO4")] / 
  max(Historic_Change_Final$Estimate[Historic_Change_Final$Fct %in% c("NH4", "NO3", "PO4")]) / 100

#Sqrt
Historic_Change_Final$Estimate_scaled_log = abs(log(Historic_Change_Final$Estimate_scaled + 1)) 

# Nutrients upscale
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH != "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH != "ELOW"] * 15
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH == "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("PO4") & Historic_Change_Final$pH == "ELOW"] * 1.5
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NO3") & Historic_Change_Final$pH != "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NO3") & Historic_Change_Final$pH != "ELOW"] * 5
Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NH4") & Historic_Change_Final$pH != "ELOW"] =
  Historic_Change_Final$Estimate_scaled_log[Historic_Change_Final$Fct %in% c("NH4") & Historic_Change_Final$pH != "ELOW"] * 2

# Colors
Historic_Change_Final$Color = c("#ffffff", "#ba947b", "#cbae9b", "#90664b", "#704f3a", "#90664b",
                                "#314f22", "#a4cf8f", "#a4cf8f", "#a4cf8f", "#88c06d", "#a4cf8f",
                                "#3f662c", "#497733", "#497733", "#629f44", "#6cb04c", "#629f44",
                                "#563d98", "#8c679c", "#775587", "#bf9ece", "#bf9ece", "#9d7bae",
                                "#563d98", "#8c679c", "#775587", "#8c679c", "#bf9ece", "#844da3",
                                "#563d98", "#9d7bae", "#8c679c", "#9d7bae", "#e0c5f0", "#775587")

FOR_AMB_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         Fct = fct_relevel(Fct, c("GPP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Forest", pH == "AMB") %>% 
  mutate(ID = seq(1,6,1))
(FOR_AMB_HIS_plot = FOR_AMB_HIS_data %>% 
  ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
  coord_polar() +
  geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
  scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
  scale_x_discrete(name = "") +
  scale_fill_manual(values = FOR_AMB_HIS_data$Color) +
  theme_ambient(panel_background_color = "white") +
  theme(axis.text        = element_text(size = 14),
        axis.title       = element_text(size = 16),
        legend.text      = element_text(size = 14),
        axis.ticks       = element_blank(),
        axis.text.x      = element_blank()))

MIX_AMB_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         Fct = fct_relevel(Fct, c("GPP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Mixed", pH == "AMB") %>% 
  mutate(ID = seq(1,6,1))
(MIX_AMB_HIS_plot = MIX_AMB_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = MIX_AMB_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

ENC_AMB_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         Fct = fct_relevel(Fct, c("GPP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Encrusting", pH == "AMB") %>% 
  mutate(ID = seq(1,6,1))
(ENC_AMB_HIS_plot = ENC_AMB_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = ENC_AMB_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

FOR_LOW_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         Fct = fct_relevel(Fct, c("GPP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Forest", pH == "LOW") %>% 
  mutate(ID = seq(1,6,1))
(FOR_LOW_HIS_plot = FOR_LOW_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = FOR_LOW_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

MIX_LOW_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         Fct = fct_relevel(Fct, c("GPP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Mixed", pH == "LOW") %>% 
  mutate(ID = seq(1,6,1))
(MIX_LOW_HIS_plot = MIX_LOW_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = MIX_LOW_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

FOR_ELOW_HIS_data = Historic_Change_Final %>% 
  mutate(pH = as.factor(pH), pH = fct_relevel(pH, c("AMB", "LOW", "ELOW")),
         Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting")),
         Fct = fct_relevel(Fct, c("GPP", "DR", "NH4", "NO3", "PO4", "CR"))) %>%  
  dplyr::filter(Comm == "Forest", pH == "ELOW") %>% 
  mutate(ID = seq(1,6,1))
(FOR_ELOW_HIS_plot = FOR_ELOW_HIS_data %>% 
    ggplot(aes(x = Fct, y = Estimate_scaled_log, fill = as.factor(ID))) + 
    coord_polar() +
    geom_col(position = "dodge", color = "black", width = 1, show.legend = F) + 
    scale_y_continuous(name = "", breaks = seq(0,4,1), limits = c(0,4), labels = rep("", 5)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = FOR_ELOW_HIS_data$Color) +
    theme_ambient(panel_background_color = "white") +
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          axis.ticks       = element_blank(),
          axis.text.x      = element_blank()))

Historic_Final <- FOR_AMB_HIS_plot + MIX_AMB_HIS_plot+ ENC_AMB_HIS_plot + 
  FOR_LOW_HIS_plot + MIX_LOW_HIS_plot + plot_spacer() +
  FOR_ELOW_HIS_plot + plot_spacer() + plot_spacer()

ggsave(Historic_Final, file = "Outputs/Figures/Historic/Historic_Final.png", width = 20, 
       height = 20, units = "cm", dpi = 300)
