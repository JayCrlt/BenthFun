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
