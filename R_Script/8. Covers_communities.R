options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)
`%notin%` = Negate(`%in%`)

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets("Data/7. Covers/20230725_tiles_species_visual_census_25subquadrats.xlsx")

# With corrected names
corrected_names <- read_excel("Data/7. Covers/corrected_names.xlsx") 

# Read each sheet into a separate dataset and rename it
datasets <- lapply(sheet_names, function(sheet) {
  read_excel("Data/7. Covers/20230725_tiles_species_visual_census_25subquadrats.xlsx", sheet = sheet)})

# Work with different communities
mixtes = c("tile_03", "tile_04", "tile_05", "tile_06", "tile_08", "tile_29") # Mixt assemblages
forest = c("tile_07", "tile_09", "tile_10", "tile_11", "tile_13", "tile_14") # Forest assemblages
encrus = c("tile_01", "tile_02", "tile_12", "tile_18", "tile_19", "tile_28") # Encrusting assemblages

# Define themes
theme_extreme_low <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
      panel.background = element_rect(fill = panel_background_color),
      plot.title = element_text(size = 18, color = "firebrick1", face = "bold"),
      panel.grid = element_line(colour = NA),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16, vjust = -3),
      axis.title.y = element_text(size = 16, vjust = 3),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      strip.text = element_text(size = 16),
      strip.background = element_rect(colour = "black", fill = "firebrick1"),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"))}
theme_ambient <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "cornflowerblue", face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_text(size = 16, vjust = -1),
        axis.title.y = element_text(size = 16, vjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "cornflowerblue"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}
theme_low <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "gold", face = "bold"),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "gold"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}

# Define the taxonomic group
table(corrected_names$taxonomy)

# Total cover for each tile Light
Tile_cover_T0 = vector("list", 18) ; Tile_cover_T1 = vector("list", 18) 
Tile_cover_T2 = vector("list", 18) ; Tile_cover_T3 = vector("list", 18) ; Tile_cover = vector("list", 18)
for (i in 1:18) {
  Tile_cover_T0[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t0_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T0" = ".") %>% mutate(T0 = ((T0/45*100) / sum(T0/45*100)) * 100)
  Tile_cover_T1[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t1_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T1" = ".") %>% mutate(T1 = ((T1/45*100) / sum(T1/45*100)) * 100)
  Tile_cover_T2[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t2_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T2" = ".") %>% mutate(T2 = ((T2/45*100) / sum(T2/45*100)) * 100)
  Tile_cover_T3[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t3_"), -matches("_back_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T3" = ".") %>% mutate(T3 = ((T3/45*100) / sum(T3/45*100)) * 100)
  Tile_cover[[i]] <- bind_cols(Tile_cover_T0[[i]], Tile_cover_T1[[i]], Tile_cover_T2[[i]], Tile_cover_T3[[i]]) %>%
    data.frame() %>% filter(T0 != 0 | T1 != 0 | T2 != 0 | T3 != 0) %>% rownames_to_column(., var = "Species")
  Tile_cover[[i]] <- data.frame(Species = rep(Tile_cover[[i]]$Species, 4),
                                Tile    = rep(sheet_names[i], length(Tile_cover[[i]]$Species)*4),
                                Time    = c(rep("T0", length(Tile_cover[[i]]$Species)), rep("T1", length(Tile_cover[[i]]$Species)), 
                                            rep("T2", length(Tile_cover[[i]]$Species)), rep("T3", length(Tile_cover[[i]]$Species))),
                                Cover   = c(Tile_cover[[i]]$T0, Tile_cover[[i]]$T1, Tile_cover[[i]]$T2, Tile_cover[[i]]$T3)) }
Tile_cover = bind_rows(Tile_cover)

# Total number of species observed
length(unique(Tile_cover$Species))

# We will add a new information with the Zone
Zone_pH    <- data_frame(Tile = sheet_names, pH = c(rep("ELOW", 6), rep("LOW", 6), rep("AMB", 6)))
Tile_cover <- Tile_cover %>% left_join(Zone_pH) %>% left_join(corrected_names) %>% select(-Species) %>% rename(Species = species_new)

# Total number of species within a functional group
data_col = cbind(Tile_cover %>% group_by(functional.group, Species) %>% summarise(n = n()), Color = 
                   rep(NA, 40))
Tile_cover <- Tile_cover %>% left_join(data_col)
Tile_cover$Species[Tile_cover$Species %in% c("Schizobrachiella sanguinea dead", "Perforatus perforatus dead", "tile")] = "non-alive"
Tile_cover$taxonomy[Tile_cover$Species == "non-alive"] = "non-alive"
data_col$Species[data_col$Species %in% c("Schizobrachiella sanguinea dead", "Perforatus perforatus dead", "tile")] = "non-alive"
data_col$functional.group[data_col$Species == "non-alive"] = "non-alive"
data_col = data_col %>% distinct(functional.group, Species, Color)
Tile_cover$Species <- factor(Tile_cover$Species, levels = data_col$Species)
Tile_cover$pH <- factor(Tile_cover$pH, levels = c("ELOW", "LOW", "AMB"))

# Species_order
taxonomy_order <- c("non-alive", "Turf", "Chlorophyta", "Phaeophyceae", "Rhodophyta", "Porifera", "Cnidaria", "Bryozoa",
                    "Polychaeta", "Crustacea", "Mollusca", "Echinodermata", "Tunicates")

Species_order <- c("non-alive", "Turf", "Acetabularia acetabulum", "Anadyomene stellata", "Bryopsis sp.", "Valonia utricularis", 
                   "Cladophora sp.", "Parvocaulis parvulus", "Flabellia petiolata", "Dictyota sp.", "Pseudolithoderma adriaticum", 
                   "Padina pavonica", "Halopteris scoparia", "Halopteris filicina", "CCA", "Hydroliton farinosum", "Jania rubens", 
                   "Corallina officinalis", "Peyssonnelia squamaria", "Hildenbrandia crouaniorum", "Crambe crambe", 
                   "Phorbas topsenti", "Cacospongia mollior", "Clathrina clathrus", "Leucandra gossei", "Terpios fugax", 
                   "Oscarella lobularis", "Merlia sp.", "Corticium candelabrum", "Haliclona sp.", "Schizoporella dunkeri", 
                   "Schizobrachiella sanguinea", "Celleporina caminata", "Patinella radiata", "Reptadeonella violacea", 
                   "Reteporella grimaldii", "Encrusting bryozoan", "Bugula neritina", "Puellina radiata", "Serpulids", 
                   "Perforatus perforatus", "Ostrea sp.", "Lima lima", "Cystodytes dellechiajei", "Diplosoma spongiforme", 
                   "Didemnum cf. coriaceum", "Didemnum maculosum", "Didemnum sp.", "Botryllus sp.", "Ascidia conchilega")

color_order   <- c("#ffffff", "#cae4b9", "#95c973", "#72b744", "#4fa516", "#669f41", "#418812", "#37730f", "#1f4008", "#dc9428", 
                   "#a46400", "#9a6618", "#7f4900", "#5b3c0e", "#ff88d7", "#e47fba", "#d94c9f", "#dc0092", "#CC0066", "#a30000", 
                   "#ffcc99", "#ffc07f", "#ffb366", "#ffac47", "#ffa732", "#ff9b32", "#ff9c19", "#ff9200", "#ff8e1a", "#ff8200", 
                   "#ffffcc", "#ffff99", "#f9e342", "#ffd655", "#f1c232", "#ffcc00", "#efb116", "#d8ae2d", "#c09b28", "#3d85c6", 
                   "#a2c4c9", "#76a5af", "#819ca0", "#d1c9e4", "#b3a6d3", "#9483c1", "#8571b8", "#674ea7", "#523e85", "#483674")

data_col = data.frame(Species = Species_order, Color = color_order)
data_col$Species   <- factor(data_col$Species, levels = Species_order)
Tile_cover$Species <- factor(Tile_cover$Species, levels = Species_order)

## Ambient front
# Mixed
AMB_merged_mix <- Tile_cover %>% dplyr::filter(pH == "AMB", Tile %in% mixtes) %>% 
  group_by(Time, Species, taxonomy, pH)  %>% summarise(Cover = sum(Cover) / 2) 
AMB_merged_mix_plot <- AMB_merged_mix %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(AMB_merged_mix$Species)]) +
  ggtitle("Ambient – Mixt communities") + theme_ambient(panel_background_color = "white")
# Forest
AMB_merged_for <- Tile_cover %>% dplyr::filter(pH == "AMB", Tile %in% forest) %>% 
  group_by(Time, Species, taxonomy, pH)  %>% summarise(Cover = sum(Cover) / 2)
AMB_merged_for_plot <- AMB_merged_for %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(AMB_merged_for$Species)]) +
  ggtitle("Ambient – Forest communities") + theme_ambient(panel_background_color = "white")
# Encrusting
AMB_merged_enc <- Tile_cover %>% dplyr::filter(pH == "AMB", Tile %in% encrus) %>% 
  group_by(Time, Species, taxonomy, pH)  %>% summarise(Cover = sum(Cover) / 2) 
AMB_merged_enc_plot <- AMB_merged_enc %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(AMB_merged_enc$Species)]) +
  ggtitle("Ambient – Encrusting communities") + theme_ambient(panel_background_color = "white")

## Low front
# Mixed
LOW_merged_mix <- Tile_cover %>% dplyr::filter(pH == "LOW", Tile %in% mixtes) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
LOW_merged_mix_plot <- LOW_merged_mix %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(LOW_merged_mix$Species)]) +
  ggtitle("Low – Mixt communities") + theme_low(panel_background_color = "white")
# Forest
LOW_merged_for <- Tile_cover %>% dplyr::filter(pH == "LOW", Tile %in% forest) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
LOW_merged_for_plot <- LOW_merged_for %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(LOW_merged_for$Species)]) +
  ggtitle("Low – Forest communities") + theme_low(panel_background_color = "white")
# Encrusting
LOW_merged_enc <- Tile_cover %>% dplyr::filter(pH == "LOW", Tile %in% encrus) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
LOW_merged_enc_plot <- LOW_merged_enc %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(LOW_merged_enc$Species)]) +
  ggtitle("Low – Encrusting communities") + theme_low(panel_background_color = "white")

## Extreme low front
# Mixed
ELO_merged_mix  <- Tile_cover %>% dplyr::filter(pH == "ELOW", Tile %in% mixtes) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
ELO_merged_mix_plot <- ELO_merged_mix %>% 
    ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
    geom_col(position = "stack", color = "black", show.legend = F) +
    labs(x = "Incubation time", y = "Cover (%)", color = "Species") + theme_classic() +
    scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(ELO_merged_mix$Species)]) +
    ggtitle("Extreme Low – Mixt communities") + theme_extreme_low(panel_background_color = "white")
# Forest
ELO_merged_for  <- Tile_cover %>% dplyr::filter(pH == "ELOW", Tile %in% forest) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
ELO_merged_for_plot <- ELO_merged_for %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(ELO_merged_for$Species)]) +
  ggtitle("Extreme Low – Forest communities") + theme_extreme_low(panel_background_color = "white")
# Encrusting
ELO_merged_enc  <- Tile_cover %>% dplyr::filter(pH == "ELOW", Tile %in% encrus) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
ELO_merged_enc_plot <- ELO_merged_enc %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(ELO_merged_enc$Species)]) +
  ggtitle("Extreme Low – Encrusting communities") + theme_extreme_low(panel_background_color = "white")

### Now only with back tiles
# Total cover for each tile Back
Tile_cover_T0 = vector("list", 18) ; Tile_cover_T1 = vector("list", 18) 
Tile_cover_T2 = vector("list", 18) ; Tile_cover_T3 = vector("list", 18) ; Tile_cover = vector("list", 18)
for (i in 1:18) {
  Tile_cover_T0[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_back_t0_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T0" = ".") %>% mutate(T0 = ((T0/25*100) / sum(T0/25*100)) * 100)
  Tile_cover_T1[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_back_t1_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T1" = ".") %>% mutate(T1 = ((T1/25*100) / sum(T1/25*100)) * 100)
  Tile_cover_T2[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_back_t2_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T2" = ".") %>% mutate(T2 = ((T2/25*100) / sum(T2/25*100)) * 100)
  Tile_cover_T3[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_back_t3_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T3" = ".") %>% mutate(T3 = ((T3/25*100) / sum(T3/25*100)) * 100)
  Tile_cover[[i]] <- bind_cols(Tile_cover_T0[[i]], Tile_cover_T1[[i]], Tile_cover_T2[[i]], Tile_cover_T3[[i]]) %>%
    data.frame() %>% filter(T0 != 0 | T1 != 0 | T2 != 0 | T3 != 0) %>% rownames_to_column(., var = "Species")
  Tile_cover[[i]] <- data.frame(Species = rep(Tile_cover[[i]]$Species, 4),
                                Tile    = rep(sheet_names[i], length(Tile_cover[[i]]$Species)*4),
                                Time    = c(rep("T0", length(Tile_cover[[i]]$Species)), rep("T1", length(Tile_cover[[i]]$Species)), 
                                            rep("T2", length(Tile_cover[[i]]$Species)), rep("T3", length(Tile_cover[[i]]$Species))),
                                Cover   = c(Tile_cover[[i]]$T0, Tile_cover[[i]]$T1, Tile_cover[[i]]$T2, Tile_cover[[i]]$T3)) }
Tile_cover = bind_rows(Tile_cover) %>% left_join(Zone_pH) %>% left_join(corrected_names) %>% 
  select(-Species) %>% rename(Species = species_new) %>% left_join(Zone_pH) 

# Total number of species within a functional group
Tile_cover <- Tile_cover %>% left_join(data_col)
Tile_cover$Species[grepl("dead|tile", Tile_cover$Species)] <- "non-alive"
Tile_cover$taxonomy[Tile_cover$Species == "non-alive"] <- "non-alive"
data_col$Species[grepl("dead|tile", data_col$Species)] <- "non-alive"
data_col$functional.group[data_col$Species == "non-alive"] = "non-alive"
data_col = data_col %>% distinct(functional.group, Species, Color)
Tile_cover$pH <- factor(Tile_cover$pH, levels = c("ELOW", "LOW", "AMB"))
Tile_cover$Species <- factor(Tile_cover$Species, levels = Species_order)

## Ambient back
# Mixed
AMB_merged_mix  <- Tile_cover %>% dplyr::filter(pH == "AMB", Tile %in% mixtes) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
AMB_merged_mix_plot_2 <- AMB_merged_mix %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(AMB_merged_mix$Species)]) +
  ggtitle("Ambient – Mixt communities") + theme_ambient(panel_background_color = "gray20")
# Forest
AMB_merged_for  <- Tile_cover %>% dplyr::filter(pH == "AMB", Tile %in% forest) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
AMB_merged_for_plot_2 <- AMB_merged_for %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(AMB_merged_for$Species)]) +
  ggtitle("Ambient – Forest communities") + theme_ambient(panel_background_color = "gray20")
# Encrusting
AMB_merged_enc  <- Tile_cover %>% dplyr::filter(pH == "AMB", Tile %in% encrus) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
AMB_merged_enc_plot_2 <- AMB_merged_enc %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(AMB_merged_enc$Species)]) +
  ggtitle("Ambient – Encrusting communities") + theme_ambient(panel_background_color = "gray20")


## Low back
# Mixed
LOW_merged_mix  <- Tile_cover %>% dplyr::filter(pH == "LOW", Tile %in% mixtes) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
LOW_merged_mix_plot_2 <- LOW_merged_mix %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(LOW_merged_mix$Species)]) +
  ggtitle("Low – Mixt communities") + theme_low(panel_background_color = "gray20")
# Forest
LOW_merged_for  <- Tile_cover %>% dplyr::filter(pH == "LOW", Tile %in% forest) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
LOW_merged_for_plot_2 <- LOW_merged_for %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(LOW_merged_for$Species)]) +
  ggtitle("Low – Forest communities") + theme_low(panel_background_color = "gray20")
# Encrusting
LOW_merged_enc  <- Tile_cover %>% dplyr::filter(pH == "LOW", Tile %in% encrus) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
LOW_merged_enc_plot_2 <- LOW_merged_enc %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(LOW_merged_enc$Species)]) +
  ggtitle("Low – Encrusting communities") + theme_low(panel_background_color = "gray20")

## Extreme low back
# Mixed
ELO_merged_mix  <- Tile_cover %>% dplyr::filter(pH == "ELOW", Tile %in% mixtes) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
ELO_merged_mix_plot_2 <- ELO_merged_mix %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(ELO_merged_mix$Species)]) +
  ggtitle("Extreme Low – Mixt communities") + theme_extreme_low(panel_background_color = "gray20")
# Forest
ELO_merged_for  <- Tile_cover %>% dplyr::filter(pH == "ELOW", Tile %in% forest) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
ELO_merged_for_plot_2 <- ELO_merged_for %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(ELO_merged_for$Species)]) +
  ggtitle("Extreme Low – Forest communities") + theme_extreme_low(panel_background_color = "gray20")
# Encrusting
ELO_merged_enc  <- Tile_cover %>% dplyr::filter(pH == "ELOW", Tile %in% encrus) %>% 
  group_by(Time, Species, taxonomy, pH) %>% summarise(Cover = sum(Cover) / 2)
ELO_merged_enc_plot_2 <- ELO_merged_enc %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(ELO_merged_enc$Species)]) +
  ggtitle("Extreme Low – Encrusting communities") + theme_extreme_low(panel_background_color = "gray20")

### Let's plot everything
(Total_com <- (AMB_merged_for_plot + AMB_merged_mix_plot + AMB_merged_enc_plot) /
    (AMB_merged_for_plot_2 + AMB_merged_mix_plot_2 + AMB_merged_enc_plot_2) /
    (LOW_merged_for_plot + LOW_merged_mix_plot + LOW_merged_enc_plot) /
    (LOW_merged_for_plot_2 + LOW_merged_mix_plot_2 + LOW_merged_enc_plot_2) /
    (ELO_merged_for_plot + ELO_merged_mix_plot + ELO_merged_enc_plot) /
    (ELO_merged_for_plot_2 + ELO_merged_mix_plot_2 + ELO_merged_enc_plot_2) + plot_layout(guides = "collect", nrow = 6)) & theme(legend.position = "none")

### SImplification
Cover_simplified = 
  rbind(AMB_merged_for_plot$data, AMB_merged_mix_plot$data, AMB_merged_enc_plot$data,
        LOW_merged_for_plot$data, LOW_merged_mix_plot$data, LOW_merged_enc_plot$data,
        ELO_merged_for_plot$data, ELO_merged_mix_plot$data, ELO_merged_enc_plot$data,
        AMB_merged_for_plot_2$data, AMB_merged_mix_plot_2$data, AMB_merged_enc_plot_2$data,
        LOW_merged_for_plot_2$data, LOW_merged_mix_plot_2$data, LOW_merged_enc_plot_2$data,
        ELO_merged_for_plot_2$data, ELO_merged_mix_plot_2$data, ELO_merged_enc_plot_2$data) %>% 
  data.frame() %>% 
  mutate(Tile_side = c(rep("Front", 816), rep("Back", 628)),
         Comm = c(rep("Forest", 88), rep("Mixed", 84), rep("Encrusting", 96),
                  rep("Forest", 88), rep("Mixed", 104), rep("Encrusting", 120),
                  rep("Forest", 76), rep("Mixed", 80), rep("Encrusting", 80),
                  rep("Forest", 72), rep("Mixed", 72), rep("Encrusting", 52),
                  rep("Forest", 80), rep("Mixed", 88), rep("Encrusting", 72),
                  rep("Forest", 72), rep("Mixed", 64), rep("Encrusting", 56))) %>% 
  select(-`.group`) %>% 
  group_by(Time, taxonomy, pH, Tile_side, Comm) %>% 
  summarise(Cover = sum(Cover))

# Colors
taxonomy_order <- c("non-alive", "Turf", "Chlorophyta", "Phaeophyceae", "Rhodophyta", "Porifera", 
                    "Bryozoa", "Polychaeta", "Crustacea", "Mollusca", "Tunicates")
color_order    <- c("#ffffff", "#cae4b9", "#72b744", "#9a6618", "#e47fba", "#ff9b32", "#ffcc00", "#3d85c6", 
                    "#a2c4c9", "#819ca0", "#8571b8")
data_col = data.frame(taxonomy = taxonomy_order, Color = color_order)
data_col$taxonomy   <- factor(data_col$taxonomy, levels = taxonomy_order)
Cover_simplified$taxonomy <- factor(Cover_simplified$taxonomy, levels = taxonomy_order)

Front_Simplified = Cover_simplified %>% dplyr::filter(Tile_side == "Front") %>% 
  mutate(Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting"))) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = taxonomy)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  facet_grid(pH ~ Comm) +
  scale_fill_manual(values = data_col$Color[data_col$taxonomy %in% unique(Cover_simplified$taxonomy)]) + 
  theme_ambient(panel_background_color = "white")

Back_Simplified = Cover_simplified %>% dplyr::filter(Tile_side == "Back") %>% 
  mutate(Comm = fct_relevel(Comm, c("Forest", "Mixed", "Encrusting"))) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = taxonomy)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") + theme_classic() +
  facet_grid(pH ~ Comm) +
  scale_fill_manual(values = data_col$Color[data_col$taxonomy %in% unique(Cover_simplified$taxonomy)]) + 
  theme_ambient()

Total_com_simplified = Front_Simplified + Back_Simplified

### Quick statistics
# Tile_cover_front = Tile_cover ### up to line 134
# Tile_cover_back = Tile_cover  ### up to line 260
# Tile_cover = rbind(Tile_cover_front %>% dplyr::select(-n), Tile_cover_back)
nb_species = Tile_cover %>% 
  mutate(Comm = case_when(Tile %in% mixtes ~ "Mixed",
                          Tile %in% encrus ~ "Encrusting",
                          TRUE ~ "Forest")) %>% 
  dplyr::filter(Species != "non-alive", Cover != 0) %>% 
  group_by(Tile, Time, pH, Comm, Species) %>% summarise(Cover = mean(Cover)) %>% 
  group_by(Tile, Time, pH, Comm) %>% summarise(n_species = n()) %>% 
  group_by(Time, pH, Comm) %>% summarise(n_species_avg = mean(n_species), n_species_sd = sd(n_species))

nb_species_amb <- nb_species %>% dplyr::filter(pH == "AMB")
nb_species_low <- nb_species %>% dplyr::filter(pH == "LOW")
nb_species_elow <- nb_species %>% dplyr::filter(pH == "ELOW")
#AMB:   20.5 ± 3.5 CALC (T0) & 24.0 ± 2.8 FLESH (T0) vs 16.5 ± 2.1 CALC (T3) & 17.5 ± 3.5 FLESH (T3)
#LOW:   22.0 ± 0.0 FLESH (T0) & 22.5 ± 0.7 CALC (T0) vs 15.5 ± 2.1 CALC (T3) & 15.5 ± 0.7 FLESH (T3)
#ELOW: 18.0 ± 0.0 CALC (T0) & 20.5 ± 3.5 FLESH (T0) vs 03.0 ± 0.0 FLESH (T3) & 04.0 ± 1.4 CALC (T3)

data.frame(
  pH = c("AMB", "AMB", "LOW", "LOW", "ELOW", "ELOW"),
  Comm = c("CALC", "FLESH", "CALC", "FLESH", "CALC", "FLESH"),
  T0_mean = c(20.5, 24.0, 22.5, 22.0, 18.0, 20.5),
  T0_sd = c(3.5, 2.8, 0.7, 0.0, 0.0, 3.5),
  T3_mean = c(16.5, 17.5, 15.5, 15.5, 4.0, 3.0),
  T3_sd = c(2.1, 3.5, 2.1, 0.7, 1.4, 0.0)) %>%
  mutate(diff_mean = T3_mean - T0_mean,
         diff_sd = sqrt(T3_sd^2 + T0_sd^2)) %>%
  select(pH, Comm, diff_mean, diff_sd)
# 4.0 ± 4.1, 7.0 ± 2.2 and 14.0 ± 1.4 (CALC) vs 6.5 ± 4.5, 6.5 ± 0.7 and 17.5 ± 3.5 (FLESH)

# Save
ggsave(Total_com, file = "Outputs/Figures/Cover/Cover_Transplants_communities.png", 
       width = 20, height = 40, units = "cm", dpi = 300)
ggsave(Total_com_simplified, file = "Outputs/Figures/Cover/Cover_Transplants_communities_taxon.png", 
       width = 30, height = 15, units = "cm", dpi = 300)