rm(list = ls())
options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)
`%notin%` = Negate(`%in%`)

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets("Data/7. Covers/20230725_tiles_species_visual_census_25subquadrats.xlsx")

# With corrected names
corrected_names <- read_excel("Data/7. Covers/corrected_names.xlsx") 

# Read each sheet into a separate dataset and rename it
datasets <- lapply(sheet_names, function(sheet) {
  read_excel("Data/7. Covers/20230725_tiles_species_visual_census_25subquadrats.xlsx", sheet = sheet)})

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
data_col$Species[data_col$Species %in% c("Schizobrachiella sanguinea dead", "Perforatus perforatus dead", "tile")] = "non-alive"
data_col$functional.group[data_col$Species == "non-alive"] = "non-alive"
data_col = data_col %>% distinct(functional.group, Species, Color)
Tile_cover$Species <- factor(Tile_cover$Species, levels = data_col$Species)
Tile_cover$pH <- factor(Tile_cover$pH, levels = c("ELOW", "LOW", "AMB"))

Species_order <- c("non-alive", "Turf", "Perforatus perforatus", "Cystodytes dellechiajei", "Didemnum maculosum",
                   "Clathrina clathrus", "Patinella radiata", "Reptadeonella violacea", "Schizobrachiella sanguinea",
                   "Schizoporella dunkeri", "Botryllus sp.", "Phorbas topsenti", "Pseudolithoderma adriaticum", "Hildenbrandia crouaniorum",
                   "Peyssonnelia squamaria", "Serpulids", "Jania rubens", "CCA", "Hydroliton farinosum", "Corallina officinalis",
                   "Terpios fugax", "Cacospongia mollior", "Valonia utricularis", "Halopteris scoparia", "Halopteris filicina",
                   "Flabellia petiolata", "Dictyota sp.", "Cladophora sp.", "Bryopsis sp.", "Anadyomene stellata", "Parvocaulis parvulus",
                   "Acetabularia acetabulum", "Padina pavonica", "Leucandra gossei", "Celleporina caminata", "Reteporella grimaldii",
                   "Ostrea sp.", "Crambe crambe")
color_order   <- c("#ffffff", "#99cc99", "#cccccc", "#f2d6ae", "#e5b060", "#dc9428", "#cc8720", "#be7e1e", "#9a6618", "#855815",
                   "#5b3c0e", "#663333", "#98240a", "#d5320e", "#ef431d", "#f58f79", "#cc9999", "#ffcccc", "#ff88d7", "#dc0092",
                   "#CC0066", "#41087a", "#003333", "#142b05", "#183206", "#1f4008", "#29560b", "#37730f", "#418812", "#4fa516", 
                   "#5dc219", "#98f02c", "#ccf797", "#ffffcc", "#ffff99", "#ffcc00", "#efb116", "#ffac47")
x_order       <- rep(seq(1, 10, 1), 4)[-tail(which(rep(seq(1, 10, 1), 4) %in% c(9, 10)), 2)] * 6
y_order       <- rep(seq(-1, -4, -1), each = 10)[-tail(which(rep(seq(-1, -4, -1), each = 10) == -4), 2)] * 1

data_col = data.frame(Species = Species_order, Color = color_order, x_order, y_order)
data_col$Species   <- factor(data_col$Species, levels = Species_order)
Tile_cover$Species <- factor(Tile_cover$Species, levels = Species_order)

##### Plot
### Let's start with ELOW
Tile_cover_ELOW  <- Tile_cover %>% dplyr::filter(pH == "ELOW") %>% 
  group_by(Species, Time, Tile) %>% summarise(Cover = sum(Cover)) 
Transplants_ELOW <- Tile_cover_ELOW %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile, ncol = 6) + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(Tile_cover_ELOW$Species)]) +
  ggtitle("Extreme Low pH conditions – Front & Laterals") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
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
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

### We continue with LOW
Tile_cover_LOW  <- Tile_cover %>% dplyr::filter(pH == "LOW")
Transplants_LOW <- Tile_cover_LOW %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile, ncol = 6) + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(Tile_cover_LOW$Species)]) +
  ggtitle("Low pH conditions – Front & Laterals") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
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
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

### We finish with AMB
Tile_cover_AMB  <- Tile_cover %>% dplyr::filter(pH == "AMB")
Transplants_AMB <- Tile_cover_AMB %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile, ncol = 6) + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(Tile_cover_AMB$Species)]) +
  ggtitle("Ambient pH conditions – Front & Laterals") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        plot.title = element_text(size = 18, color = "cornflowerblue", face = "bold"),
        panel.grid = element_line(colour = NA),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "cornflowerblue"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

Legend_Light <- data_col %>% mutate(Species_lab = str_replace(Species_order, "\\s(?!sp)", "\n")) %>% 
  ggplot(aes(x = x_order, y = y_order, fill = Species, label = Species_lab)) + 
  geom_label(show.legend = F, hjust = 0,
             color = c(rep("black", 9), rep("white", 5), rep("black", 7), rep("white", 7), rep("black", 10))) +
  scale_x_continuous(name = "", labels = rep("", 6), breaks = seq(10, 60, 10), limits = c(6, 63)) +  
  scale_y_continuous(name = "", labels = rep("", 6), limits = c(-4.5, -0.5)) +
  scale_fill_manual(values = color_order) + 
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = NA),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

(Light_Cover <- Transplants_AMB + plot_spacer() + Transplants_LOW + plot_spacer() + Transplants_ELOW + 
    plot_spacer() + Legend_Light + 
    plot_layout(guides = "collect", heights = c(30, -7.5, 30, -7.5, 30, -1, 30)))
  
# Total cover for each tile Light
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
Tile_cover = bind_rows(Tile_cover) %>% left_join(Zone_pH) %>% left_join(corrected_names) %>% select(-Species) %>% rename(Species = species_new)

# Total number of species observed
length(unique(Tile_cover$Species))

# We will add a new information with the Zone
Zone_pH    <- data_frame(Tile = sheet_names, pH = c(rep("ELOW", 6), rep("LOW", 6), rep("AMB", 6)))
Tile_cover <- Tile_cover %>% left_join(Zone_pH) 

# Total number of species within a functional group
data_col = cbind(Tile_cover %>% group_by(functional.group, Species) %>% summarise(n = n()), Color = 
                   rep(NA, 39))
Tile_cover <- Tile_cover %>% left_join(data_col)
Tile_cover$Species[grepl("dead|tile", Tile_cover$Species)] <- "non-alive"
data_col$Species[grepl("dead|tile", data_col$Species)] <- "non-alive"
data_col$functional.group[data_col$Species == "non-alive"] = "non-alive"
data_col = data_col %>% distinct(functional.group, Species, Color)
Tile_cover$pH <- factor(Tile_cover$pH, levels = c("ELOW", "LOW", "AMB"))

data_col = data_col %>% left_join(Legend_Light$data, by = "Species") %>% 
  select(functional.group, Species, Color.y) %>% rename(Color = Color.y)

Species_order <- c("non-alive", "Turf", "Lima lima", "Perforatus perforatus", "Cystodytes dellechiajei", "Didemnum cf. coriaceum", 
                   "Didemnum maculosum", "Didemnum sp.", "Clathrina clathrus", "Patinella radiata", "Schizobrachiella sanguinea",
                   "Schizoporella dunkeri", "Botryllus sp.", "Phorbas topsenti", "Pseudolithoderma adriaticum", 
                   "Hildenbrandia crouaniorum", "Peyssonnelia squamaria", "Serpulids", "CCA", "Terpios fugax", "Bugula neritina", 
                   "Oscarella lobularis", "Diplosoma spongiforme", "Cacospongia mollior", "Haliclona sp.", "Valonia utricularis", 
                   "Flabellia petiolata", "Dictyota sp.", "Anadyomene stellata", "Ascidia conchilega", "Celleporina caminata", 
                   "Puellina radiata", "Merlia sp.", "Crambe crambe", "Encrusting bryozoan", "Corticium candelabrum")
color_order   <- c("#ffffff", "#99cc99", "#aaaccc", "#cccccc", "#f2d6ae", "#e5b090", "#e5b060", "#ce9e56", "#dc9428", "#cc8720", 
                   "#9a6618", "#855815", "#5b3c0e", "#663333", "#98240a", "#d5320e", "#ef431d", "#f58f79", "#ffcccc", "#CC0066", 
                   "#c27ba0", "#8e7cc3", "#674ea7", "#41087a", "#0b5394", "#003333", "#1f4008", "#29560b", "#4fa516", "#e1ecdd", 
                   "#ffff99", "#f7f767", "#ecc03b", "#ffac47", "#f2842c", "#c2680a")

x_order       <- rep(seq(1, 10, 1), 4)[-tail(which(rep(seq(1, 10, 1), 4) %in% c(7, 8, 9, 10)), 4)] * 6
y_order       <- rep(seq(-1, -4, -1), each = 10)[-tail(which(rep(seq(-1, -4, -1), each = 10) == -4), 4)] * 1

data_col = data.frame(Species = Species_order, Color = color_order, x_order, y_order)
data_col$Species   <- factor(data_col$Species, levels = Species_order)
Tile_cover$Species <- factor(Tile_cover$Species, levels = Species_order)

##### Plot
### Let's start with ELOW
Tile_cover_ELOW  <- Tile_cover %>% dplyr::filter(pH == "ELOW") %>% 
  group_by(Species, Time, Tile) %>% summarise(Cover = sum(Cover)) 
Transplants_ELOW_2 <- Tile_cover_ELOW %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile, ncol = 6) + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(Tile_cover_ELOW$Species)]) +
  ggtitle("Extreme Low pH conditions – Back") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "gray20"),
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
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

### We continue with LOW
Tile_cover_LOW  <- Tile_cover %>% dplyr::filter(pH == "LOW") %>% 
  group_by(Species, Time, Tile) %>% summarise(Cover = sum(Cover)) 
Transplants_LOW_2 <- Tile_cover_LOW %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile, ncol = 6) + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(Tile_cover_LOW$Species)]) +
  ggtitle("Low pH conditions – Back") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "gray20"),
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
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

### We finish with AMB
Tile_cover_AMB  <- Tile_cover %>% dplyr::filter(pH == "AMB") %>% 
  group_by(Species, Time, Tile) %>% summarise(Cover = sum(Cover)) 
Transplants_AMB_2 <- Tile_cover_AMB %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black", show.legend = F) +
  scale_x_discrete(labels = c("", "", "", "")) +
  labs(x = "", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile, ncol = 6) + theme_classic() +
  scale_fill_manual(values = data_col$Color[data_col$Species %in% unique(Tile_cover_AMB$Species)]) +
  ggtitle("Ambient pH conditions – Back") +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "gray20"),
        plot.title = element_text(size = 18, color = "cornflowerblue", face = "bold"),
        panel.grid = element_line(colour = NA),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "cornflowerblue"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

data_col <- data_col %>% mutate(Species_lab = str_replace(Species_order, "\\s(?!sp.)", "\n"))
data_col$Species_lab[data_col$Species == "Diplosoma spongiforme"] = "Diplosoma\nspongiforme"
Legend_Dark <- data_col %>% 
  ggplot(aes(x = x_order, y = y_order, fill = Species, label = Species_lab)) + 
  geom_label(show.legend = F, hjust = 0, 
             color = c(rep("black", 11), rep("white", 5), rep("black", 7), rep("white", 5), rep("black", 8))) +
  scale_x_continuous(name = "", labels = rep("", 6), breaks = seq(10, 60, 10), limits = c(6, 63)) +  
  scale_y_continuous(name = "", labels = rep("", 6), limits = c(-4.5, -0.5)) +
  scale_fill_manual(values = color_order) + 
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = NA),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 16),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

(Dark_Cover <- Transplants_AMB_2 + plot_spacer() + Transplants_LOW_2 + plot_spacer() + Transplants_ELOW_2 + 
    plot_spacer() + Legend_Dark + 
    plot_layout(guides = "collect", heights = c(30, -7.5, 30, -7.5, 30, -1, 30)))

Cover_tot <- Transplants_AMB + Transplants_AMB_2 + plot_spacer() + plot_spacer() +
  Transplants_LOW + Transplants_LOW_2 + plot_spacer() + plot_spacer() +
  Transplants_ELOW + Transplants_ELOW_2 + plot_spacer() + plot_spacer() +
  Legend_Light + Legend_Dark + plot_layout(heights = c(30, -7.5, 30, -7.5, 30, -1, 30), ncol = 2)

ggsave(Cover_tot, file = "Outputs/Figures/Cover/Cover_Transplants.jpg", width = 70, height = 35, units = "cm", dpi = 300)
