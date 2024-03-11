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

##### Plot
### Let's start with ELOW
Tile_cover_ELOW <- Tile_cover %>% dplyr::filter(pH == "ELOW")
# For viz we will take only species with cover >10% at least once
species_of_interest <- Tile_cover_ELOW %>% dplyr::filter(Cover >= 10)
species_of_interest <- unique(species_of_interest$Species)

Transplants_ELOW <- Tile_cover_ELOW %>% 
  #dplyr::filter(Species %in% species_of_interest, Species %notin% c("Perforatus perforatus dead", "tile", "encrusting bryozoan dead")) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = functional.group)) +
  geom_col(position = "stack", color = "black") +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
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
Tile_cover_LOW <- Tile_cover %>% dplyr::filter(pH == "LOW")
# For viz we will take only species with cover >10% at least once
species_of_interest <- Tile_cover_LOW %>% dplyr::filter(Cover >= 10)
species_of_interest <- unique(species_of_interest$Species)

Transplants_LOW <- Tile_cover_LOW %>% 
  dplyr::filter(Species %in% species_of_interest, Species %notin% c("Turf", "tile", "encrusting bryozoan dead")) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black") +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
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
Tile_cover_AMB <- Tile_cover %>% dplyr::filter(pH == "AMB")
# For viz we will take only species with cover >10% at least once
species_of_interest <- Tile_cover_AMB %>% dplyr::filter(Cover >= 10)
species_of_interest <- unique(species_of_interest$Species)

Transplants_AMB <- Tile_cover_AMB %>% dplyr::filter(Species %in% species_of_interest, Species %notin% c("tile")) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black") +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "cornflowerblue"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

Light_Cover <- Transplants_ELOW / Transplants_LOW /Transplants_AMB + plot_layout(guides = "collect")

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

##### Plot
### Let's start with ELOW
Tile_cover_ELOW <- Tile_cover %>% dplyr::filter(pH == "ELOW")
# For viz we will take only species with cover >10% at least once
species_of_interest <- Tile_cover_ELOW %>% dplyr::filter(Cover >= 10)
species_of_interest <- unique(species_of_interest$Species)

Transplants_ELOW_2 <- Tile_cover_ELOW %>% 
  dplyr::filter(Species %in% species_of_interest, Species %notin% c("Perforatus perforatus dead", "tile", "encrusting bryozoan dead")) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black") +
  labs(x = "Incubation time", y = "Cover (%)", color = "Species") +
  facet_wrap(~ Tile) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "gray20", linewidth = 1),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "firebrick3"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

### We continue with LOW
Tile_cover_LOW <- Tile_cover %>% dplyr::filter(pH == "LOW")
# For viz we will take only species with cover >10% at least once
species_of_interest <- Tile_cover_LOW %>% dplyr::filter(Cover >= 10)
species_of_interest <- unique(species_of_interest$Species)

Transplants_LOW_2 <- Tile_cover_LOW %>% 
  dplyr::filter(Species %in% species_of_interest, Species %notin% c("Turf", "tile", "encrusting bryozoan dead")) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black") +
  labs(x = "Incubation time", y = "Overlapped Cover (%)", color = "Species") +
  facet_wrap(~ Tile) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "gray20", linewidth = 1),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "goldenrod"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

### We finish with AMB
Tile_cover_AMB <- Tile_cover %>% dplyr::filter(pH == "AMB")
# For viz we will take only species with cover >10% at least once
species_of_interest <- Tile_cover_AMB %>% dplyr::filter(Cover >= 10)
species_of_interest <- unique(species_of_interest$Species)

Transplants_AMB_2 <- Tile_cover_AMB %>% dplyr::filter(Species %in% species_of_interest, Species %notin% c("tile")) %>% 
  ggplot(aes(x = factor(Time, levels = c("T0", "T1", "T2", "T3")), y = Cover, fill = Species)) +
  geom_col(position = "stack", color = "black") +
  labs(x = "Incubation time", y = "Overlapped Cover (%)", color = "Species") +
  facet_wrap(~ Tile) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "gray20", linewidth = 1),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "royalblue4"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))

Dark_Cover <- Transplants_ELOW_2 / Transplants_LOW_2 /Transplants_AMB_2 + plot_layout(guides = "collect")
(Transplants_ELOW + Transplants_ELOW_2) / (Transplants_LOW + Transplants_LOW_2) / (Transplants_AMB + Transplants_AMB_2)


# Total cover for each tile Light
Tile_cover_T0 = vector("list", 18) ; Tile_cover_T1 = vector("list", 18) 
Tile_cover_T2 = vector("list", 18) ; Tile_cover_T3 = vector("list", 18) ; Tile_cover = vector("list", 18)
for (i in 1:18) {
  Tile_cover_T0[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t0_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T0" = ".") %>% mutate(T0 = ((T0/25*100) / sum(T0/25*100)) * 100)
  Tile_cover_T1[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t1_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T1" = ".") %>% mutate(T1 = ((T1/25*100) / sum(T1/25*100)) * 100)
  Tile_cover_T2[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t2_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T2" = ".") %>% mutate(T2 = ((T2/25*100) / sum(T2/25*100)) * 100)
  Tile_cover_T3[[i]] <- datasets[[i]] %>% column_to_rownames("Species") %>% select(matches("_t3_")) %>%
    rowSums(na.rm = T) %>% data.frame() %>% rename("T3" = ".") %>% mutate(T3 = ((T3/25*100) / sum(T3/25*100)) * 100)
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

sp = data.frame(Species = unique(Tile_cover$Species))
openxlsx::write.xlsx(sp, "species_Benthfun.xlsx")
