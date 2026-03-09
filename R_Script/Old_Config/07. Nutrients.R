options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

### Data
Nutrients  <- read_excel("Data/6. Nutrients/Nutrients.xlsx", sheet = "Sheet1") %>% select(-c(`DATA ANALYSIS`, Package))
Biomass    <- read_excel("Outputs/Summary/Biomass_Transplants_data.xlsx", sheet = "Sheet1") 
Diving_log <- read_excel("Data/1. Diving log/Diving_log_BenthFun.xlsx", 
                         col_types = c("date", "text", "text", "date", "date", 
                                       "date", "text", "text", "numeric", "numeric"),
                         sheet = "Corrected") %>% 
  mutate(., Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S"))

# Raw data
NH3 <- Nutrients %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `NH3 (mmol m-3)`, fill = pH, x = Phase)) + geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
PO4 <- Nutrients %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `PO4 (mmol m-3)`, fill = pH, x = Phase)) + geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
NO2 <- Nutrients %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `NO2 (mmol m-3)`, fill = pH, x = Phase)) + geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
NO3 <- Nutrients %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `NO3 (mmol m-3)`, fill = pH, x = Phase)) + geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
SiO4 <- Nutrients %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `SiO4 (mmol m-3)`, fill = pH, x = Phase)) + geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
NH3 + PO4 + NO2 + NO3 + SiO4 + plot_layout(guides = "collect", ncol = 5) & theme(legend.position = "bottom")

# Transplants
Transplants_nut <- Nutrients %>% dplyr::filter(Experiment == "Transplants") %>% group_by(Phase,pH) %>% group_split()

for (j in 1:12) {
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

NH3 <- Transplants_nut %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `NH3 (mmol m-3)`, fill = pH, x = Phase)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
PO4 <- Transplants_nut %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `PO4 (mmol m-3)`, fill = pH, x = Phase)) + 
  geom_boxplot(outliers = FALSE) + 
  #geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + 
  theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
NO2 <- Transplants_nut %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `NO2 (mmol m-3)`, fill = pH, x = Phase)) + 
  #geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + 
  geom_boxplot(outliers = FALSE) + 
  theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
NO3 <- Transplants_nut %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `NO3 (mmol m-3)`, fill = pH, x = Phase)) + 
  geom_boxplot(outliers = FALSE) +
  #geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + 
  theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
SiO4 <- Transplants_nut %>% mutate(pH = factor(pH, levels = c("ELOW", "LOW", "AMB"))) %>% dplyr::filter(Experiment != "PI Curves") %>% 
  ggplot(., aes(y = `SiO4 (mmol m-3)`, fill = pH, x = Phase)) + 
  #geom_jitter(shape= 21, width = .2, size = 2, alpha = .8) + 
  geom_boxplot(outliers = FALSE) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick2", "gold", "royalblue3"))
NH3 + PO4 + NO2 + NO3 + SiO4 + plot_layout(guides = "collect", ncol = 5) & theme(legend.position = "bottom")

# Build Nutrients dataset
Nutrients_database = Diving_log %>% dplyr::select(Label, `Tile_N°`) %>% 
  drop_na() %>% left_join(Transplants_nut %>% rename(Label = Sample)) %>% 
  dplyr::select(`Tile_N°`, pH, Phase, `NH3 (mmol m-3)`, `PO4 (mmol m-3)`, `NO2 (mmol m-3)`, `NO3 (mmol m-3)`, `SiO4 (mmol m-3)`) %>% 
  rename(Time = Phase, Tile = `Tile_N°`) %>% mutate(Tile = paste("tile_", str_pad(Tile, width = 2, pad = "0"), sep = "")) %>% 
  drop_na()

Nutrients_database = data.frame(Tile = rep(Nutrients_database$Tile, 5),
                                pH   = rep(Nutrients_database$pH,   5),
                                Time = rep(Nutrients_database$Time, 5),
                                Nutrients_var = c(rep("NH3", length(Nutrients_database$`NH3 (mmol m-3)`)),
                                                  rep("PO4", length(Nutrients_database$`PO4 (mmol m-3)`)),
                                                  rep("NO2", length(Nutrients_database$`NO2 (mmol m-3)`)),
                                                  rep("NO3", length(Nutrients_database$`NO3 (mmol m-3)`)),
                                                  rep("SiO4", length(Nutrients_database$`SiO4 (mmol m-3)`))),
                                Nutrients_val = c(Nutrients_database$`NH3 (mmol m-3)`, Nutrients_database$`PO4 (mmol m-3)`,
                                                  Nutrients_database$`NO2 (mmol m-3)`, Nutrients_database$`NO3 (mmol m-3)`,
                                                  Nutrients_database$`SiO4 (mmol m-3)`))

xlsx::write.xlsx(Nutrients_database, file = "Outputs/Summary/Nutrients_Transplants_data.xlsx", row.names = F)