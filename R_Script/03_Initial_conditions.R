#########################################################################################################################################
########################################################## Initial conditions ###########################################################
#########################################################################################################################################

### Table 1 ----
Alkalinity_dataset_T0 = Alkalinity_dataset %>% dplyr::filter(`Stage incubation` == "t0") %>% 
  select(`pH condition`, `Stage experiment`, `analysis date`, correction, `Batch sd`) %>% 
  arrange(`Stage experiment`)

## Reformatage
label_decomposition <- str_split(Tile_concerned$Label, fixed("_"))
for (i in 1:length(label_decomposition)) {
  Tile_concerned$`pH condition`[i] <- label_decomposition[[i]][3]
  Tile_concerned$`Stage experiment`[i] <- label_decomposition[[i]][1]
  Tile_concerned$`Stage incubation`[i] <- label_decomposition[[i]][2]
  Tile_concerned$`Tile concerned`[i] <- paste(label_decomposition[[i]][4], 
                                              substr(label_decomposition[[i]][5], 1, nchar(label_decomposition[[i]][5])), sep = "_")}

Time = Tile_concerned %>% drop_na(Label) %>% group_by(`pH condition`, `Stage experiment`) %>% 
  dplyr::filter(`Stage experiment` != "PI") %>% 
  arrange(`Stage experiment`) %>% dplyr::select(Diving_Date, Label, Start_incubation, `pH condition`, `Stage experiment`, Temperature, pH_mV)

Alk_T0 <- Alkalinity_dataset_T0 %>% left_join(Time) %>% 
  select(Diving_Date, Start_incubation, `Stage experiment`, `pH condition`, correction, `Batch sd`, Temperature, pH_mV, Label) %>% data.frame() 
rows_to_remove <- c() ; for (i in seq(1, nrow(Alk_T0), by = 4)) {rows_to_remove <- c(rows_to_remove, i+1, i+2)}
Alk_T0 <- Alk_T0[-rows_to_remove, ]

label_decomposition <- str_split(Nutrients$Sample, fixed("_"))
for (i in 1:length(label_decomposition)) {
  Nutrients$`Stage incubation`[i] <- label_decomposition[[i]][2]
  Nutrients$`Tile concerned`[i] <- paste(label_decomposition[[i]][4], 
                                         substr(label_decomposition[[i]][5], 1, nchar(label_decomposition[[i]][5])), sep = "_")}
Nutrients = Nutrients %>% rename(pH.condition = pH, Stage.experiment = Phase, Label = Sample)

T0 = Alk_T0 %>% left_join(Nutrients, by = c("Label", "pH.condition", "Stage.experiment")) %>% 
  mutate(Sampling_Date = Diving_Date, `Starting_time_GMT+1` = Start_incubation - chron::times("01:00:00"),
         Experimental_set = rep(c("Set_1", "Set_2"), 18)) %>% 
  rename(Stage_experiment = Stage.experiment, pH_condition = pH.condition, Alk_mean = correction, Alk_sd = Batch.sd, 
         NH3_mmol.m3 = `NH3 (mmol m-3)`, PO4_mmol.m3 = `PO4 (mmol m-3)`, NO2_mmol.m3 = `NO2 (mmol m-3)`, NO3_mmol.m3 = `NO3 (mmol m-3)`, 
         SiO4_mmol.m3 = `SiO4 (mmol m-3)`) %>% mutate(Stage_experiment = recode(Stage_experiment, "Tn" = "Tn1")) %>% 
  select(Sampling_Date, `Starting_time_GMT+1`, Experiment, Stage_experiment, pH_condition, Experimental_set, Alk_mean, Alk_sd, NH3_mmol.m3, 
         PO4_mmol.m3, NO2_mmol.m3, NO3_mmol.m3, Temperature, pH_mV)

# Table 1
T0

# Figure 1
(Fig_1 = pH_Long_term %>% 
    mutate(Site = fct_recode(Site, "Extreme low" = "extreme_low", "Low" = "low", "Ambient" = "amb")) %>% 
    mutate(Site = factor(Site, levels = c("Ambient", "Low", "Extreme low"))) %>%
    ggplot(aes(x = pH, y = Site)) +
    geom_density_ridges(alpha=0.6, bandwidth=0.05, aes(fill = Site, color = Site), linewidth = 1) +
    geom_point(data = mean_pH, aes(x = pH, y = Site, fill = Site), shape = 21, color = "black", size = 4, show.legend = F) +
    scale_x_continuous(breaks = seq(5.5,8.5,0.5), limits = c(5.75,8.25), name = expression(pH[T])) +
    scale_color_manual(values=c("royalblue3", "goldenrod1", "firebrick2"), labels = c("Extreme low", "Low", "Ambient")) +
    scale_fill_manual(values=c("royalblue3", "goldenrod1", "firebrick2"), labels = c("Extreme low", "Low", "Ambient")) +
    theme_classic() + 
    theme(axis.text        = element_text(size = 14),
          axis.title       = element_text(size = 16),
          legend.text      = element_text(size = 14),
          legend.title     = element_blank(),
          panel.border     = element_rect(color = "black", fill = NA, size = 1),
          strip.text       = element_blank(), 
          strip.background = element_blank(),
          legend.position  = "bottom"))