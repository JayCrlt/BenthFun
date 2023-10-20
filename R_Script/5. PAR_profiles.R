rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl)

# Diving log
Diving_log <- read_excel("Data/1. Diving log/Diving_log_BenthFun.xlsx", 
                         col_types = c("date", "text", "text", "date", "date", 
                                       "date", "text", "text", "numeric", "numeric"),
                         sheet = "Corrected") |> 
  mutate(Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S"))

PI_Photosynthesis <- read_excel("Outputs/Tables/PI/PI_Photosynthesis.xlsx")

label_decomposition <- str_split(Diving_log$Label, fixed("_"))
for (i in 1:length(label_decomposition)) {
  Diving_log$`Stage experiment`[i] <- label_decomposition[[i]][1] 
  Diving_log$pH_Cond[i] <- label_decomposition[[i]][3] }

Diving_log_PAR <- Diving_log |> dplyr::filter(`Stage experiment` == "PI")

# Functions
`%notin%` <- Negate(`%in%`)

Folder         <- "Data/2. Incubations/PI_Curves/Light"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))

files_PAR = list() ; for (i in 1:length(document_files)) {
  files_PAR[[i]]           <- read.csv(paste(Folder, document_files[i], sep = "/"), header=T) }

Sensor_Temporal_1 <- rbind(files_PAR[[1]], files_PAR[[3]]) |> data.frame() |> 
  mutate(Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) |> 
  dplyr::select(c(Datetime, Date, Time, raw_value, calibrated_value)) |> 
  mutate(sensor_used = rep("Sensor_Temporal_1", dim(rbind(files_PAR[[1]], files_PAR[[3]]))[1]))

Sensor_Temporal_3 <- rbind(files_PAR[[2]], files_PAR[[4]]) |> data.frame() |> 
  mutate(Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) |> 
  dplyr::select(c(Datetime, Date, Time, raw_value, calibrated_value)) |> 
  mutate(sensor_used = rep("Sensor_Temporal_3", dim(rbind(files_PAR[[2]], files_PAR[[4]]))[1]))

Light_Activity <- rbind(Sensor_Temporal_1, Sensor_Temporal_3) |> data.frame() |> group_by(Date) |> group_split()

ELOW_AM <- Light_Activity[[2]] |> dplyr::filter(Datetime >= "2023-05-29 06:10:00", Datetime <= "2023-05-29 13:35:00")
ELOW_PM <- Light_Activity[[2]] |> dplyr::filter(Datetime >= "2023-05-29 16:50:00", Datetime <= "2023-05-29 20:55:00")
LOW_AM <- Light_Activity[[3]] |> dplyr::filter(Datetime >= "2023-05-30 06:25:00", Datetime <= "2023-05-30 14:00:00")
LOW_PM <- Light_Activity[[3]] |> dplyr::filter(Datetime >= "2023-05-30 17:15:00", Datetime <= "2023-05-30 19:50:00")
AMB_AM <- Light_Activity[[4]] |> dplyr::filter(Datetime >= "2023-05-31 06:20:00", Datetime <= "2023-05-31 13:45:00")
AMB_PM <- Light_Activity[[4]] |> dplyr::filter(Datetime >= "2023-05-31 17:05:00", Datetime <= "2023-05-31 19:45:00")

Light_Activity <- rbind(ELOW_AM, ELOW_PM, LOW_AM, LOW_PM, AMB_AM, AMB_PM) |> data.frame()
avg_light <- Light_Activity |> group_by(Datetime) |> summarise(calibrated_value_avg = mean(calibrated_value), 
                                                                 calibrated_value_sd  = sd(calibrated_value))
values_to_rm = avg_light |> dplyr::filter(calibrated_value_sd >= 20)
Light_Activity = Light_Activity |> dplyr::filter(Datetime %notin% c(values_to_rm$Datetime))
Light_Activity <- Light_Activity |> group_by(Date) |> group_split()

A <- ggplot(Light_Activity[[1]], aes(x = Datetime, y = calibrated_value)) + 
  geom_point(aes(fill = sensor_used), shape = 21, size = 3, alpha = .7) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 15), 
              linetype = "dashed", size = 0.5, col = "black", se = F) +
  scale_x_datetime(name = "", limits = as.POSIXct(strptime(c("2023-05-29 06:00:00", "2023-05-29 21:00:00"), 
                                                           format = "%Y-%m-%d %H:%M:%S")), 
                   breaks = scales::date_breaks("3 hour"), labels = scales::date_format("%H:%M")) + 
  scale_y_continuous(name = expression("PAR irradiance (μmol."*m^-2*"."*s^-1*")"), limits = c(0,600)) +
  theme_classic() + ggtitle("Extreme Low conditions") +
  guides(fill = guide_legend(title = "Light sensor used:")) +
  scale_fill_discrete(labels=c('Light Sensor N°1', 'Light Sensor N°2'))

B <- ggplot(Light_Activity[[2]], aes(x = Datetime, y = calibrated_value)) + 
  geom_point(aes(fill = sensor_used), shape = 21, size = 3, alpha = .7) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 25), 
              linetype = "dashed", size = 0.5, col = "black", se = F) +
  scale_x_datetime(name = "", limits = as.POSIXct(strptime(c("2023-05-30 06:00:00", "2023-05-30 21:00:00"), 
                                                           format = "%Y-%m-%d %H:%M:%S")), 
                   breaks = scales::date_breaks("3 hour"), labels = scales::date_format("%H:%M")) + 
  scale_y_continuous(name = "", limits = c(0,600)) +
  theme_classic() + ggtitle("Low conditions") +
  guides(fill = guide_legend(title = "Light sensor used:")) +
  scale_fill_discrete(labels=c('Light Sensor N°1', 'Light Sensor N°2')) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

C <- ggplot(Light_Activity[[3]], aes(x = Datetime, y = calibrated_value)) + 
  geom_point(aes(fill = sensor_used), shape = 21, size = 3, alpha = .7) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE, k = 10), 
              linetype = "dashed", size = 0.5, col = "black", se = F) +
  scale_x_datetime(name = "", limits = as.POSIXct(strptime(c("2023-05-31 06:00:00", "2023-05-31 21:00:00"), 
                                                           format = "%Y-%m-%d %H:%M:%S")), 
                   breaks = scales::date_breaks("3 hour"), labels = scales::date_format("%H:%M")) + 
  scale_y_continuous(name = "", limits = c(0,600)) +
  theme_classic() + ggtitle("Ambient conditions") +
  guides(fill = guide_legend(title = "Light sensor used:")) +
  scale_fill_discrete(labels=c('Light Sensor N°1', 'Light Sensor N°2')) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

Light_Profile_PI_Curves <- A + B + C + plot_layout(guides = "collect") & theme(legend.position = "bottom") 

## Find the average PAR irradiance for each incubation
Diving_log_PAR <- Diving_log_PAR |> 
  mutate(Start_incubation = as.POSIXct(paste(Diving_log_PAR$Diving_Date, Diving_log_PAR$Start_incubation, sep = " "), 
                                       format = "%Y-%m-%d %H:%M:%S"),
         Stop_Incubation = as.POSIXct(paste(Diving_log_PAR$Diving_Date, Diving_log_PAR$Stop_Incubation, sep = " "), 
                                       format = "%Y-%m-%d %H:%M:%S"),
         Stop_Alkalinity = as.POSIXct(paste(Diving_log_PAR$Diving_Date, Diving_log_PAR$Stop_Alkalinity, sep = " "), 
                                       format = "%Y-%m-%d %H:%M:%S"))

PAR_value = vector("list", length(Diving_log_PAR$Diving_Date)) ; for (i in 1:length(Diving_log_PAR$Diving_Date)) {
PAR_value[[i]] = avg_light |> dplyr::filter(Datetime >= Diving_log_PAR$Start_incubation[[i]], 
                           Datetime <= Diving_log_PAR$Stop_Incubation[[i]]) |> 
  summarise(PAR_irradiance_mean = mean(calibrated_value_avg), PAR_irradiance_sd = sd(calibrated_value_avg)) }
PAR_value <- PAR_value |> bind_rows()
Diving_log_PAR <- cbind(Diving_log_PAR, PAR_value) |> data.frame()

## Barplot
Diving_log_PAR_barplot <- Diving_log_PAR |> dplyr::filter(Tile_N. != "<NA>") |> 
  mutate(Incubation_Time = c(rep("T1", 3), rep("T2", 3), rep("T3", 3), rep("T4", 3), rep("T5", 3), rep("T6", 3), 
                              rep("T7", 3), rep("T8", 3),
                              rep("T1", 3), rep("T2", 3), rep("T3", 3), rep("T4", 3), rep("T5", 3), rep("T6", 3), rep("T7", 3), 
                              rep("T1", 3), rep("T2", 3), rep("T3", 3), rep("T4", 3), rep("T5", 3), rep("T6", 3), rep("T7", 3)))

Diving_log_PAR_barplot <- Diving_log_PAR_barplot |> group_by(Incubation_Time, pH_Cond) |> 
  summarise(PAR_mean = mean(PAR_irradiance_mean), PAR_sd = mean(PAR_irradiance_sd))

PAR_Barplot <- Diving_log_PAR_barplot |> mutate(pH_Cond = fct_relevel(pH_Cond, "ELOW", "LOW", "AMB")) |> 
  ggplot() + 
  geom_bar(aes(x = Incubation_Time, y = PAR_mean, fill = PAR_mean), stat = "identity", col = "black") + 
  geom_errorbar(aes(x = Incubation_Time, ymin = PAR_mean - PAR_sd, ymax = PAR_mean + PAR_sd), width = 0.2) +
  facet_wrap(~pH_Cond) + theme_classic() +
  scale_y_continuous(name = expression("PAR irradiance (μmol."*m^-2*"."*s^-1*")"), breaks = seq(0,600,100)) +
  scale_x_discrete(name = "") + guides(fill = guide_legend(title = "PAR irradiance"))
  
## Generate a summary table for the PI curves

Diving_log_PAR <- Diving_log_PAR |>
  mutate(incubation_time = difftime(Diving_log_PAR$Stop_Incubation, Diving_log_PAR$Start_incubation))

## pH
# Constantes
R = 8.31451 ; F = 96485.309 ; STris = 34.5
###Tris values at different temperature
Tris_data = data.frame(mVTris = c(-89.5, -87.7, -86.2, -85.4), TTris = c(12.15, 13.98, 15.57, 16.27), Date = rep("07/06/2023",4))
mVTris_t  = lm(mVTris ~ TTris, data = Tris_data) ; plot(Tris_data$TTris, Tris_data$mVTris) ; abline(mVTris_t)
####pH
mvTris= Diving_log_PAR$Temperature * mVTris_t$coefficients[2] + mVTris_t$coefficients[1]
phTris = (11911.08 - 18.2499 * STris - 0.039336 * STris^2) * (1/(Diving_log_PAR$Temperature + 273.15)) - 366.27059 + 
  0.53993607 * STris + 0.00016329 * STris^2 + (64.52243 - 0.084041 * STris) * log(Diving_log_PAR$Temperature + 273.15) - 
  0.11149858 * (Diving_log_PAR$Temperature + 273.15)
Diving_log_PAR$`pH_logH+` = phTris + (mvTris / 1000 - Diving_log_PAR$pH_mV / 1000) /
  (R * (Diving_log_PAR$Temperature + 273.15) * log(10) / F)

### PI Curve elaboration
PI_Photosynthesis <- PI_Photosynthesis |> mutate(common_label = paste(pH_cond, incubation_time, sep = "_"))
Diving_log_PAR_barplot <- Diving_log_PAR_barplot |> mutate(common_label = paste(pH_Cond, Incubation_Time, sep = "_"))

Mix_dataset_PI_PAR <- Diving_log_PAR_barplot |> left_join(PI_Photosynthesis) |> 
  dplyr::select(-c(common_label, pH_cond, incubation_time))

# Amb model
Mix_dataset_PI_PAR_AMB <- Mix_dataset_PI_PAR |> dplyr::filter(pH_Cond == "AMB")
NLS_AMB <- nls(formula = net_photosynthesis_rate_avg ~ a * (1 - b * (exp(-c * PAR_mean))),
    data = Mix_dataset_PI_PAR_AMB,
    start = list(a = 5, b = 1.5, c = 0.005))

Observed_values = Mix_dataset_PI_PAR_AMB$net_photosynthesis_rate_avg
Predicted_values = predict(NLS_AMB, Mix_dataset_PI_PAR_AMB)
R2_AMB = 1-sum((Observed_values - Predicted_values)^2) / (length(Observed_values) * var(Observed_values))

# Low model
Mix_dataset_PI_PAR_LOW <- Mix_dataset_PI_PAR |> dplyr::filter(pH_Cond == "LOW")
NLS_LOW <- nls(formula = net_photosynthesis_rate_avg ~ a * (1 - b * (exp(-c * PAR_mean))),
               data = Mix_dataset_PI_PAR_LOW,
               start = list(a = 5, b = 1.5, c = 0.005))

Observed_values = Mix_dataset_PI_PAR_LOW$net_photosynthesis_rate_avg
Predicted_values = predict(NLS_LOW, Mix_dataset_PI_PAR_LOW)
R2_LOW = 1-sum((Observed_values - Predicted_values)^2) / (length(Observed_values) * var(Observed_values))

# Low model
Mix_dataset_PI_PAR_ELOW <- Mix_dataset_PI_PAR |> dplyr::filter(pH_Cond == "ELOW")
NLS_ELOW <- nls(formula = net_photosynthesis_rate_avg ~ a * (1 - b * (exp(-c * PAR_mean))),
               data = Mix_dataset_PI_PAR_ELOW,
               start = list(a = 5, b = 1.5, c = 0.005))

Observed_values = Mix_dataset_PI_PAR_ELOW$net_photosynthesis_rate_avg
Predicted_values = predict(NLS_ELOW, Mix_dataset_PI_PAR_ELOW)
R2_ELOW = 1-sum((Observed_values - Predicted_values)^2) / (length(Observed_values) * var(Observed_values))

### Predictions
Predicted_df <- data.frame(PAR_mean = seq(0,600,1))
Predicted_df <- Predicted_df |> mutate(photo_predicted_ELOW = predict(NLS_ELOW, Predicted_df),
                                       photo_predicted_LOW  = predict(NLS_LOW, Predicted_df),
                                       photo_predicted_AMB  = predict(NLS_AMB, Predicted_df))

Mix_dataset_PI_PAR = Mix_dataset_PI_PAR |> mutate(Tile_N. = factor(Tile_N., levels = c(2, 9, 18, 10,19, 29, 4, 8, 14)))
Mix_dataset_PI_PAR_curves = Mix_dataset_PI_PAR |> group_by(pH_Cond) |> group_split()

PI_ELOW <- ggplot(Mix_dataset_PI_PAR_curves[[2]]) + 
  geom_line(data = Predicted_df, aes(x = PAR_mean, y = photo_predicted_ELOW), color = "firebrick", linewidth = 1, linetype = "dashed") +
  geom_point(aes(x = PAR_mean, y = net_photosynthesis_rate_avg, fill = Tile_N.), shape = 21, color = "black", size = 3) + 
  scale_fill_manual(values = c("firebrick1", "firebrick3", "darkred")) +
  scale_x_continuous(name = expression("PAR irradiance (μmol."*m^-2*"."*s^-1*")"), limits = c(0,600)) +
  scale_y_continuous(name = expression(O[2]~"concentration (mg."*L^-1*h^-1*")"), limits = c(-4,10)) +
  theme_classic() + ggtitle("Extreme Low conditions") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        title = element_text(size = 17)) +
  annotate('text', 50, 10, label = expression("y" == "3.13 × (1 - 1.61 × exp"^(0.007 * x)*")"), parse = TRUE, hjust = 0) +
  annotate('text', 50, 9, label = expression("R"^2 == "0.52"), parse = TRUE, hjust = 0)

PI_LOW <- ggplot(Mix_dataset_PI_PAR_curves[[3]]) + 
  geom_line(data = Predicted_df, aes(x = PAR_mean, y = photo_predicted_LOW), color = "orange", linewidth = 1, linetype = "dashed") +
  geom_point(aes(x = PAR_mean, y = net_photosynthesis_rate_avg, fill = Tile_N.), shape = 21, color = "black", size = 3) + 
  scale_fill_manual(values = c("gold", "goldenrod", "goldenrod4")) +
  scale_x_continuous(name = expression("PAR irradiance (μmol."*m^-2*"."*s^-1*")"), limits = c(0,600)) +
  scale_y_continuous(name = expression(O[2]~"concentration (mg."*L^-1*h^-1*")"), limits = c(-4,10)) +
  theme_classic() + ggtitle("Low conditions") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        title = element_text(size = 17),
        legend.text = element_text(size = 15)) +
  annotate('text', 50, 10, label = expression("y" == "6.94 × (1 - 1.32 × exp"^(0.007 * x)*")"), parse = TRUE, hjust = 0) +
  annotate('text', 50, 9, label = expression("R"^2 == "0.58"), parse = TRUE, hjust = 0)

PI_AMB <- ggplot(Mix_dataset_PI_PAR_curves[[1]]) + 
  geom_line(data = Predicted_df, aes(x = PAR_mean, y = photo_predicted_AMB), color = "royalblue", linewidth = 1, linetype = "dashed") +
  geom_point(aes(x = PAR_mean, y = net_photosynthesis_rate_avg, fill = Tile_N.), shape = 21, color = "black", size = 3) + 
  scale_fill_manual(values = c("dodgerblue", "dodgerblue3", "dodgerblue4")) +
  scale_x_continuous(name = expression("PAR irradiance (μmol."*m^-2*"."*s^-1*")"), limits = c(0,600)) +
  scale_y_continuous(name = expression(O[2]~"concentration (mg."*L^-1*h^-1*")"), limits = c(-4,10)) +
  theme_classic() + ggtitle("Ambient conditions") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        title = element_text(size = 17),
        legend.text = element_text(size = 15)) +
  annotate('text', 50, 10, label = expression("y" == "5.13 × (1 - 1.51 × exp"^(0.008 * x)*")"), parse = TRUE, hjust = 0) +
  annotate('text', 50, 9, label = expression("R"^2 == "0.73"), parse = TRUE, hjust = 0)

PI_Relationships <- PI_ELOW + PI_LOW + PI_AMB + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Export documents
ggsave(PI_Relationships, filename = "PI_relationships.png", path = "Outputs/Figures/PI", device = "png", width = 12, height = 5) 