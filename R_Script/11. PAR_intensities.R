### Changes over time due to pH change
rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)

### Load datasets
## Permanent PAR
# Calibrate PAR across sites #2nd campaign
ELO_Light <- read.csv2("Data/5. Environmental Long term/Light/20230920_par_odyssey_permanent_extreme_low.csv", sep = ",") %>% 
  rename(PAR_ELO = CALIBRATED.VALUE) %>% dplyr::select(-c(RAW.VALUE, Scan.No))
LOW_Light <- read.csv2("Data/5. Environmental Long term/Light/20230920_par_odyssey_permanent_low.csv", sep = ",") %>% 
  rename(PAR_LOW = CALIBRATED.VALUE) %>% dplyr::select(-c(RAW.VALUE, Scan.No))
AMB_Light <- read.csv2("Data/5. Environmental Long term/Light/20230920_par_odyssey_permanent_amb.csv", sep = ",") %>% 
  rename(PAR_AMB = CALIBRATED.VALUE) %>% dplyr::select(-c(RAW.VALUE, Scan.No))
# PAR 1st Campaign, only ELOW
May_ELO   <- read.csv2("Data/5. Environmental Long term/Light/20230628_par_odyssey_permanent_extreme_low.csv", sep = ",") %>% 
  rename(PAR_ELO = CALIBRATED.VALUE) %>% dplyr::select(-c(RAW.VALUE, Scan.No))
# Diving_Log
Diving_log <- read_excel("Data/1. Diving log/Diving_log_BenthFun.xlsx", 
                         col_types = c("date", "text", "text", "date", "date", 
                                       "date", "text", "text", "numeric", "numeric"), sheet = "Corrected") %>% 
  mutate(., Start_incubation = format(as.POSIXct(Start_incubation), format = "%H:%M:%S"), 
         Stop_Incubation = format(as.POSIXct(Stop_Incubation), format = "%H:%M:%S"), 
         Stop_Alkalinity = format(as.POSIXct(Stop_Alkalinity), format = "%H:%M:%S"))

# Combine Permanent PAR #2nd campaign
PAR_Tot <- ELO_Light %>% left_join(LOW_Light) %>% left_join(AMB_Light) %>% drop_na() %>% 
  mutate(Datetime_bef12 = as.POSIXct(paste(Date, Time), format = "%d/%m/%y %H:%M:%S")) %>% 
  mutate(Datetime_aft12 = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")) %>% 
  dplyr::select(Datetime_bef12, Datetime_aft12, PAR_ELO, PAR_LOW, PAR_AMB) %>% 
  mutate_at(vars(PAR_ELO, PAR_LOW, PAR_AMB), as.numeric)
PAR_Tot$Datetime_bef12[is.na(PAR_Tot$Datetime_bef12)] = PAR_Tot$Datetime_aft12[is.na(PAR_Tot$Datetime_bef12)]
PAR_Tot <- PAR_Tot %>% rename(Datetime = Datetime_bef12) %>% dplyr::select(-Datetime_aft12) 

# Define the ratio for each day
max_values_per_day <- PAR_Tot %>% 
  dplyr::filter(Datetime < "2023-09-20 00:00:00") %>% mutate(Day = as.Date(Datetime)) %>%
  group_by(Day) %>% summarise(max_PAR_ELO = max(PAR_ELO), max_PAR_LOW = max(PAR_LOW), max_PAR_AMB = max(PAR_AMB)) %>% 
  mutate(max_PAR_AMB = max_PAR_AMB / max_PAR_ELO, max_PAR_LOW = max_PAR_LOW / max_PAR_ELO,
         max_PAR_ELO = max_PAR_ELO / max_PAR_ELO) %>% dplyr::filter(Day != "2023-09-16") %>% 
  summarise(mean_PAR_ELO = mean(max_PAR_ELO), sd_ELO = sd(max_PAR_ELO),
            mean_PAR_LOW = mean(max_PAR_LOW), sd_LOW = sd(max_PAR_LOW),
            mean_PAR_AMB = mean(max_PAR_AMB), sd_AMB = sd(max_PAR_AMB))
## The PAR intensity is 23.3 ± 9.5%, and 65.7 ± 3.4% lower in LOW and AMB conditions respectively, compared to ELOW

# Extrapolate for 1st campaign
May_ELO <- May_ELO %>%  
  mutate(Datetime_before_update = as.POSIXct(paste(Date, Time), format = "%d/%m/%y %H:%M:%S")) %>% 
  mutate(Datetime_after_update = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")) %>% 
  dplyr::select(Datetime_before_update, Datetime_after_update, PAR_ELO) 
May_ELO$Datetime_before_update[is.na(May_ELO$Datetime_before_update)] = 
  May_ELO$Datetime_after_update[is.na(May_ELO$Datetime_before_update)]
May_ELO <- May_ELO %>% rename(Datetime = Datetime_before_update) %>% dplyr::select(-Datetime_after_update) 
for(i in 1:length(May_ELO$PAR_ELO)) {
  May_ELO$PAR_LOW[i] = as.numeric(May_ELO$PAR_ELO[i]) * 
    rnorm(1, max_values_per_day[3]$mean_PAR_LOW, max_values_per_day[4]$sd_LOW)
  May_ELO$PAR_AMB[i] = as.numeric(May_ELO$PAR_ELO[i]) * 
    rnorm(1, max_values_per_day[5]$mean_PAR_AMB, max_values_per_day[6]$sd_AMB)}
May_ELO$PAR_ELO = as.numeric(May_ELO$PAR_ELO)

# Combine Permanent PAR Data
Permanent_PAR_Tot <- rbind(May_ELO, PAR_Tot) %>% data.frame()

### Extract the PAR activity for each incubation
## Transplants experiment
# Prepare the PAR file
Folder    <- "Data/2. Incubations/Transplants/Light"
document_files <- list.files(paste(getwd(), Folder, sep = "/"))
files_PAR = list() ; for (i in 1:length(document_files)) {
  files_PAR[[i]]  <- read_csv(paste(Folder, document_files[i], sep = "/"))}
files_PAR <- files_PAR %>% bind_rows() %>% select(-`Scan No`) %>% 
  mutate(Datetime_before_update = as.POSIXct(paste(Date, Time), format = "%d/%m/%y %H:%M:%S"),
         Datetime_after_update  = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
files_PAR$Datetime_before_update[is.na(files_PAR$Datetime_before_update)] = 
  files_PAR$Datetime_after_update[is.na(files_PAR$Datetime_before_update)]
files_PAR <- files_PAR %>% rename(Datetime = Datetime_before_update) %>% dplyr::select(-Datetime_after_update) %>% 
  select(Datetime, Date, Time, `CALIBRATED VALUE`) %>% rename(PAR_intensity = `CALIBRATED VALUE`)
files_PAR = files_PAR %>% group_by(Datetime, Date, Time) %>% summarise(PAR_intensity = mean(PAR_intensity))

# Prepare the diving log document to extract
PAR_to_extract_from_incubations <- Diving_log %>% drop_na(c(`Tile_N°`, Label)) %>% 
  mutate(Tile           = paste("tile_", str_pad(`Tile_N°`, width = 2, pad = "0"), sep = ""),
         Datetime_start = as.POSIXct(paste(Diving_Date, Start_incubation), format = "%Y-%m-%d %H:%M:%S"),
         Datetime_stop  = as.POSIXct(paste(Diving_Date, Stop_Incubation), format = "%Y-%m-%d %H:%M:%S")) 
for (i in 1:length(PAR_to_extract_from_incubations$Label)) {
PAR_to_extract_from_incubations$Time[i] <- strsplit(PAR_to_extract_from_incubations$Label, "_")[[i]][1]
PAR_to_extract_from_incubations$pH[i]   <- strsplit(PAR_to_extract_from_incubations$Label, "_")[[i]][3] }
PAR_to_extract_from_incubations = PAR_to_extract_from_incubations %>% 
  dplyr::select(Tile, Time, pH, Datetime_start, Datetime_stop) %>% 
  dplyr::filter(Time %notin% c("Tn", "Tn2", "PI"))

# For T3 ELOW, same condition as day after
for (i in 55:60) {
  PAR_to_extract_from_incubations$Datetime_start[i] = PAR_to_extract_from_incubations$Datetime_start[i] + 24*3600
  PAR_to_extract_from_incubations$Datetime_stop[i]  = PAR_to_extract_from_incubations$Datetime_stop[i]  + 24*3600
}

extracted_PAR = vector(mode = "list", length = dim(PAR_to_extract_from_incubations)[1]) 
for (i in 1:dim(PAR_to_extract_from_incubations)[1]) {
  extracted_PAR[[i]] = files_PAR %>% 
    dplyr::filter(Datetime >= PAR_to_extract_from_incubations$Datetime_start[i],
                  Datetime <= PAR_to_extract_from_incubations$Datetime_stop[i]) %>%
    mutate(Tile = PAR_to_extract_from_incubations$Tile[i],
           Time = PAR_to_extract_from_incubations$Time[i],
           pH   = PAR_to_extract_from_incubations$pH[i]) %>% 
    group_by(Tile, Time, pH) %>% summarise(PAR_intensity = mean(PAR_intensity)) %>% 
    data.frame()}
extracted_PAR = extracted_PAR %>% bind_rows()
table(extracted_PAR$pH, extracted_PAR$Time)

# It appears we do not have T1 – Problem with Ligh sensors...
# But regarding the trends, T1 and T2 are sunny days both
ggplot(Permanent_PAR_Tot, aes(x = Datetime)) + geom_line(aes(y = PAR_ELO), color = "red") +
  geom_line(aes(y = PAR_LOW), color = "gold") +
  geom_line(aes(y = PAR_AMB), color = "cornflowerblue")

# Compare T1 to T3
T1_Permanent = Permanent_PAR_Tot %>% 
  dplyr::filter(Datetime >= PAR_to_extract_from_incubations$Datetime_start[19],
                Datetime <= PAR_to_extract_from_incubations$Datetime_stop[36]) %>% 
  mutate(Day = as.Date(Datetime)) %>%
  group_by(Day) %>% summarise(PAR_ELO = max(PAR_ELO))
T2_Permanent = Permanent_PAR_Tot %>% 
  dplyr::filter(Datetime >= PAR_to_extract_from_incubations$Datetime_start[37],
                Datetime <= PAR_to_extract_from_incubations$Datetime_stop[54]) %>% 
  mutate(Day = as.Date(Datetime)) %>%
  group_by(Day) %>% summarise(PAR_ELO = max(PAR_ELO))

# Define a ration to extrapolate Light
T1_ratio_ELO = (T1_Permanent$PAR_ELO / T2_Permanent$PAR_ELO)[1]
T1_ratio_LOW = (T1_Permanent$PAR_ELO / T2_Permanent$PAR_ELO)[2]
T1_ratio_AMB = (T1_Permanent$PAR_ELO / T2_Permanent$PAR_ELO)[3]

# Define T1 dataset
extracted_PAR_T1 = extracted_PAR[19:36,]
extracted_PAR_T1$PAR_intensity[extracted_PAR_T1$pH == "ELOW"] = 
  extracted_PAR_T1$PAR_intensity[extracted_PAR_T1$pH == "ELOW"] * T1_ratio_ELO
extracted_PAR_T1$PAR_intensity[extracted_PAR_T1$pH == "LOW"] = 
  extracted_PAR_T1$PAR_intensity[extracted_PAR_T1$pH == "LOW"] * T1_ratio_LOW
extracted_PAR_T1$PAR_intensity[extracted_PAR_T1$pH == "AMB"] = 
  extracted_PAR_T1$PAR_intensity[extracted_PAR_T1$pH == "AMB"] * T1_ratio_AMB
extracted_PAR_T1$Time = "T1"

# Define final dataset for transplants
PAR_Transplants = rbind(extracted_PAR, extracted_PAR_T1) %>% data_frame() %>% arrange(Time) %>% data.frame()
xlsx::write.xlsx(PAR_Transplants, file = "Outputs/Summary/PAR_Transplants.xlsx", row.names = F)