### Changes over time due to pH change
rm(list = ls()) ; options(cores = 4, warn = -1) ; library(tidyverse) ; library(patchwork) ; library(readxl) ; library(brms)
`%notin%` = Negate(`%in%`)

# Calibrate PAR across sites
ELO_Light <- read.csv2("Data/5. Environmental Long term/Light/20230920_par_odyssey_permanent_extreme_low.csv", sep = ",")
LOW_Light <- read.csv2("Data/5. Environmental Long term/Light/20230920_par_odyssey_permanent_low.csv", sep = ",")
AMB_Light <- read.csv2("Data/5. Environmental Long term/Light/20230920_par_odyssey_permanent_amb.csv", sep = ",")

# Rename variable of interest
ELO_Light <- ELO_Light %>% rename(PAR_ELO = CALIBRATED.VALUE) %>% dplyr::select(-c(RAW.VALUE, Scan.No))
LOW_Light <- LOW_Light %>% rename(PAR_LOW = CALIBRATED.VALUE) %>% dplyr::select(-c(RAW.VALUE, Scan.No))
AMB_Light <- AMB_Light %>% rename(PAR_AMB = CALIBRATED.VALUE) %>% dplyr::select(-c(RAW.VALUE, Scan.No))

# Combine
PAR_Tot <- ELO_Light %>% left_join(LOW_Light) %>% left_join(AMB_Light) %>% drop_na() %>% 
  mutate(Datetime_bef12 = as.POSIXct(paste(Date, Time), format = "%d/%m/%y %H:%M:%S")) %>% 
  mutate(Datetime_aft12 = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")) %>% 
  dplyr::select(Datetime_bef12, Datetime_aft12, PAR_ELO, PAR_LOW, PAR_AMB) %>% 
  mutate_at(vars(PAR_ELO, PAR_LOW, PAR_AMB), as.numeric)
PAR_Tot$Datetime_bef12[is.na(PAR_Tot$Datetime_bef12)] = PAR_Tot$Datetime_aft12[is.na(PAR_Tot$Datetime_bef12)]
PAR_Tot <- PAR_Tot %>% rename(Datetime = Datetime_bef12) %>% dplyr::select(-Datetime_aft12) %>% 
  dplyr::filter(Datetime < "2023-09-20 00:00:00")

# Define the ratio for each day
max_values_per_day <- PAR_Tot %>% mutate(Day = as.Date(Datetime)) %>%
  group_by(Day) %>% summarise(max_PAR_ELO = max(PAR_ELO), max_PAR_LOW = max(PAR_LOW), max_PAR_AMB = max(PAR_AMB)) %>% 
  mutate(max_PAR_AMB = max_PAR_AMB / max_PAR_ELO, max_PAR_LOW = max_PAR_LOW / max_PAR_ELO,
         max_PAR_ELO = max_PAR_ELO / max_PAR_ELO) %>% dplyr::filter(Day != "2023-09-16") %>% 
  summarise(mean_PAR_ELO = mean(max_PAR_ELO), sd_ELO = sd(max_PAR_ELO),
            mean_PAR_LOW = mean(max_PAR_LOW), sd_LOW = sd(max_PAR_LOW),
            mean_PAR_AMB = mean(max_PAR_AMB), sd_AMB = sd(max_PAR_AMB))
## The PAR intensity is 23.3 ± 9.5%, and 65.7 ± 3.4% lower in LOW and AMB conditions respectively, compared to ELOW

### Extract the PAR activity for each in cubation