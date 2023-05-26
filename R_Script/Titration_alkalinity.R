# non-linear regression for alkalinity files
# Steeve Comeau and Fred Gazeau,
# updated by Jeremy Carlot 26/05/2023

rm(list = ls()) ; options(digits = 9, cores = 4, warn = -1)
library(hms) ; library(tidyverse) ; library(seacarb) ; library(stringr) ; library(readr)

calibration <- read.csv("Data/Spring_2023/Transplants/Alkalinity/calibration_TRIS.csv", 
                          dec = ",", sep = ";")

m0 <- as.numeric(readline("What is the sample's weight?"))
S  <- as.numeric(readline("What is the sample's salinity?"))

#################################################################
# Reading the databases: 
#################################################################

mVTris = calibration$mV[length(calibration$mV)]
pHTris = calibration$pH[length(calibration$pH)]
At <- NULL

Folder <- "Data/Spring_2023/Transplants/Alkalinity/Titration_exports"
file  <- list.files(Folder)

list_files  <- NULL 
list_files  <- c(list_files, t) 
sample_name <- NULL

# Files from the titrator
for (file in file) { l <- read.csv(paste(Folder, file, sep = "/"), fileEncoding = "UCS-2LE", skip = 17, sep = "\t")

p  <- l %>% as_tibble() %>% 
  rename(Volume = mL, mV = mV, `mV/mL` = mV.mL, Time = s, temp = X.C, dV = mL.1, dE = mV.1, X = X) %>% 
  dplyr::select(-c(X, dV, dE)) %>% 
  dplyr::mutate(weight = m0, salinity = S)

S  <- p[,7]  				# Salinity from your sample
T  <- p[,5]					# Temperature during the titration
Tk <- T + 273.15 		# Absolute temperature during the titration
C  <- 0.1						# acid titration
d  <- 1.002 				# acid density
R  <- 8.31447215
F  <- 96485.339924

pH <- pHTris + (mVTris/1000 - p[,2]/1000) / (R*(Tk)*log(10)/F) ; colnames(pH) <- c("pH")
p  <- cbind(p,pH)

#################################################################
# Value selection between pH 3 and 3.5

pHlim    <- 3.5
i        <- p[,8] < pHlim
z        <- p[i, ]
z        <- subset(z, z[,8]>3)
S        <- z[,7]					# Salinity from your sample	
T        <- z[,5]					# Temperature during the titration
Tk       <- T + 273.15 		# Absolute temperature during the titration
acid_vol <- z[,1]			    # acid volume addition
m        <- acid_vol*d		# acid mass
m0       <- z[,6]					# sample mass

#################################################################
# Gran Curve
#################################################################

F1 <- (m0 + m) * exp((z[,2] / 1000) / (R * (Tk) / F))

#################################################################
# Linear regression
#################################################################

f  <- lm(m ~ F1)
TA <- f$coefficients[1] * C / m0[1]

#################################################################

E0     <- z[,2] / 1000 - (R * Tk / F) * log((-m0 * TA + m * C) / (m0 + m))
Hprime <- exp((z[,2]/1000-E0)/(R*Tk/F))

St     <-(0.14 / 96.062) * (S / 1.80655)
Ksa    <- seacarb::Ks(S, T, 0)
Z      <- 1 + St / Ksa
Ft     <- (0.000067 / 18.998) * (S / 1.80655)
Kf     <- exp(874 / Tk - 9.68 + 0.111 * S^(0.5))
y      <- (m / m0)
regr   <-nls(y ~ ((At + (St/(1 + Ksa * Z / (f * Hprime))) + (Ft/ (1 + Kf / (f * Hprime))) + (f * Hprime / Z)) / 
                    (C - f * Hprime / Z)), 
             start = list(At = TA, f = 1))

#################################################################
# Resultat exportation
#################################################################

print(summary(regr))
print(is.list(regr))
print(names(regr))
print(names(summary(regr)))
print(coef(regr))
coefs       <- coef(regr)
At          <- append(At, coefs[1])
sample_name <- append(sample_name, p[1, 8])
}

Results <- cbind(At * 1000000, list_files)
write.table(Results, "Data/Spring_2023/Transplants/Alkalinity/Results_TA/Results_TA.csv", sep="\t")
print(paste("The alkalinity of your sample is: ", (At * 1000000), sep = ""))

# usefull fumctions
pH <- pHTris + (mVTris / 1000 - 211 / 1000) / (R * (Tk) * log(10) / F)