#########################################################################################################################################
############################################################ Source Scripts #############################################################
#########################################################################################################################################

source("R_Script/01_Packages_and_Functions.R") # Set up environment
source("R_Script/02_Load_Data.R")              # Load data sets
source("R_Script/03_Initial_conditions.R")     # Figure 1 and Table 1
source("R_Script/04_Biomass_change.R")         # Figure 2
source("R_Script/05_Transplant_change.R")      # Figure 3
source("R_Script/06_Historical_change.R")      # Figure 4

#########################################################################################################################################
############################################################### Outputs #################################################################
#########################################################################################################################################

ggsave(Fig_1, file = "Outputs/Figures/Raw_Figures/Figure_1.png", width = 15, height = 15, units = "cm", dpi = 300)
ggsave(Fig_2, file = "Outputs/Figures/Raw_Figures/Figure_2.png", width = 30, height = 9 , units = "cm", dpi = 300)
ggsave(Fig_3, file = "Outputs/Figures/Raw_Figures/Figure_3.png", width = 50, height = 25, units = "cm", dpi = 300)
ggsave(Fig_4, file = "Outputs/Figures/Raw_Figures/Figure_4.png", width = 20, height = 20, units = "cm", dpi = 300)