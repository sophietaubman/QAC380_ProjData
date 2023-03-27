# Uploading Data Set
MASTER_CLEAN_DATA_FOTM <- read_excel("~/Downloads/MASTER CLEAN DATA FOTM.xlsx")
View(MASTER_CLEAN_DATA_FOTM)

# Load libraries needed

library(descr)
library(ggplot2)

# DATA MANAGEMENT #

# Variables: 
  # unique ID = loyalty number
  # Physical
  # mental health: 
  # snap status: Y/N 
  # employment status: Y/N 
  # Retired: Y/N
  # ethnicity: Hispanic Y/N
  # race demographics: American Indian or Alaska Native, Asian, Black or African American, Native Hawaiian or Other Pacific Islander, White ore Caucasian, Oteher Race

# Make New Subset of Data Using Variables of Interest:

FOTM_BL_Sub <- MASTER_CLEAN_DATA_FOTM[,c("Loyalty card number","Age", "GENDER","RACE","HISPANIC","INSURANCEYN","MEDICARE","MEDICAID","EMPLOYED","RETIRED","DISABLED","SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT","HEALTH_MENTAL_0_TEXT")]
View(FOTM_BL_Sub)

# Renaming Variables:

names(FOTM_BL_Sub)[names(FOTM_BL_Sub)== "HEALTH_PHYS_0_TEXT"] <- "HEALTH_PHYS"
names(FOTM_BL_Sub)[names(FOTM_BL_Sub)== "HEALTH_MENTAL_0_TEXT"] <- "HEALTH_MENTAL"

# Labeling Retirement and Employment as NA:

FOTM_BL_Sub$EMPLOYED[FOTM_BL_Sub$RETIRED == "YES" & FOTM_BL_Sub$EMPLOYED == "NO"] <- NA
FOTM_BL_Sub$RETIRED[FOTM_BL_Sub$EMPLOYED == "YES" & FOTM_BL_Sub$RETIRED == "NO"] <- NA

# Deleting a Row:

FOTM_BL_Sub_1 <- FOTM_BL_Sub[-1,]

View(FOTM_BL_Sub_1)

# Univariate Bar Graph:

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(Age)))+ggtitle("Age")

# Univariate Histogram: 

  # Age:

ggplot(FOTM_BL_Sub_1, aes(Age)) + geom_histogram(binwidth = 5) + ggtitle("Age Distribution") + ylab("Number of People")



