
################################################################################
# DATA MANAGEMENT 
################################################################################

# Variables: 
# Unique ID ("Loyalty card number")
# Age ("Age")
# Gender ("GENDER") "Man", "Woman", or "Genderqueer/non-binary, neither exclusively man nor woman"
# Race** ("RACE") American Indian or Alaska Native, Asian, Black or African American, 
# Native Hawaiian or Other Pacific Islander, White ore Caucasian, Oteher Race **Option to select multiple
# Hispanic ("HISPANIC") YES/NO
# Insurance ("INSURANCEYN") YES/NO
# Medicare ("MEDICARE") YES/NO
# Medicaid ("MEDICAID") YES/NO
# Employed ("EMPLOYED") YES/NO/NA
# Retired ("RETIRED") YES/NO/NA
# Disabled ("DISABLED") YES/NO
# Snap ("SNAP") YES/NO
# General Health ("GENHEALTH") "Poor" = 1, "Fair, or" = 2, "Good" = 3, "Very good" = 4, "Excellent" = 5
# Physical Health ("HEALTH_PHYS_0_TEXT" --> "HEALTH_PHYS") Number of days of poor physical health in past 30 days
# Mental Health ("HEALTH_MENTAL_0_TEXT" --> "HEALTH_MENTAL") Number of days of poor mental health in past 30 days
# During the past 12 months, were you ever hungry and didn't eat because you didn't have enough money? (HUNGRY) Y/N
# Did you ever eat less during the past 12 months to save money? (EAT_LESS) Y/N
# Did snap provide enough benefits for your family to last the entire month? (SNAP_ENOUGH) Y/N

################################################################################
# Load libraries needed 
################################################################################

library(ggplot2)
library(descr)
library(dplyr)
library(readxl)
library(tidyverse)
library(plyr)
library(poLCA)

################################################################################
# Make New Subset of Data Using Variables of Interest BASELINE
################################################################################

FOTM_BL_Sub <- FOTM_BL[,c("Loyalty card number","Age", "GENDER",
                          "RACE","HISPANIC","INSURANCEYN","MEDICARE",
                          "MEDICAID","EMPLOYED","RETIRED","DISABLED",
                          "SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT",
                          "HEALTH_MENTAL_0_TEXT","HUNGRY","EAT_LESS","SNAP_ENOUGH", "RACE_5_TEXT")]

FOTM_BL_Sub <- FOTM_BL_Sub[-1,]

################################################################################
# Make New Subset of Data Using Variables of Interest 6 MONTH
################################################################################

FOTM_6mo_Sub <- FOTM_6mo[,c("Loyalty card member","YOB","INSURANCE Y/N","MEDICARE",
                            "MEDICAID","EMPLOYED","RETIRED","DISABLED",
                            "SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT",
                            "HEALTH_MENTAL_0_TEXT","HUNGRY","EAT_LESS", "SNAP_ENOUGH")] 

names(FOTM_6mo_Sub)[names(FOTM_6mo_Sub)== "Loyalty card member"] <- "Loyalty card number"

################################################################################
# Make New Subset of Data Using Variables of Interest 12 MONTH
################################################################################

FOTM_12mo_Sub <- FOTM_12mo[,c("Loyalty card member","YOB","INSURANCEYN","MEDICARE",
                              "MEDICAID","EMPLOYED","RETIRED","DISABLED",
                              "SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT",
                              "HEALTH_MENTAL_0_TEXT","HUNGRY","EAT_LESS","SNAP_ENOUGH")]

FOTM_12mo_Sub <- FOTM_12mo_Sub[-1,]

names(FOTM_12mo_Sub)[names(FOTM_12mo_Sub)== "Loyalty card member"] <- "Loyalty card number"

################################################################################
# Renaming Variables
################################################################################

names(FOTM_BL_Sub)[names(FOTM_BL_Sub)== "HEALTH_PHYS_0_TEXT"] <- "HEALTH_PHYS"
names(FOTM_BL_Sub)[names(FOTM_BL_Sub)== "HEALTH_MENTAL_0_TEXT"] <- "HEALTH_MENTAL"


names(FOTM_6mo_Sub)[names(FOTM_6mo_Sub)== "HEALTH_PHYS_0_TEXT"] <- "HEALTH_PHYS"
names(FOTM_6mo_Sub)[names(FOTM_6mo_Sub)== "HEALTH_MENTAL_0_TEXT"] <- "HEALTH_MENTAL"


names(FOTM_12mo_Sub)[names(FOTM_12mo_Sub)== "HEALTH_PHYS_0_TEXT"] <- "HEALTH_PHYS"
names(FOTM_12mo_Sub)[names(FOTM_12mo_Sub)== "HEALTH_MENTAL_0_TEXT"] <- "HEALTH_MENTAL"

################################################################################
# Make Employment Variable
################################################################################

FOTM_BL_Sub$Employment[FOTM_BL_Sub$RETIRED == "YES" & FOTM_BL_Sub$EMPLOYED == "NO"] <- "Retired"
FOTM_BL_Sub$Employment[FOTM_BL_Sub$EMPLOYED == "YES" & FOTM_BL_Sub$RETIRED == "NO"] <- "Employed"
FOTM_BL_Sub$Employment[FOTM_BL_Sub$RETIRED == "NO" & FOTM_BL_Sub$EMPLOYED == "NO"] <- "Unemployed"
FOTM_BL_Sub$Employment[FOTM_BL_Sub$RETIRED == "DON'T KNOW" & FOTM_BL_Sub$EMPLOYED == "NO"] <- NA


FOTM_6mo_Sub$Employment[FOTM_6mo_Sub$RETIRED == "YES" & FOTM_6mo_Sub$EMPLOYED == "NO"] <- "Retired"
FOTM_6mo_Sub$Employment[FOTM_6mo_Sub$EMPLOYED == "YES" & FOTM_6mo_Sub$RETIRED == "NO"] <- "Employed"
FOTM_6mo_Sub$Employment[FOTM_6mo_Sub$RETIRED == "NO" & FOTM_6mo_Sub$EMPLOYED == "NO"] <- "Unemployed"
FOTM_6mo_Sub$Employment[FOTM_6mo_Sub$RETIRED == "DON'T KNOW" & FOTM_6mo_Sub$EMPLOYED == "NO"] <- NA


FOTM_12mo_Sub$Employment[FOTM_12mo_Sub$RETIRED == "YES" & FOTM_12mo_Sub$EMPLOYED == "NO"] <- "Retired"
FOTM_12mo_Sub$Employment[FOTM_12mo_Sub$EMPLOYED == "YES" & FOTM_12mo_Sub$RETIRED == "NO"] <- "Employed"
FOTM_12mo_Sub$Employment[FOTM_12mo_Sub$RETIRED == "NO" & FOTM_12mo_Sub$EMPLOYED == "NO"] <- "Unemployed"
FOTM_12mo_Sub$Employment[FOTM_12mo_Sub$RETIRED == "DON'T KNOW" & FOTM_12mo_Sub$EMPLOYED == "NO"] <- NA

################################################################################
# Age Categories
################################################################################

FOTM_BL_Sub$Age_Groups[FOTM_BL_Sub$Age >= 26 & FOTM_BL_Sub$Age < 63] <- "26-63 Years"
FOTM_BL_Sub$Age_Groups[FOTM_BL_Sub$Age >= 63 & FOTM_BL_Sub$Age < 71] <- "63-71 Years"
FOTM_BL_Sub$Age_Groups[FOTM_BL_Sub$Age >= 71 & FOTM_BL_Sub$Age < 78] <- "71-78 Years"
FOTM_BL_Sub$Age_Groups[FOTM_BL_Sub$Age >= 78] <- "Over 78"

################################################################################

FOTM_BL_Sub$GENHEALTH[FOTM_BL_Sub$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_BL_Sub$GENHEALTH[FOTM_BL_Sub$GENHEALTH == "DON'T KNOW"] <- NA


FOTM_6mo_Sub$GENHEALTH[FOTM_6mo_Sub$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_6mo_Sub$GENHEALTH[FOTM_6mo_Sub$GENHEALTH == "DON'T KNOW"] <- NA


FOTM_12mo_Sub$GENHEALTH[FOTM_12mo_Sub$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_12mo_Sub$GENHEALTH[FOTM_12mo_Sub$GENHEALTH == "DON'T KNOW"] <- NA

################################################################################

FOTM_BL_Sub$HUNGRY[FOTM_BL_Sub$HUNGRY == "DON'T KNOW"|FOTM_BL_Sub$HUNGRY == "REFUSED"] <- NA

FOTM_6mo_Sub$HUNGRY[FOTM_6mo_Sub$HUNGRY == "DON'T KNOW"|FOTM_6mo_Sub$HUNGRY == "REFUSED"] <- NA

FOTM_12mo_Sub$HUNGRY[FOTM_12mo_Sub$HUNGRY == "DON'T KNOW"|FOTM_12mo_Sub$HUNGRY == "REFUSED"] <- NA

################################################################################

FOTM_BL_Sub$Health_Phys_Weeks[FOTM_BL_Sub$HEALTH_PHYS==0]<-"None"
FOTM_BL_Sub$Health_Phys_Weeks[FOTM_BL_Sub$HEALTH_PHYS>=1 & FOTM_BL_Sub$HEALTH_PHYS<8]<-"One Week"
FOTM_BL_Sub$Health_Phys_Weeks[FOTM_BL_Sub$HEALTH_PHYS>=8 & FOTM_BL_Sub$HEALTH_PHYS<30]<-"More Than One Week"
FOTM_BL_Sub$Health_Phys_Weeks[FOTM_BL_Sub$HEALTH_PHYS==30]<-"Every Day"


FOTM_6mo_Sub$Health_Phys_Weeks[FOTM_6mo_Sub$HEALTH_PHYS==0]<-"None"
FOTM_6mo_Sub$Health_Phys_Weeks[FOTM_6mo_Sub$HEALTH_PHYS>=1 & FOTM_6mo_Sub$HEALTH_PHYS<8]<-"One Week"
FOTM_6mo_Sub$Health_Phys_Weeks[FOTM_6mo_Sub$HEALTH_PHYS>=8 & FOTM_6mo_Sub$HEALTH_PHYS<30]<-"More Than One Week"
FOTM_6mo_Sub$Health_Phys_Weeks[FOTM_6mo_Sub$HEALTH_PHYS==30]<-"Every Day"


FOTM_12mo_Sub$Health_Phys_Weeks[FOTM_12mo_Sub$HEALTH_PHYS==0]<-"None"
FOTM_12mo_Sub$Health_Phys_Weeks[FOTM_12mo_Sub$HEALTH_PHYS>=1 & FOTM_12mo_Sub$HEALTH_PHYS<8]<-"One Week"
FOTM_12mo_Sub$Health_Phys_Weeks[FOTM_12mo_Sub$HEALTH_PHYS>=8 & FOTM_12mo_Sub$HEALTH_PHYS<30]<-"More Than One Week"
FOTM_12mo_Sub$Health_Phys_Weeks[FOTM_12mo_Sub$HEALTH_PHYS==30]<-"Every Day"

################################################################################

FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL==0]<-"None"
FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL>=1 & FOTM_BL_Sub$HEALTH_MENTAL<8]<-"One Week"
FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL>=8 & FOTM_BL_Sub$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL==30]<-"Every Day"
FOTM_BL_Sub$Health_Ment_Weeks <- as.factor(FOTM_BL_Sub$Health_Ment_Weeks)

FOTM_BL_Sub$Health_Ment_Weeks <- as.numeric(as.character(FOTM_BL_Sub$Health_Ment_Weeks))

freq(FOTM_BL_Sub$Health_Ment_Weeks)

FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL==0]<-"None"
FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL>=1 & FOTM_6mo_Sub$HEALTH_MENTAL<8]<-"One Week"
FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL>=8 & FOTM_6mo_Sub$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL==30]<-"Every Day"


FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL==0]<-"None"
FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL>=1 & FOTM_12mo_Sub$HEALTH_MENTAL<8]<-"One Week"
FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL>=8 & FOTM_12mo_Sub$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL==30]<-"Every Day"

################################################################################

FOTM_BL_Sub$HISPANIC[FOTM_BL_Sub$HISPANIC == "NO"] <- "No"
FOTM_BL_Sub$HISPANIC[FOTM_BL_Sub$HISPANIC == "YES"] <- "Yes"
FOTM_BL_Sub$HISPANIC[FOTM_BL_Sub$HISPANIC == "DON'T KNOW"] <- "Don't Know"

# No Hispanic variable for 6 or 12 month #

################################################################################

FOTM_BL_Sub$DISABLED[FOTM_BL_Sub$DISABLED == "DON'T KNOW"] <- NA
FOTM_BL_Sub$DISABLED[FOTM_BL_Sub$DISABLED == "REFUSED"] <- NA


FOTM_6mo_Sub$DISABLED[FOTM_6mo_Sub$DISABLED == "DON'T KNOW"] <- NA
FOTM_6mo_Sub$DISABLED[FOTM_6mo_Sub$DISABLED == "REFUSED"] <- NA


FOTM_12mo_Sub$DISABLED[FOTM_12mo_Sub$DISABLED == "DON'T KNOW"] <- NA
FOTM_12mo_Sub$DISABLED[FOTM_12mo_Sub$DISABLED == "REFUSED"] <- NA

################################################################################

FOTM_BL_Sub$SNAP[FOTM_BL_Sub$SNAP == "DON'T KNOW"] <- NA

FOTM_6mo_Sub$SNAP[FOTM_6mo_Sub$SNAP == "DON'T KNOW"] <- NA

FOTM_12mo_Sub$SNAP[FOTM_12mo_Sub$SNAP == "DON'T KNOW"] <- NA

################################################################################

FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "MAN"] <- "Man"
FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "WOMAN"] <- "Woman"
FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "REFUSED"] <- NA
FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "Genderqueer/non-binary, neither exclusively man nor woman"] <- NA

# No Gender variable for 6 or 12 month #

################################################################################

FOTM_BL_Sub[FOTM_BL_Sub$RACE == "RACE_OTHER", "RACE"] <- FOTM_BL_Sub[FOTM_BL_Sub$RACE == "RACE_OTHER", "RACE_5_TEXT"]
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_WHITE"] <- "White"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_BLACK"] <- "Black"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_AMERICAN_INDIAN"] <- "American Indian"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_DK"] <- NA
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "dominican/puerto rican"] <- "Dominican"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "dominicano"] <- "Dominican"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "hispanic"] <- "Hispanic"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "hispanic/puerto rican"] <- "Hispanic"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "hispano"] <- "Hispanic"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "latina"] <- "Latina"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "latina/hispanic"] <- "Latina"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "latino"] <- "Latina"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "Latino"] <- "Latina"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "n/a"] <- NA
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "puerto rican"] <- "Puerto Rican"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_REF"] <- NA
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_ASIAN"] <- "Asian"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK"|FOTM_BL_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK,RACE_WHITE"|FOTM_BL_Sub$RACE == 
                   "RACE_AMERICAN_INDIAN,RACE_OTHER"|FOTM_BL_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_WHITE"] <- "American Indian"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_BLACK,RACE_OTHER"|FOTM_BL_Sub$RACE == "RACE_BLACK,RACE_WHITE"] <- "Black"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_NATHAWOROTH"] <- "Nathaworoth"
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_WHITE, RACE_OTHER"|FOTM_BL_Sub$RACE == "RACE_WHITE,RACE_DK"|FOTM_BL_Sub$RACE == "RACE_WHITE,RACE_OTHER"] <- "White"
FOTM_BL_Sub <- FOTM_BL_Sub[,-12]
FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "American"|FOTM_BL_Sub$RACE == "Asian"|FOTM_BL_Sub$RACE == "Boricua"|
                   FOTM_BL_Sub$RACE == "Cape Verdan"|FOTM_BL_Sub$RACE == "Cape Verdean, Portuguese, Italian"|
                   FOTM_BL_Sub$RACE == "Columbian"|FOTM_BL_Sub$RACE == "French Indian, Italian"|
                   FOTM_BL_Sub$RACE == "Haitian"|FOTM_BL_Sub$RACE == "Jewish"|FOTM_BL_Sub$RACE == "Mestizo"|
                   FOTM_BL_Sub$RACE == "Nathaworoth"|FOTM_BL_Sub$RACE == "Nigerian"|FOTM_BL_Sub$RACE == "Portuguese"|
                   FOTM_BL_Sub$RACE == "Sudamericana"] <- "Other"

################################################################################
# Merging Datasets
################################################################################

FOTM_BL_Sub$`Loyalty card number` <- as.numeric(as.character(FOTM_BL_Sub$`Loyalty card number`))

FOTM_12mo_Sub$`Loyalty card number` <- as.numeric(as.character(FOTM_12mo_Sub$`Loyalty card number`))

alldata <- FOTM_BL_Sub %>%
  left_join(FOTM_6mo_Sub, by='Loyalty card number') %>%
  left_join(FOTM_12mo_Sub, by='Loyalty card number')

alldata_Sub <- alldata[,c("Loyalty card number","Age","HISPANIC","GENDER","RACE",
                          "GENHEALTH","GENHEALTH.x","GENHEALTH.y","YOB.x",
                          "Employment","Employment.x","Employment.y","Health_Ment_Weeks",
                          "Health_Ment_Weeks.x","Health_Ment_Weeks.y","Health_Phys_Weeks",
                          "Health_Phys_Weeks.x","Health_Phys_Weeks.y")]

col_order <- c("Loyalty card number","GENDER","HISPANIC","RACE","Age","YOB.x",
               "GENHEALTH.x","GENHEALTH.y","GENHEALTH",
               "Employment.x","Employment.y","Employment","Health_Ment_Weeks.x",
               "Health_Ment_Weeks.y","Health_Ment_Weeks","Health_Phys_Weeks.x",
               "Health_Phys_Weeks.y","Health_Phys_Weeks")

alldata_Sub <- alldata_Sub[, col_order]

names(alldata_Sub)[names(alldata_Sub)== "YOB.x"] <- "YOB"

names(alldata_Sub)[names(alldata_Sub)== "GENHEALTH.x"] <- "GENHEALTH_BL"
names(alldata_Sub)[names(alldata_Sub)== "GENHEALTH.y"] <- "GENHEALTH_6mo"
names(alldata_Sub)[names(alldata_Sub)== "GENHEALTH"] <- "GENHEALTH_12mo"

names(alldata_Sub)[names(alldata_Sub)== "Employment.x"] <- "Employment_BL"
names(alldata_Sub)[names(alldata_Sub)== "Employment.y"] <- "Employment_6mo"
names(alldata_Sub)[names(alldata_Sub)== "Employment"] <- "Employment_12mo"

names(alldata_Sub)[names(alldata_Sub)== "Health_Ment_Weeks.x"] <- "Health_Ment_Weeks_BL"
names(alldata_Sub)[names(alldata_Sub)== "Health_Ment_Weeks.y"] <- "Health_Ment_Weeks_6mo"
names(alldata_Sub)[names(alldata_Sub)== "Health_Ment_Weeks"] <- "Health_Ment_Weeks_12mo"

names(alldata_Sub)[names(alldata_Sub)== "Health_Phys_Weeks.x"] <- "Health_Phys_Weeks_BL"
names(alldata_Sub)[names(alldata_Sub)== "Health_Phys_Weeks.y"] <- "Health_Phys_Weeks_6mo"
names(alldata_Sub)[names(alldata_Sub)== "Health_Phys_Weeks"] <- "Health_Phys_Weeks_12mo"

################################################################################

alldata_Sub_graphs <- alldata_Sub[,c("Loyalty card number","GENDER","HISPANIC","RACE","Age","YOB",
                                     "GENHEALTH_BL","GENHEALTH_6mo","GENHEALTH_12mo",
                                     "Employment_BL","Employment_6mo","Employment_12mo","Health_Ment_Weeks_BL",
                                     "Health_Ment_Weeks_6mo","Health_Ment_Weeks_12mo","Health_Phys_Weeks_BL",
                                     "Health_Phys_Weeks_6mo","Health_Phys_Weeks_12mo")]

# Simple linear regression if explanatory variable is quantitative

race.lm <- lm(GENHEALTH_BL ~ RACE, data = alldata_Sub) 
summary(race.lm)

