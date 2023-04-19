
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
# Make New Subset of Data Using Variables of Interest BASELINE
################################################################################

FOTM_BL_Sub <- FOTM_BL[,c("Loyalty card number","Age", "GENDER",
                                         "RACE","HISPANIC","INSURANCEYN","MEDICARE",
                                         "MEDICAID","EMPLOYED","RETIRED","DISABLED",
                                         "SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT",
                                         "HEALTH_MENTAL_0_TEXT","HUNGRY","EAT_LESS","SNAP_ENOUGH", "RACE_5_TEXT")]
View(FOTM_BL_Sub)

################################################################################
# Make New Subset of Data Using Variables of Interest 6 MONTH
################################################################################

FOTM_6mo_Sub <- FOTM_6mo[,c("Loyalty card number","Age", "GENDER",
                                         "RACE","HISPANIC","INSURANCEYN","MEDICARE",
                                         "MEDICAID","EMPLOYED","RETIRED","DISABLED",
                                         "SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT",
                                         "HEALTH_MENTAL_0_TEXT","HUNGRY","EAT_LESS","SNAP_ENOUGH")]
View(FOTM_6mo_Sub)

################################################################################
# Make New Subset of Data Using Variables of Interest 12 MONTH
################################################################################

FOTM_12mo_Sub <- FOTM_12mo[,c("Loyalty card number","Age", "GENDER",
                                         "RACE","HISPANIC","INSURANCEYN","MEDICARE",
                                         "MEDICAID","EMPLOYED","RETIRED","DISABLED",
                                         "SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT",
                                         "HEALTH_MENTAL_0_TEXT","HUNGRY","EAT_LESS","SNAP_ENOUGH")]
View(FOTM_12mo_Sub)

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


FOTM_6mo_Sub$Age_Groups[FOTM_6mo_Sub$Age >= 26 & FOTM_6mo_Sub$Age < 63] <- "26-63 Years"
FOTM_6mo_Sub$Age_Groups[FOTM_6mo_Sub$Age >= 63 & FOTM_6mo_Sub$Age < 71] <- "63-71 Years"
FOTM_6mo_Sub$Age_Groups[FOTM_6mo_Sub$Age >= 71 & FOTM_6mo_Sub$Age < 78] <- "71-78 Years"
FOTM_6mo_Sub$Age_Groups[FOTM_6mo_Sub$Age >= 78] <- "Over 78"


FOTM_12mo_Sub$Age_Groups[FOTM_12mo_Sub$Age >= 26 & FOTM_12mo_Sub$Age < 63] <- "26-63 Years"
FOTM_12mo_Sub$Age_Groups[FOTM_12mo_Sub$Age >= 63 & FOTM_12mo_Sub$Age < 71] <- "63-71 Years"
FOTM_12mo_Sub$Age_Groups[FOTM_12mo_Sub$Age >= 71 & FOTM_12mo_Sub$Age < 78] <- "71-78 Years"
FOTM_12mo_Sub$Age_Groups[FOTM_12mo_Sub$Age >= 78] <- "Over 78"


FOTM_BL_Sub$Age_Groups <-revalue(FOTM_BL_Sub$Age_Groups, c("26-63 Years"="1", "63-71 Years"="2", "71-78 Years"="3", "Over 78"="4"))

FOTM_6mo_Sub$Age_Groups <-revalue(FOTM_6mo_Sub$Age_Groups, c("26-63 Years"="1", "63-71 Years"="2", "71-78 Years"="3", "Over 78"="4"))

FOTM_12mo_Sub$Age_Groups <-revalue(FOTM_12mo_Sub$Age_Groups, c("26-63 Years"="1", "63-71 Years"="2", "71-78 Years"="3", "Over 78"="4"))


################################################################################


FOTM_BL_Sub$GENHEALTH[FOTM_BL_Sub$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_BL_Sub$GENHEALTH[FOTM_BL_Sub$GENHEALTH == "DON'T KNOW"] <- NA


FOTM_6mo_Sub$GENHEALTH[FOTM_6mo_Sub$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_6mo_Sub$GENHEALTH[FOTM_6mo_Sub$GENHEALTH == "DON'T KNOW"] <- NA


FOTM_12mo_Sub$GENHEALTH[FOTM_12mo_Sub$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_12mo_Sub$GENHEALTH[FOTM_12mo_Sub$GENHEALTH == "DON'T KNOW"] <- NA


FOTM_BL_Sub_Sub$GENHEALTH<-revalue(FOTM_BL_Sub$GENHEALTH, c("Poor"="1", "Fair"="2", "Good"="3", "Very good"="4", "Excellent"="5"))

FOTM_6mo_Sub$GENHEALTH<-revalue(FOTM_6mo_Sub$GENHEALTH, c("Poor"="1", "Fair"="2", "Good"="3", "Very good"="4", "Excellent"="5"))

FOTM_12mo_Sub$GENHEALTH<-revalue(FOTM_12mo_Sub$GENHEALTH, c("Poor"="1", "Fair"="2", "Good"="3", "Very good"="4", "Excellent"="5"))


################################################################################

LCA_subset$HUNGRY[LCA_subset$HUNGRY == "DON'T KNOW"|LCA_subset$HUNGRY == "REFUSED"] <- NA

FOTM_BL_Sub$HUNGRY<-revalue(FOTM_BL_Sub$HUNGRY, c("NO"="1", "YES"="2"))

FOTM_6mo_Sub$HUNGRY<-revalue(FOTM_6mo_Sub$HUNGRY, c("NO"="1", "YES"="2"))

FOTM_12mo_Sub$HUNGRY<-revalue(FOTM_12mo_Sub$HUNGRY, c("NO"="1", "YES"="2"))

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

FOTM_BL_Sub$Health_Phys_Weeks<-revalue(FOTM_BL_Sub$Health_Phys_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))

FOTM_BL_Sub$Health_Phys_Weeks<-revalue(FOTM_BL_Sub$Health_Phys_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))

################################################################################

FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL==0]<-"None"
FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL>=1 & FOTM_BL_Sub$HEALTH_MENTAL<8]<-"One Week"
FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL>=8 & FOTM_BL_Sub$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_BL_Sub$Health_Ment_Weeks[FOTM_BL_Sub$HEALTH_MENTAL==30]<-"Every Day"
FOTM_BL_Sub$Health_Ment_Weeks <- as.factor(FOTM_BL_Sub$Health_Ment_Weeks)


FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL==0]<-"None"
FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL>=1 & FOTM_6mo_Sub$HEALTH_MENTAL<8]<-"One Week"
FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL>=8 & FOTM_6mo_Sub$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_6mo_Sub$Health_Ment_Weeks[FOTM_6mo_Sub$HEALTH_MENTAL==30]<-"Every Day"
FOTM_6mo_Sub$Health_Ment_Weeks <- as.factor(FOTM_6mo_Sub$Health_Ment_Weeks)


FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL==0]<-"None"
FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL>=1 & FOTM_12mo_Sub$HEALTH_MENTAL<8]<-"One Week"
FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL>=8 & FOTM_12mo_Sub$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_12mo_Sub$Health_Ment_Weeks[FOTM_12mo_Sub$HEALTH_MENTAL==30]<-"Every Day"
FOTM_12mo_Sub$Health_Ment_Weeks <- as.factor(FOTM_12mo_Sub$Health_Ment_Weeks)

FOTM_BL_Sub$Health_Ment_Weeks<-revalue(FOTM_BL_Sub$Health_Ment_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))

FOTM_6mo_Sub$Health_Ment_Weeks<-revalue(FOTM_6mo_Sub$Health_Ment_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))

FOTM_12mo_Sub$Health_Ment_Weeks<-revalue(FOTM_12mo_Sub$Health_Ment_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))

################################################################################


FOTM_BL_Sub$HISPANIC[FOTM_BL_Sub$HISPANIC == "NO"] <- "No"
FOTM_BL_Sub$HISPANIC[FOTM_BL_Sub$HISPANIC == "YES"] <- "Yes"
FOTM_BL_Sub$HISPANIC[FOTM_BL_Sub$HISPANIC == "DON'T KNOW"] <- "Don't Know"

FOTM_6mo_Sub$HISPANIC[FOTM_6mo_Sub$HISPANIC == "NO"] <- "No"
FOTM_6mo_Sub$HISPANIC[FOTM_6mo_Sub$HISPANIC == "YES"] <- "Yes"
FOTM_6mo_Sub$HISPANIC[FOTM_6mo_Sub$HISPANIC == "DON'T KNOW"] <- "Don't Know"


FOTM_12mo_Sub$HISPANIC[FOTM_12mo_Sub$HISPANIC == "NO"] <- "No"
FOTM_12mo_Sub$HISPANIC[FOTM_12mo_Sub$HISPANIC == "YES"] <- "Yes"
FOTM_12mo_Sub$HISPANIC[FOTM_12mo_Sub$HISPANIC == "DON'T KNOW"] <- "Don't Know"

FOTM_BL_Sub$HISPANIC<-revalue(FOTM_BL_Sub$HISPANIC, c("NO"="1", "YES"="2"))


################################################################################


FOTM_BL_Sub$DISABLED[FOTM_BL_Sub$DISABLED == "DON'T KNOW"] <- NA
FOTM_BL_Sub$DISABLED[FOTM_BL_Sub$DISABLED == "REFUSED"] <- NA


FOTM_6mo_Sub$DISABLED[FOTM_6mo_Sub$DISABLED == "DON'T KNOW"] <- NA
FOTM_6mo_Sub$DISABLED[FOTM_6mo_Sub$DISABLED == "REFUSED"] <- NA


FOTM_12mo_Sub$DISABLED[FOTM_12mo_Sub$DISABLED == "DON'T KNOW"] <- NA
FOTM_12mo_Sub$DISABLED[FOTM_12mo_Sub$DISABLED == "REFUSED"] <- NA

FOTM_BL_Sub$DISABLED<-revalue(FOTM_BL_Sub$DISABLED, c("NO"="1", "YES"="2"))

################################################################################

FOTM_BL_Sub$SNAP[FOTM_BL_Sub$SNAP == "DON'T KNOW"] <- NA

FOTM_6mo_Sub$SNAP[FOTM_6mo_Sub$SNAP == "DON'T KNOW"] <- NA

FOTM_12mo_Sub$SNAP[FOTM_12mo_Sub$SNAP == "DON'T KNOW"] <- NA

FOTM_BL_Sub$SNAP<-revalue(FOTM_BL_Sub$SNAP, c("NO"="1", "YES"="2"))


FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "MAN"] <- "Man"
FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "WOMAN"] <- "Woman"
FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "REFUSED"] <- NA
FOTM_BL_Sub$GENDER[FOTM_BL_Sub$GENDER == "Genderqueer/non-binary, neither exclusively man nor woman"] <- NA

FOTM_6mo_Sub$GENDER[FOTM_6mo_Sub$GENDER == "MAN"] <- "Man"
FOTM_6mo_Sub$GENDER[FOTM_6mo_Sub$GENDER == "WOMAN"] <- "Woman"
FOTM_6mo_Sub$GENDER[FOTM_6mo_Sub$GENDER == "REFUSED"] <- NA
FOTM_6mo_Sub$GENDER[FOTM_6mo_Sub$GENDER == "Genderqueer/non-binary, neither exclusively man nor woman"] <- NA

FOTM_12mo_Sub$GENDER[FOTM_12mo_Sub$GENDER == "MAN"] <- "Man"
FOTM_12mo_Sub$GENDER[FOTM_12mo_Sub$GENDER == "WOMAN"] <- "Woman"
FOTM_12mo_Sub$GENDER[FOTM_12mo_Sub$GENDER == "REFUSED"] <- NA
FOTM_12mo_Sub$GENDER[FOTM_12mo_Sub$GENDER == "Genderqueer/non-binary, neither exclusively man nor woman"] <- NA

FOTM_BL_Sub$GENDER<-revalue(FOTM_BL_Sub$GENDER, c("Male"="1", "Woman"="2", "Genderqueer"="3"))



# RACE # 

FOTM_BL_Sub$RACE[FOTM_BL_Sub$RACE == "RACE_OTHER"] <- "Other"
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

freq(FOTM_BL_Sub$RACE, plot=F)


FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_OTHER"] <- "Other"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_WHITE"] <- "White"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_BLACK"] <- "Black"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_AMERICAN_INDIAN"] <- "American Indian"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_DK"] <- NA
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "dominican/puerto rican"] <- "Dominican"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "dominicano"] <- "Dominican"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "hispanic"] <- "Hispanic"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "hispanic/puerto rican"] <- "Hispanic"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "hispano"] <- "Hispanic"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "latina"] <- "Latina"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "latina/hispanic"] <- "Latina"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "latino"] <- "Latina"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "Latino"] <- "Latina"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "n/a"] <- NA
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "puerto rican"] <- "Puerto Rican"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_REF"] <- NA
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_ASIAN"] <- "Asian"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK"|FOTM_6mo_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK,RACE_WHITE"|FOTM_6mo_Sub$RACE == 
                   "RACE_AMERICAN_INDIAN,RACE_OTHER"|FOTM_6mo_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_WHITE"] <- "American Indian"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_BLACK,RACE_OTHER"|FOTM_6mo_Sub$RACE == "RACE_BLACK,RACE_WHITE"] <- "Black"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_NATHAWOROTH"] <- "Nathaworoth"
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "RACE_WHITE, RACE_OTHER"|FOTM_6mo_Sub$RACE == "RACE_WHITE,RACE_DK"|FOTM_6mo_Sub$RACE == "RACE_WHITE,RACE_OTHER"] <- "White"
FOTM_6mo_Sub <- FOTM_6mo_Sub[,-12]
FOTM_6mo_Sub$RACE[FOTM_6mo_Sub$RACE == "American"|FOTM_6mo_Sub$RACE == "Asian"|FOTM_6mo_Sub$RACE == "Boricua"|
                   FOTM_6mo_Sub$RACE == "Cape Verdan"|FOTM_6mo_Sub$RACE == "Cape Verdean, Portuguese, Italian"|
                   FOTM_6mo_Sub$RACE == "Columbian"|FOTM_6mo_Sub$RACE == "French Indian, Italian"|
                   FOTM_6mo_Sub$RACE == "Haitian"|FOTM_6mo_Sub$RACE == "Jewish"|FOTM_6mo_Sub$RACE == "Mestizo"|
                   FOTM_6mo_Sub$RACE == "Nathaworoth"|FOTM_6mo_Sub$RACE == "Nigerian"|FOTM_6mo_Sub$RACE == "Portuguese"|
                   FOTM_6mo_Sub$RACE == "Sudamericana"] <- "Other"


FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_OTHER"] <- "Other"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_WHITE"] <- "White"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_BLACK"] <- "Black"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_AMERICAN_INDIAN"] <- "American Indian"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_DK"] <- NA
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "dominican/puerto rican"] <- "Dominican"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "dominicano"] <- "Dominican"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "hispanic"] <- "Hispanic"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "hispanic/puerto rican"] <- "Hispanic"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "hispano"] <- "Hispanic"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "latina"] <- "Latina"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "latina/hispanic"] <- "Latina"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "latino"] <- "Latina"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "Latino"] <- "Latina"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "n/a"] <- NA
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "puerto rican"] <- "Puerto Rican"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_REF"] <- NA
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_ASIAN"] <- "Asian"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK"|FOTM_12mo_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK,RACE_WHITE"|FOTM_12mo_Sub$RACE == 
                   "RACE_AMERICAN_INDIAN,RACE_OTHER"|FOTM_12mo_Sub$RACE == "RACE_AMERICAN_INDIAN,RACE_WHITE"] <- "American Indian"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_BLACK,RACE_OTHER"|FOTM_12mo_Sub$RACE == "RACE_BLACK,RACE_WHITE"] <- "Black"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_NATHAWOROTH"] <- "Nathaworoth"
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "RACE_WHITE, RACE_OTHER"|FOTM_12mo_Sub$RACE == "RACE_WHITE,RACE_DK"|FOTM_12mo_Sub$RACE == "RACE_WHITE,RACE_OTHER"] <- "White"
FOTM_12mo_Sub <- FOTM_12mo_Sub[,-12]
FOTM_12mo_Sub$RACE[FOTM_12mo_Sub$RACE == "American"|FOTM_12mo_Sub$RACE == "Asian"|FOTM_12mo_Sub$RACE == "Boricua"|
                   FOTM_12mo_Sub$RACE == "Cape Verdan"|FOTM_12mo_Sub$RACE == "Cape Verdean, Portuguese, Italian"|
                   FOTM_12mo_Sub$RACE == "Columbian"|FOTM_12mo_Sub$RACE == "French Indian, Italian"|
                   FOTM_12mo_Sub$RACE == "Haitian"|FOTM_12mo_Sub$RACE == "Jewish"|FOTM_12mo_Sub$RACE == "Mestizo"|
                   FOTM_12mo_Sub$RACE == "Nathaworoth"|FOTM_12mo_Sub$RACE == "Nigerian"|FOTM_12mo_Sub$RACE == "Portuguese"|
                   FOTM_12mo_Sub$RACE == "Sudamericana"] <- "Other"


FOTM_BL_Sub$RACE<-revalue(FOTM_BL_Sub$RACE, c("American Indian"="1", "Black"="2",
                                            "Cape Verdan"="3", "Dominican"="4", "Hispanic"="5",
                                            "Latina"="6", "Puerto Rican"="7", "Spanish"="8", "White"="9",
                                            "Other"="10"))

################################################################################
# Recode response values to numbers starting at "1" (It's a poLCA thing)
################################################################################



















