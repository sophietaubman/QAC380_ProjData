################################################################################
# Uploading Data Set 
################################################################################
library(readxl)
library(descr)
library(ggplot2)
library(tidyverse)

MASTER_CLEAN_DATA_FOTM <- read_excel("/Volumes/qac380/Data and Codebooks 2023/RIPHI/FOTM Data/MASTER CLEAN DATA FOTM.xlsx")
View(MASTER_CLEAN_DATA_FOTM)

###############################################################################
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
# Physical Health ("HEALTH_PHYS") Number of days of poor physical health in past 30 days
# Mental Health ("HEALTH_MENTAL") Number of days of poor mental health in past 30 days

################################################################################
# Make New Subset of Data Using Variables of Interest
################################################################################

FOTM_BL_Sub <- MASTER_CLEAN_DATA_FOTM[,c("Loyalty card number","Age", "GENDER",
                                         "RACE","HISPANIC","INSURANCEYN","MEDICARE",
                                         "MEDICAID","EMPLOYED","RETIRED","DISABLED",
                                         "SNAP","GENHEALTH","HEALTH_PHYS_0_TEXT",
                                         "HEALTH_MENTAL_0_TEXT")]
View(FOTM_BL_Sub)

################################################################################
# Renaming Variables
################################################################################

names(FOTM_BL_Sub)[names(FOTM_BL_Sub)== "HEALTH_PHYS_0_TEXT"] <- "HEALTH_PHYS"
names(FOTM_BL_Sub)[names(FOTM_BL_Sub)== "HEALTH_MENTAL_0_TEXT"] <- "HEALTH_MENTAL"

################################################################################
# Labeling Retirement and Employment as NA
################################################################################

FOTM_BL_Sub$EMPLOYED[FOTM_BL_Sub$RETIRED == "YES" & FOTM_BL_Sub$EMPLOYED == "NO"] <- NA
FOTM_BL_Sub$RETIRED[FOTM_BL_Sub$EMPLOYED == "YES" & FOTM_BL_Sub$RETIRED == "NO"] <- NA

################################################################################
# Deleting a Row
################################################################################

FOTM_BL_Sub_1 <- FOTM_BL_Sub[-1,]
View(FOTM_BL_Sub_1)

################################################################################

# Change Physical Health from Character Variable to Numeric
FOTM_BL_Sub_1$HEALTH_PHYS <- as.numeric(as.character(FOTM_BL_Sub_1$HEALTH_PHYS))

# Create Categories
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS==0]<-"None"
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS>=1 & FOTM_BL_Sub_1$HEALTH_PHYS<8]<-"One Week"
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS>=8 & FOTM_BL_Sub_1$HEALTH_PHYS<30]<-"More Than One Week"
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS==30]<-"Every Day"
FOTM_BL_Sub_1$Health_Phys_Weeks <- as.factor(FOTM_BL_Sub_1$Health_Phys_Weeks)


################################################################################

# MENTAL HEALTH #

# Change Mental Health from Character Variable to Numeric
FOTM_BL_Sub_1$HEALTH_MENTAL <- as.numeric(as.character(FOTM_BL_Sub_1$HEALTH_MENTAL))

# Create Categories
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL==0]<-"None"
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL>=1 & FOTM_BL_Sub_1$HEALTH_MENTAL<8]<-"One Week"
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL>=8 & FOTM_BL_Sub_1$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL==30]<-"Every Day"
FOTM_BL_Sub_1$Health_Ment_Weeks <- as.factor(FOTM_BL_Sub_1$Health_Ment_Weeks)


###############################################################################


# HISPANIC #

FOTM_BL_Sub_1$HISPANIC[FOTM_BL_Sub_1$HISPANIC == "NO"] <- "No"
FOTM_BL_Sub_1$HISPANIC[FOTM_BL_Sub_1$HISPANIC == "YES"] <- "Yes"
FOTM_BL_Sub_1$HISPANIC[FOTM_BL_Sub_1$HISPANIC == "DON'T KNOW"] <- "Don't Know"

################################################################################

# This needs to be edited for the sub_1 data set in order to allow for changing NA title #

FOTM_BL_Sub$EMPLOYED[FOTM_BL_Sub$RETIRED == "YES" & FOTM_BL_Sub$EMPLOYED == "NO"] <- NA
FOTM_BL_Sub$RETIRED[FOTM_BL_Sub$EMPLOYED == "YES" & FOTM_BL_Sub$RETIRED == "NO"] <- NA

# Changing labels #

FOTM_BL_Sub_1$RETIRED[FOTM_BL_Sub_1$RETIRED == "NO"] <- "Unemployed"
FOTM_BL_Sub_1$RETIRED[FOTM_BL_Sub_1$RETIRED == "YES"] <- "Retired"
FOTM_BL_Sub_1$RETIRED[FOTM_BL_Sub_1$RETIRED == "DON'T KNOW"] <- "Don't Know"

################################################################################

# DISABLED #

FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "DON'T KNOW"] <- NA
FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "REFUSED"] <- NA

################################################################################

# GENERAL HEALTH #

FOTM_BL_Sub_1$GENHEALTH[FOTM_BL_Sub_1$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_BL_Sub_1$GENHEALTH[FOTM_BL_Sub_1$GENHEALTH == "DON'T KNOW"] <- NA

#Latent Class Analysis with Covariates
library(plyr)
library(dplyr)
library(poLCA)

# DEAL WITH HUNGRY VARIABLE

#First, make sure all variables are factors and subset the data to include only LCA classification 
#variables and covariates

# subset the data set
lca_subset<-subset(FOTM_BL_Sub_1, select=c(Age, GENDER, RACE, HISPANIC, INSURANCEYN, MEDICARE,
                                           MEDICAID, RETIRED, DISABLED, SNAP, GENHEALTH, Health_Phys_Weeks, Health_Ment_Weeks))

# cleaning up variables for LCA

FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 26 & FOTM_BL_Sub_1$Age < 63] <- "26_63"
FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 63 & FOTM_BL_Sub_1$Age < 71] <- "63_71"
FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 71 & FOTM_BL_Sub_1$Age < 78] <- "71_78"
FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 78] <- "78up"

factor(FOTM_BL_Sub_1$Age)

FOTM_BL_Sub_1$GENDER[FOTM_BL_Sub_1$GENDER == "MAN"] <- "Man"
FOTM_BL_Sub_1$GENDER[FOTM_BL_Sub_1$GENDER == "WOMAN"] <- "Woman"

FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "REFUSED"] <- NA
FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "DON'T KNOW"] <- NA

FOTM_BL_Sub_1$SNAP[FOTM_BL_Sub_1$SNAP == "DON'T KNOW"] <- NA

FOTM_BL_Sub_1$GENHEALTH[FOTM_BL_Sub_1$GENHEALTH == "DON'T KNOW"] <- NA

FOTM_BL_Sub_1$MEDICARE[FOTM_BL_Sub_1$MEDICARE == "REFUSED"] <- NA

FOTM_BL_Sub_1$MEDICAID[FOTM_BL_Sub_1$MEDICAID == "REFUSED"] <- NA

# FOTM_BL_Sub_1$HUNGRY[FOTM_BL_Sub_1$HUNGRY == "DON'T KNOW"] <- NA


# recode response values to numbers starting at "1" (It's a poLCA thing)
lca_subset$Age<-revalue(lca_subset$Age, c("26_63"="1", "63_71"="2", "71_78"="3", "78up"="4"))
lca_subset$GENDER<-revalue(lca_subset$GENDER, 
                           c("Man"="1", "Woman"="2", "Genderqueer"="3")) 
lca_subset$RACE<-revalue(lca_subset$RACE, c("RACE_BLACK"="1", "RACE_WHITE"="2", "RACE_AMERICAN_INDIAN"="3", "RACE_ASIAN"="4", "RACE_OTHER"="5", "RACE_DK"="6"))
lca_subset$HISPANIC<-revalue(lca_subset$HISPANIC, c("No"="1", "Yes"="2"))
lca_subset$INSURANCEYN<-revalue(lca_subset$INSURANCEYN, c("YES"="1", "NO"="2", "DON'T KNOW"="3"))
lca_subset$MEDICARE<-revalue(lca_subset$MEDICARE, c("NO"="1", "YES"="2"))
lca_subset$MEDICAID<-revalue(lca_subset$MEDICAID, c("NO"="1", "YES"="2"))
lca_subset$RETIRED<-revalue(lca_subset$RETIRED, c("Retired"="2", "Unemployed"="1"))
lca_subset$DISABLED<-revalue(lca_subset$DISABLED, c("NO"="1", "YES"="2"))
lca_subset$SNAP<-revalue(lca_subset$SNAP, c("NO"="1", "YES"="2"))
lca_subset$GENHEALTH<-revalue(lca_subset$GENHEALTH, c("Poor"="1", "Fair"="2", "Good"="3", "Very good"="4", "Excellent"="5"))
lca_subset$Health_Phys_Weeks<-revalue(lca_subset$Health_Phys_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))
lca_subset$Health_Ment_Weeks<-revalue(lca_subset$Health_Ment_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))
# lca_subset$HUNGRY<-revalue(lca_subset$HUNGRY, c("NO"="1", "YES"="2"))

# make sure all variables are factors

# lca_subset$ethnic<-as.factor(lca_subset$ethnic)

lca_subset$Age<-as.factor(lca_subset$Age)
lca_subset$GENDER<-as.factor(lca_subset$GENDER)
lca_subset$RACE<-as.factor(lca_subset$RACE)
lca_subset$HISPANIC<-as.factor(lca_subset$HISPANIC)
lca_subset$INSURANCEYN<-as.factor(lca_subset$INSURANCEYN)
lca_subset$MEDICARE<-as.factor(lca_subset$MEDICARE)
lca_subset$MEDICAID<-as.factor(lca_subset$MEDICAID)
lca_subset$RETIRED<-as.factor(lca_subset$RETIRED)
lca_subset$DISABLED<-as.factor(lca_subset$DISABLED)
lca_subset$SNAP<-as.factor(lca_subset$SNAP)
lca_subset$GENHEALTH<-as.factor(lca_subset$GENHEALTH)
lca_subset$Health_Phys_Weeks<-as.factor(lca_subset$Health_Phys_Weeks)
lca_subset$Health_Ment_Weeks<-as.factor(lca_subset$Health_Ment_Weeks)
# lca_subset$HUNGRY<-as.factor(lca_subset$HUNGRY)


#Second, define the LCA formula. Variables in parentheses are the latent class classification variables. 
#Variables outside of the parentheses are covariates (not included in the LCA). 
#Finally, run the LCA specifying a range of classes
f <- cbind(Age, GENDER, RACE, HISPANIC, INSURANCEYN, MEDICARE, MEDICAID, RETIRED, DISABLED, 
           SNAP)~GENHEALTH+Health_Phys_Weeks+Health_Ment_Weeks

# latent class analysis specifying 1-3 classes
lCA1 <- poLCA(f,lca_subset, nclass=1,nrep=15) 
lCA2 <- poLCA(f,lca_subset, nclass=2,nrep=15, graphs = T)
lCA3 <- poLCA(f,lca_subset, nclass=3,nrep=15, graphs = T)
lCA4 <- poLCA(f,lca_subset, nclass=4,nrep=15, graphs = T)


# # Calculate entropy (3-class mode)l- values closer to 1.0 indicate greater separation of the classes.
# entropy<-function (p) sum(-p*log(p))
# error_prior <- entropy(LCA3$P) # Class proportions
# error_post <- mean(apply(LCA3$posterior, 1, entropy))
# LCA3_entropy <- (error_prior - error_post) / error_prior
# LCA3_entropy
# 
# #predicted class membership is in:
# LCA3$predclass[1:30]
# 
# #could be used as another variable (part of the data):
# lca_subset$class <- LCA3$predclass