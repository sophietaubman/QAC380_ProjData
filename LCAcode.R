################################################################################
# Uploading Data Set 
################################################################################
library(readxl)
library(descr)
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)
library(poLCA)

MASTER_CLEAN_DATA_FOTM <- read_excel("/Volumes/qac380/Data and Codebooks 2023/RIPHI/FOTM Data/MASTER CLEAN DATA FOTM.xlsx")
View(MASTER_CLEAN_DATA_FOTM)

###############################################################################
# Variables of Interest # 
################################################################################

# Unique ID ("Loyalty card number")
# Age ("Age")
# Insurance ("INSURANCEYN") YES/NO
# Disabled ("DISABLED") YES/NO
# Snap ("SNAP") YES/NO
# Gender ("GENDER")
# Hispanic ("HISPANIC") YES/NO
# Race ("RACE") 
# Employment Status ("RETIRED") Employed/Retired/Unemployed
# General Health ("GENHEALTH") "Poor" = 1, "Fair, or" = 2, "Good" = 3, "Very good" = 4, "Excellent" = 5
# Hunger ("HUNGRY") YES/NO

################################################################################
# Make New Subset Using Variables of Interest
################################################################################

# Loyalty Card Number # --> Code to 1 word for new subset

names(MASTER_CLEAN_DATA_FOTM)[names(MASTER_CLEAN_DATA_FOTM)== "Loyalty card number"] <- "LCN"

LCA_subset <- subset(MASTER_CLEAN_DATA_FOTM, select=c(LCN, Age, INSURANCEYN, DISABLED, SNAP, GENDER, HISPANIC, 
                                                      RACE, RETIRED, GENHEALTH, HUNGRY, RACE_5_TEXT)) 
View(LCA_subset)

LCA_subset <- LCA_subset[-1,]
View(LCA_subset)

################################################################################
# Cleaning-Up Variables for LCA #
################################################################################

# DISABLED #

LCA_subset$DISABLED[LCA_subset$DISABLED == "DON'T KNOW"] <- NA
LCA_subset$DISABLED[LCA_subset$DISABLED == "REFUSED"] <- NA

# GENERAL HEALTH #

LCA_subset$GENHEALTH[LCA_subset$GENHEALTH == "Fair, or"] <- "Fair"
LCA_subset$GENHEALTH[LCA_subset$GENHEALTH == "DON'T KNOW"] <- NA

# SNAP #

LCA_subset$SNAP[LCA_subset$SNAP == "DON'T KNOW"] <- NA

# AGE #

LCA_subset$Age[LCA_subset$Age >= 26 & LCA_subset$Age < 63] <- "26_63"
LCA_subset$Age[LCA_subset$Age >= 63 & LCA_subset$Age < 71] <- "63_71"
LCA_subset$Age[LCA_subset$Age >= 71 & LCA_subset$Age < 78] <- "71_78"
LCA_subset$Age[LCA_subset$Age >= 78] <- "78up"
factor(LCA_subset$Age)

# GENDER #

LCA_subset$GENDER[LCA_subset$GENDER == "MAN"] <- "Man"
LCA_subset$GENDER[LCA_subset$GENDER == "WOMAN"] <- "Woman"
LCA_subset$GENDER[LCA_subset$GENDER == "REFUSED"] <- NA
LCA_subset$GENDER[LCA_subset$GENDER == "Genderqueer/non-binary, neither exclusively man nor woman"] <- NA

# HISPANIC #

LCA_subset$HISPANIC[LCA_subset$HISPANIC == "Yes"] <- "YES"
LCA_subset$HISPANIC[LCA_subset$HISPANIC == "No"] <- "NO"
LCA_subset$HISPANIC[LCA_subset$HISPANIC == "DON'T KNOW"] <- NA

# RACE # 

LCA_subset[LCA_subset$RACE == "RACE_OTHER", "RACE"] <- LCA_subset[LCA_subset$RACE == "RACE_OTHER", "RACE_5_TEXT"]
LCA_subset$RACE[LCA_subset$RACE == "RACE_WHITE"] <- "White"
LCA_subset$RACE[LCA_subset$RACE == "RACE_BLACK"] <- "Black"
LCA_subset$RACE[LCA_subset$RACE == "RACE_AMERICAN_INDIAN"] <- "American Indian"
LCA_subset$RACE[LCA_subset$RACE == "RACE_DK"] <- NA
LCA_subset$RACE[LCA_subset$RACE == "dominican/puerto rican"] <- "Dominican"
LCA_subset$RACE[LCA_subset$RACE == "dominicano"] <- "Dominican"
LCA_subset$RACE[LCA_subset$RACE == "hispanic"] <- "Hispanic"
LCA_subset$RACE[LCA_subset$RACE == "hispanic/puerto rican"] <- "Hispanic"
LCA_subset$RACE[LCA_subset$RACE == "hispano"] <- "Hispanic"
LCA_subset$RACE[LCA_subset$RACE == "latina"] <- "Latina"
LCA_subset$RACE[LCA_subset$RACE == "latina/hispanic"] <- "Latina"
LCA_subset$RACE[LCA_subset$RACE == "latino"] <- "Latina"
LCA_subset$RACE[LCA_subset$RACE == "Latino"] <- "Latina"
LCA_subset$RACE[LCA_subset$RACE == "n/a"] <- NA
LCA_subset$RACE[LCA_subset$RACE == "puerto rican"] <- "Puerto Rican"
LCA_subset$RACE[LCA_subset$RACE == "RACE_REF"] <- NA
LCA_subset$RACE[LCA_subset$RACE == "RACE_ASIAN"] <- "Asian"
LCA_subset$RACE[LCA_subset$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK"|LCA_subset$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK,RACE_WHITE"|LCA_subset$RACE == 
                  "RACE_AMERICAN_INDIAN,RACE_OTHER"|LCA_subset$RACE == "RACE_AMERICAN_INDIAN,RACE_WHITE"] <- "American Indian"
LCA_subset$RACE[LCA_subset$RACE == "RACE_BLACK,RACE_OTHER"|LCA_subset$RACE == "RACE_BLACK,RACE_WHITE"] <- "Black"
LCA_subset$RACE[LCA_subset$RACE == "RACE_NATHAWOROTH"] <- "Nathaworoth"
LCA_subset$RACE[LCA_subset$RACE == "RACE_WHITE, RACE_OTHER"|LCA_subset$RACE == "RACE_WHITE,RACE_DK"|LCA_subset$RACE == "RACE_WHITE,RACE_OTHER"] <- "White"
LCA_subset <- LCA_subset[,-12]
LCA_subset$RACE[LCA_subset$RACE == "American"|LCA_subset$RACE == "Asian"|LCA_subset$RACE == "Boricua"|
                  LCA_subset$RACE == "Cape Verdan"|LCA_subset$RACE == "Cape Verdean, Portuguese, Italian"|
                  LCA_subset$RACE == "Columbian"|LCA_subset$RACE == "French Indian, Italian"|
                  LCA_subset$RACE == "Haitian"|LCA_subset$RACE == "Jewish"|LCA_subset$RACE == "Mestizo"|
                  LCA_subset$RACE == "Nathaworoth"|LCA_subset$RACE == "Nigerian"|LCA_subset$RACE == "Portuguese"|
                  LCA_subset$RACE == "Sudamericana"] <- "Other"

# RETIRED #

LCA_subset$RETIRED[LCA_subset$RETIRED == "DON'T KNOW"|LCA_subset$RETIRED == "REFUSED"] <- NA

# HUNGRY #

LCA_subset$HUNGRY[LCA_subset$HUNGRY == "DON'T KNOW"|LCA_subset$HUNGRY == "REFUSED"] <- NA


################################################################################
# Recode response values to numbers starting at "1" (It's a poLCA thing)
################################################################################

LCA_subset$Age<-revalue(LCA_subset$Age, c("26_63"="1", "63_71"="2", "71_78"="3", "78up"="4"))
LCA_subset$INSURANCEYN<-LCA_subset(lca_subset$INSURANCEYN, c("YES"="1", "NO"="2"))
LCA_subset$DISABLED<-revalue(LCA_subset$DISABLED, c("NO"="1", "YES"="2"))
LCA_subset$SNAP<-revalue(LCA_subset$SNAP, c("NO"="1", "YES"="2"))
LCA_subset$GENHEALTH<-revalue(LCA_subset$GENHEALTH, c("Poor"="1", "Fair"="2", "Good"="3", "Very good"="4", "Excellent"="5"))
LCA_subset$GENDER<-revalue(LCA_subset$GENDER, c("Male"="1", "Woman"="2", "Genderqueer"="3"))
LCA_subset$HISPANIC<-revalue(LCA_subset$HISPANIC, c("NO"="1", "YES"="2"))
LCA_subset$RACE<-revalue(LCA_subset$RACE, c("American Indian"="1", "Black"="2",
                                            "Cape Verdan"="3", "Dominican"="4", "Hispanic"="5",
                                            "Latina"="6", "Puerto Rican"="7", "Spanish"="8", "White"="9",
                                            "Other"="10"))
### continue at RETIRED ### -miles


################################################################################
# Make sure all variables are factors
################################################################################

LCA_subset$Age<-as.factor(LCA_subset$Age)
LCA_subset$GENDER<-as.factor(LCA_subset$GENDER)
LCA_subset$INSURANCEYN<-as.factor(LCA_subset$INSURANCEYN)
LCA_subset$DISABLED<-as.factor(LCA_subset$DISABLED)
LCA_subset$SNAP<-as.factor(LCA_subset$SNAP)
LCA_subset$GENHEALTH<-as.factor(LCA_subset$GENHEALTH)

################################################################################
# Define the LCA formula. Variables in parentheses are the latent class classification variables. 
# Variables outside of the parentheses are covariates (not included in the LCA). 
# Run the LCA specifying a range of classes
################################################################################

f <- cbind(Age, INSURANCEYN, DISABLED, SNAP)~GENHEALTH

################################################################################
# Latent Class Analysis Specifying 1-3 Classes
################################################################################

lCA1 <- poLCA(f,LCA_subset, nclass=1,nrep=15) 
lCA2 <- poLCA(f,LCA_subset, nclass=2,nrep=15, graphs = T)
lCA3 <- poLCA(f,LCA_subset, nclass=3,nrep=15, graphs = T)
lCA4 <- poLCA(f,LCA_subset, nclass=4,nrep=15, graphs = T)

################################################################################
# THIS (ENTROPY) WILL HAPPEN LATER #
################################################################################
# Calculate entropy (3-class mode)l- values closer to 1.0 indicate greater separation of the classes.
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(LCA3$P) # Class proportions
error_post <- mean(apply(LCA3$posterior, 1, entropy))
LCA3_entropy <- (error_prior - error_post) / error_prior
LCA3_entropy

# predicted class membership is in:
LCA3$predclass[1:30]

# could be used as another variable (part of the data):
lca_subset$class <- LCA3$predclass

