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

MASTER_CLEAN_DATA_FOTM <- read_excel("/Users/mileslambke/Library/Containers/com.microsoft.Excel/Data/Desktop/GitHub/QAC380_ProjData/Copy of MASTER CLEAN DATA FOTM.xlsx", sheet="All Baseline ")
View(MASTER_CLEAN_DATA_FOTM)

# Loyalty Card Number # --> Code to 1 word for new subset

names(MASTER_CLEAN_DATA_FOTM)[names(MASTER_CLEAN_DATA_FOTM)== "Loyalty card number"] <- "LCN"

# check out duplicated LCN #748
duplcn<-subset(MASTER_CLEAN_DATA_FOTM, LCN=="748")
# different people - delete for now. Check with Yelena to fix later
MASTER_CLEAN_DATA_FOTM<-subset(MASTER_CLEAN_DATA_FOTM, LCN !="748")
###############################################################################
# Variables of Interest # 
################################################################################

# Unique ID ("Loyalty card number")
# Age ("Age")
# Disabled ("DISABLED") YES/NO
# Snap ("SNAP") YES/NO
# Gender ("GENDER")
# Hispanic ("HISPANIC") YES/NO
# Race ("RACE") 
# Employment Status ("EMPLOYMENT") Employed/Retired/Unemployed
# General Health ("GENHEALTH") "Poor" = 1, "Fair, or" = 2, "Good" = 3, "Very good" = 4, "Excellent" = 5
# Hunger ("HUNGRY") YES/NO
# Eat Less than Desired ("EAT_LESS")
# Medicare Status ("MEDICARE") YES/NO
# Medicaid Status ("MEDICAID") YES/NO
# Self Reported Physical Health ("Health_Phys_Weeks")
# Self Reported Mental Health ("Health_Ment_Weeks")

################################################################################
# Make New Subset Using Variables of Interest
################################################################################

#USDA Food Security
MASTER_CLEAN_DATA_FOTM$HH3<-0
MASTER_CLEAN_DATA_FOTM$HH3[grepl("Often", MASTER_CLEAN_DATA_FOTM$FOOD_DIDNT_LAST)|
                             grepl("Sometimes", MASTER_CLEAN_DATA_FOTM$FOOD_DIDNT_LAST)]<-1

MASTER_CLEAN_DATA_FOTM$HH4<-0
MASTER_CLEAN_DATA_FOTM$HH4[grepl("Often", MASTER_CLEAN_DATA_FOTM$BALANCED_MEALS)|
                             grepl("Sometimes", MASTER_CLEAN_DATA_FOTM$BALANCED_MEALS)]<-1

MASTER_CLEAN_DATA_FOTM$AD1<-0
MASTER_CLEAN_DATA_FOTM$AD1[MASTER_CLEAN_DATA_FOTM$SKIP_MEALS == "YES"]<-1

MASTER_CLEAN_DATA_FOTM$AD1a<-0
MASTER_CLEAN_DATA_FOTM$AD1a[grepl("Almost", MASTER_CLEAN_DATA_FOTM$OFTEN_SKIP_MEALS)|
                             grepl("Some months", MASTER_CLEAN_DATA_FOTM$OFTEN_SKIP_MEALS)]<-1

MASTER_CLEAN_DATA_FOTM$AD2<-0
MASTER_CLEAN_DATA_FOTM$AD2[MASTER_CLEAN_DATA_FOTM$EAT_LESS == "YES"]<-1

MASTER_CLEAN_DATA_FOTM$AD3<-0
MASTER_CLEAN_DATA_FOTM$AD3[MASTER_CLEAN_DATA_FOTM$HUNGRY == "YES"]<-1

# SUM food security items for total score
MASTER_CLEAN_DATA_FOTM$SUM_FS<-(MASTER_CLEAN_DATA_FOTM$HH3+MASTER_CLEAN_DATA_FOTM$HH4+
                                   MASTER_CLEAN_DATA_FOTM$AD1+MASTER_CLEAN_DATA_FOTM$AD1a+
                                   MASTER_CLEAN_DATA_FOTM$AD2+MASTER_CLEAN_DATA_FOTM$AD3)
freq(MASTER_CLEAN_DATA_FOTM$SUM_FS, plot=F)

# categorize
MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY[MASTER_CLEAN_DATA_FOTM$SUM_FS<=1]<-"High/Marginal Food Security"
MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY[MASTER_CLEAN_DATA_FOTM$SUM_FS>=2 & 
                                       MASTER_CLEAN_DATA_FOTM$SUM_FS<=4]<-"Low Food Security"
MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY[MASTER_CLEAN_DATA_FOTM$SUM_FS>4]<-"Very Low Food Security"
freq(MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY, plot=F)


LCA_subset <- subset(MASTER_CLEAN_DATA_FOTM, select=c(LCN, Age, INSURANCEYN, DISABLED, SNAP, GENDER, HISPANIC, 
                                                      RACE, EMPLOYED, RETIRED,FOOD_SECURITY, RACE_5_TEXT, MEDICARE, 
                                                      MEDICAID, GENHEALTH, HEALTH_PHYS_0_TEXT, 
                                                      HEALTH_MENTAL_0_TEXT)) 
View(LCA_subset)

#LCA_subset <- LCA_subset[-1,]

################################################################################
# Renaming Variables #
################################################################################

names(LCA_subset)[names(LCA_subset)== "HEALTH_PHYS_0_TEXT"] <- "HEALTH_PHYS"
names(LCA_subset)[names(LCA_subset)== "HEALTH_MENTAL_0_TEXT"] <- "HEALTH_MENTAL"

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

LCA_subset$SNAP[LCA_subset$SNAP == "DON’T KNOW"] <- NA

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
LCA_subset$HISPANIC[LCA_subset$HISPANIC == "DON’T KNOW"] <- NA
LCA_subset$HISPANIC<-as.factor(LCA_subset$HISPANIC)

# try to identify NA ethnicity by race
sub1<-filter(LCA_subset,is.na(HISPANIC))
View(sub1)
# can't determine - leave HISPANIC as NA

# RACE # 

LCA_subset[LCA_subset$RACE == "RACE_OTHER", "RACE"] <- LCA_subset[LCA_subset$RACE == "RACE_OTHER", "RACE_5_TEXT"]
LCA_subset$RACE[LCA_subset$RACE == "RACE_WHITE"] <- "White"
LCA_subset$RACE[LCA_subset$RACE == "RACE_BLACK"] <- "Black"
LCA_subset$RACE[LCA_subset$RACE == "RACE_AMERICAN_INDIAN"] <- "American Indian"
LCA_subset$RACE[LCA_subset$RACE == "RACE_DK"] <- NA
LCA_subset$RACE[LCA_subset$RACE == "dominican/puerto rican"] <- "Multiracial"
LCA_subset$RACE[LCA_subset$RACE == "dominicano"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "hispanic"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "hispanic/puerto rican"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "hispano"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "latina"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "latina/hispanic"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "latino"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "Latino"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "n/a"] <- NA
LCA_subset$RACE[LCA_subset$RACE == "puerto rican"] <- "Other"
LCA_subset$RACE[LCA_subset$RACE == "RACE_REF"] <- NA
LCA_subset$RACE[LCA_subset$RACE == "RACE_ASIAN"] <- "Asian"

LCA_subset$RACE[LCA_subset$RACE == "RACE_AMERICAN_INDIAN,RACE_BLACK"|LCA_subset$RACE == 
                  "RACE_AMERICAN_INDIAN,RACE_BLACK,RACE_WHITE"|LCA_subset$RACE == "RACE_AMERICAN_INDIAN,RACE_OTHER"|
                  LCA_subset$RACE == "RACE_AMERICAN_INDIAN,RACE_WHITE"|LCA_subset$RACE == "RACE_BLACK,RACE_OTHER"|
                  LCA_subset$RACE == "RACE_BLACK,RACE_WHITE" | LCA_subset$RACE == "RACE_AMERICAN_INDIAN,RACE_WHITE"|
                  LCA_subset$RACE == "RACE_BLACK,RACE_OTHER"|LCA_subset$RACE == "French Indian, Italian"|
                  LCA_subset$RACE == "RACE_BLACK,RACE_WHITE"|LCA_subset$RACE == "RACE_WHITE, RACE_OTHER"|
                  LCA_subset$RACE =="RACE_WHITE,RACE_DK"|LCA_subset$RACE =="RACE_WHITE,RACE_OTHER"|
                  LCA_subset$RACE == "Cape Verdean, Portuguese, Italian"] <- "Multiracial"

LCA_subset$RACE[LCA_subset$RACE == "RACE_NATHAWOROTH"|LCA_subset$RACE == "American"|LCA_subset$RACE == "Asian"|LCA_subset$RACE == "Boricua"|
                  LCA_subset$RACE == "Cape Verdan"|
                  LCA_subset$RACE == "Columbian"|
                  LCA_subset$RACE == "Haitian"|LCA_subset$RACE == "Jewish"|LCA_subset$RACE == "Mestizo"|
                  LCA_subset$RACE == "Nathaworoth"|LCA_subset$RACE == "Nigerian"|LCA_subset$RACE == "Portuguese"|
                  LCA_subset$RACE == "Sudamericana"|LCA_subset$RACE == "Dominican"|LCA_subset$RACE == "Hispanic"|
                  LCA_subset$RACE == "Latina"|LCA_subset$RACE == "Puerto Rican"|
                  LCA_subset$RACE == "Spanish"] <- "Other"
freq(LCA_subset$RACE)
# Removing RACE_5_TEXT Column #

LCA_subset <- subset(LCA_subset, select =-c(RACE_5_TEXT))

# EMPLOYMENT #

LCA_subset$EMPLOYMENT[LCA_subset$RETIRED == "YES" & LCA_subset$EMPLOYED == "NO"] <- "Retired"
LCA_subset$EMPLOYMENT[LCA_subset$EMPLOYED == "YES" & LCA_subset$RETIRED == "NO"] <- "Employed"
LCA_subset$EMPLOYMENT[LCA_subset$RETIRED == "NO" & LCA_subset$EMPLOYED == "NO"] <- "Unemployed"
LCA_subset$EMPLOYMENT[LCA_subset$RETIRED == "DON'T KNOW" & LCA_subset$EMPLOYED == "NO"] <- NA
LCA_subset <- subset(LCA_subset, select =-c(EMPLOYED,RETIRED))


# MEDICARE #

LCA_subset$MEDICARE[LCA_subset$MEDICARE == "DON'T KNOW"] <- NA
LCA_subset$MEDICARE[LCA_subset$MEDICARE == "REFUSED"] <- NA

# MEDICAID #

LCA_subset$MEDICAID[LCA_subset$MEDICAID == "DON'T KNOW"] <- NA
LCA_subset$MEDICAID[LCA_subset$MEDICAID == "REFUSED"] <- NA

# combine medicare and medicaid
LCA_subset$Medicare_Medicaid[LCA_subset$MEDICAID=="YES"|LCA_subset$MEDICAID=="YES"]<-"YES"
LCA_subset$Medicare_Medicaid[LCA_subset$MEDICAID=="NO" & LCA_subset$MEDICAID=="NO"]<-"NO"
freq(LCA_subset$Medicare_Medicaid, plot=F)

LCA_subset <- subset(LCA_subset, select =-c(MEDICARE,MEDICAID))

# Health_Phys_Weeks #

LCA_subset$HEALTH_PHYS<-as.numeric(LCA_subset$HEALTH_PHYS)
LCA_subset$Health_Phys_Weeks[LCA_subset$HEALTH_PHYS==0]<-"None"
LCA_subset$Health_Phys_Weeks[LCA_subset$HEALTH_PHYS>=1 & LCA_subset$HEALTH_PHYS<8]<-"One Week"
LCA_subset$Health_Phys_Weeks[LCA_subset$HEALTH_PHYS>=8 & LCA_subset$HEALTH_PHYS<30]<-"More Than One Week"
LCA_subset$Health_Phys_Weeks[LCA_subset$HEALTH_PHYS==30]<-"Every Day"

LCA_subset <- subset(LCA_subset, select =c(-HEALTH_PHYS))

# Health_Ment_Weeks #

LCA_subset$HEALTH_MENTAL<-as.numeric(LCA_subset$HEALTH_MENTAL)
LCA_subset$Health_Ment_Weeks[LCA_subset$HEALTH_MENTAL==0]<-"None"
LCA_subset$Health_Ment_Weeks[LCA_subset$HEALTH_MENTAL>=1 & LCA_subset$HEALTH_MENTAL<8]<-"One Week"
LCA_subset$Health_Ment_Weeks[LCA_subset$HEALTH_MENTAL>=8 & LCA_subset$HEALTH_MENTAL<30]<-"More Than One Week"
LCA_subset$Health_Ment_Weeks[LCA_subset$HEALTH_MENTAL==30]<-"Every Day"

LCA_subset <- subset(LCA_subset, select =-c(HEALTH_MENTAL))

# duplicate subset data under another name to avoid any problems from LCA
LCA_subset2<-LCA_subset

################################################################################
# Recode response values to numbers starting at "1" (It's a poLCA thing)
################################################################################

LCA_subset2$Age<-revalue(LCA_subset2$Age, c("26_63"="1", "63_71"="2", "71_78"="3", "78up"="4"))
LCA_subset2$DISABLED<-revalue(LCA_subset2$DISABLED, c("NO"="1", "YES"="2"))
LCA_subset2$SNAP<-revalue(LCA_subset2$SNAP, c("NO"="1", "YES"="2"))
LCA_subset2$GENHEALTH<-revalue(LCA_subset2$GENHEALTH, c("Poor"="1", "Fair"="2", "Good"="3", 
                                                      "Very good"="4", "Excellent"="5"))
LCA_subset2$GENDER<-revalue(LCA_subset2$GENDER, c("Man"="1", "Woman"="2"))
LCA_subset2$HISPANIC<-revalue(LCA_subset2$HISPANIC, c("NO"="1", "YES"="2"))
LCA_subset2$RACE<-revalue(LCA_subset2$RACE, c("White"="1", "Black"="2",
                                            "American Indian"="3", "Multiracial"="4", "Other"="5"))
LCA_subset2$FOOD_SECURITY<-revalue(LCA_subset2$FOOD_SECURITY, c("High/Marginal Food Security"="1", 
                                                              "Low Food Security"="2",
                                                              "Very Low Food Security"="3"))
LCA_subset2$Medicare_Medicaid<-revalue(LCA_subset2$Medicare_Medicaid, c("NO"="1", "YES"="2"))
LCA_subset2$EMPLOYMENT<-revalue(LCA_subset2$EMPLOYMENT, c("Employed"="1", "Unemployed"="2", "Retired"="3"))
LCA_subset2$Health_Phys_Weeks<-revalue(LCA_subset2$Health_Phys_Weeks, 
                                      c("None"="1", "One Week"="2", "More Than One Week"="3", 
                                        "Every Day"="4"))
LCA_subset2$Health_Ment_Weeks<-revalue(LCA_subset2$Health_Ment_Weeks, 
                                      c("None"="1", "One Week"="2", "More Than One Week"="3", 
                                        "Every Day"="4"))


################################################################################
# Make sure all variables are factors
################################################################################

LCA_subset2$Age<-as.factor(LCA_subset2$Age)
LCA_subset2$DISABLED<-as.factor(LCA_subset2$DISABLED)
LCA_subset2$SNAP<-as.factor(LCA_subset2$SNAP)
LCA_subset2$GENDER<-as.factor(LCA_subset2$GENDER)
LCA_subset2$HISPANIC<-as.factor(LCA_subset2$HISPANIC)
LCA_subset2$RACE<-as.factor(LCA_subset2$RACE)
LCA_subset2$FOOD_SECURITY<-as.factor(LCA_subset2$FOOD_SECURITY)
LCA_subset2$Medicare_Medicaid<-as.factor(LCA_subset2$Medicare_Medicaid)
LCA_subset2$EMPLOYMENT<-as.factor(LCA_subset2$EMPLOYMENT)
LCA_subset2$GENHEALTH<-as.factor(LCA_subset2$GENHEALTH)
LCA_subset2$Health_Phys_Weeks<-as.factor(LCA_subset2$Health_Phys_Weeks)
LCA_subset2$Health_Ment_Weeks<-as.factor(LCA_subset2$Health_Ment_Weeks)


################################################################################
# Define the LCA formula. Variables in parentheses are the latent class classification variables. 
# Variables outside of the parentheses are covariates (not included in the LCA). 
# Run the LCA specifying a range of classes
################################################################################

f <- cbind(Age, DISABLED, SNAP, 
           HISPANIC, RACE, FOOD_SECURITY, Medicare_Medicaid,
           EMPLOYMENT, Health_Phys_Weeks, Health_Ment_Weeks)~1


################################################################################
# Latent Class Analysis 
################################################################################

lCA3 <- poLCA(f,LCA_subset2, nclass=3,nrep=50, graphs = T, na.rm=F)

################################################################################
# THIS (ENTROPY) WILL HAPPEN LATER #
################################################################################
# Calculate entropy (2 and 3-class models)- values closer to 1.0 indicate greater separation of the classes.

entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lCA3$P) # Class proportions
error_post <- mean(apply(lCA3$posterior, 1, entropy))
lCA3_entropy <- (error_prior - error_post) / error_prior
lCA3_entropy

# predicted class membership is in:
lCA3$predclass[1:30]

# add as another variable to the subset data (part of the data):
LCA_subset2$class <- lCA3$predclass

# merge class variable back into full FOTM data set
# delete uncleaned variables from full FOTM data set
FOTM_BL_Sub<-subset(MASTER_CLEAN_DATA_FOTM, select=-c(Age, DISABLED, SNAP, 
                                                      HISPANIC, RACE, FOOD_SECURITY))
FOTM_BL_Full<-inner_join(FOTM_BL_Sub,LCA_subset2, by="LCN")
#################################################################################
# Here's where you can do regression analysis on your health outcomes at 6 months
#  controlling for baseline health outcomes - we can talk more tomorrow
# Might want to consider creating 2 categories for logistic
#################################################################################