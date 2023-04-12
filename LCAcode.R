#Latent Class Analysis with Covariates
library(plyr)
library(dplyr)
library(poLCA)

# DEAL WITH HUNGRY VARIABLE

#First, make sure all variables are factors and subset the data to include only LCA classification 
#variables and covariates

# subset the data set
lca_subset<-subset(FOTM_BL_Sub_1, select=c(Age, GENDER, RACE, HISPANIC, INSURANCEYN, MEDICARE,
                                    MEDICAID, EMPLOYED, RETIRED, DISABLED, SNAP, GENHEALTH, Health_Phys_Weeks, Health_Ment_Weeks, HUNGRY))

# cleaning up variables for LCA
FOTM_BL_Sub_1$Age <- as.character(as.numeric(FOTM_BL_Sub_1$Age))

FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 26 & FOTM_BL_Sub_1$Age < 63] <- "26-63 Years"
FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 63 & FOTM_BL_Sub_1$Age < 71] <- "63-71 Years"
FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 71 & FOTM_BL_Sub_1$Age < 78] <- "71-78 Years"
FOTM_BL_Sub_1$Age[FOTM_BL_Sub_1$Age >= 78] <- "Over 78"

FOTM_BL_Sub_1$GENDER[FOTM_BL_Sub_1$GENDER == "MAN"] <- "Man"
FOTM_BL_Sub_1$GENDER[FOTM_BL_Sub_1$GENDER == "WOMAN"] <- "Woman"

FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "REFUSED"] <- NA
FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "DON'T KNOW"] <- NA

FOTM_BL_Sub_1$SNAP[FOTM_BL_Sub_1$SNAP == "DON'T KNOW"] <- NA

FOTM_BL_Sub_1$GENHEALTH[FOTM_BL_Sub_1$GENHEALTH == "DON'T KNOW"] <- NA

FOTM_BL_Sub_1$MEDICARE[FOTM_BL_Sub_1$MEDICARE == "REFUSED"] <- NA

FOTM_BL_Sub_1$MEDICAID[FOTM_BL_Sub_1$MEDICAID == "REFUSED"] <- NA

FOTM_BL_Sub_1$EMPLOYED[FOTM_BL_Sub_1$EMPLOYED == "REFUSED"] <- NA

FOTM_BL_Sub_1$HUNGRY[FOTM_BL_Sub_1$HUNGRY == "DON'T KNOW"] <- NA



# recode response values to numbers starting at "1" (It's a poLCA thing)
lca_subset$Age<-revalue(lca_subset$Age, c("26-63 Years"="1", "63-71 Years"="2", "71-78 Years"="3", "Over 78"="4"))
lca_subset$GENDER<-revalue(lca_subset$GENDER, 
                                         c("Man"="1", "Woman"="2", "Genderqueer"="3")) 
lca_subset$RACE<-revalue(lca_subset$RACE, c("RACE_BLACK"="1", "RACE_WHITE"="2", "RACE_AMERICAN_INDIAN"="3", "RACE_ASIAN"="4", "RACE_OTHER"="5", "RACE_DK"="6"))
lca_subset$HISPANIC<-revalue(lca_subset$HISPANIC, c("No"="1", "Yes"="2"))
lca_subset$INSURANCEYN<-revalue(lca_subset$INSURANCEYN, c("YES"="1", "NO"="2", "DON'T KNOW"="3"))
lca_subset$MEDICARE<-revalue(lca_subset$MEDICARE, c("NO"="1", "YES"="2"))
lca_subset$MEDICAID<-revalue(lca_subset$MEDICAID, c("NO"="1", "YES"="2"))
lca_subset$EMPLOYED<-revalue(lca_subset$EMPLOYED, c("NO"="1", "YES"="2"))
lca_subset$RETIRED<-revalue(lca_subset$RETIRED, c("Retired"="2", "Unemployed"="1"))
lca_subset$DISABLED<-revalue(lca_subset$DISABLED, c("NO"="1", "YES"="2"))
lca_subset$SNAP<-revalue(lca_subset$SNAP, c("NO"="1", "YES"="2"))
lca_subset$GENHEALTH<-revalue(lca_subset$GENHEALTH, c("Poor"="1", "Fair"="2", "Good"="3", "Very good"="4", "Excellent"="5"))
lca_subset$Health_Phys_Weeks<-revalue(lca_subset$Health_Phys_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))
lca_subset$Health_Ment_Weeks<-revalue(lca_subset$Health_Ment_Weeks, c("None"="1", "One Week"="2", "More Than One Week"="3", "Every Day"="4"))
lca_subset$HUNGRY<-revalue(lca_subset$HUNGRY, c("NO"="1", "YES"="2"))

# make sure all variables are factors

# lca_subset$ethnic<-as.factor(lca_subset$ethnic)

lca_subset$Age<-as.factor(lca_subset$Age)
lca_subset$GENDER<-as.factor(lca_subset$GENDER)
lca_subset$RACE<-as.factor(lca_subset$RACE)
lca_subset$HISPANIC<-as.factor(lca_subset$HISPANIC)
lca_subset$INSURANCEYN<-as.factor(lca_subset$INSURANCEYN)
lca_subset$MEDICARE<-as.factor(lca_subset$MEDICARE)
lca_subset$MEDICAID<-as.factor(lca_subset$MEDICAID)
lca_subset$EMPLOYED<-as.factor(lca_subset$EMPLOYED)
lca_subset$RETIRED<-as.factor(lca_subset$RETIRED)
lca_subset$DISABLED<-as.factor(lca_subset$DISABLED)
lca_subset$SNAP<-as.factor(lca_subset$SNAP)
lca_subset$GENHEALTH<-as.factor(lca_subset$GENHEALTH)
lca_subset$Health_Phys_Weeks<-as.factor(lca_subset$Health_Phys_Weeks)
lca_subset$Health_Ment_Weeks<-as.factor(lca_subset$Health_Ment_Weeks)
lca_subset$HUNGRY<-as.factor(lca_subset$HUNGRY)


#Second, define the LCA formula. Variables in parentheses are the latent class classification variables. 
#Variables outside of the parentheses are covariates (not included in the LCA). 
#Finally, run the LCA specifying a range of classes
f <- cbind(Age, GENDER, RACE, HISPANIC, INSURANCEYN, MEDICARE, MEDICAID, EMPLOYED, RETIRED, DISABLED, 
           SNAP)~GENHEALTH+Health_Phys_Weeks+Health_Ment_Weeks+HUNGRY

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