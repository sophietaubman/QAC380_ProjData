#Latent Class Analysis with Covariates

#First, make sure all variables are factors and subset the data to include only LCA classification 
#variables and covariates

# subset the data set
lca_subset<-subset(FOTM_BL_Sub, select=c(Age, GENDER, RACE, HISPANIC, INSURANCEYN, MEDICARE,
                                    MEDICAID, EMPLOYED, RETIRED, DISABLED, SNAP, GENHEALTH, Health_Phys_Weeks, Health_Mental_Weeks))

# recode response values to numbers starting at "1" (It's a poLCA thing)
lca_subset$Age<-revalue(lca_subset$Age, c("No"="2", "Yes"="1"))
lca_subset$GENDER<-revalue(lca_subset$condom_use_frequency, 
                                         c("At least sometimes"="1", "Rarely or Never"="2"))
lca_subset$RACE<-revalue(lca_subset$other_drugs_sex, c("No"="2", "Yes"="1"))
lca_subset$HISPANIC<-revalue(lca_subset$drugs_sex___1, c("Unchecked"="2", "Checked"="1"))
lca_subset$INSURANCEYN<-revalue(lca_subset$drugs_sex___2, c("Unchecked"="2", "Checked"="1"))
lca_subset$MEDICARE<-revalue(lca_subset$met_online, c("No"="2", "Yes"="1"))
lca_subset$MEDICAID<-revalue(lca_subset$rf_anymsm, c("Non-MSM"="2", "MSM"="1"))
lca_subset$EMPLOYED<-revalue(lca_subset$white, c("non-White"="2", "White"="1"))
lca_subset$RETIRED<-revalue(lca_subset$ethnic, c("Non-Hispanic"="2", "Hispanic"="1"))
lca_subset$DISABLED<-revalue(lca_subset$insured, c("Private"="1", "Other"="2", "No"="3"))
lca_subset$SNAP<-revalue(lca_subset$low_income, c("$12,000 or more"="2", "Less than $12,000"="1"))
lca_subset$GENHEALTH<-revalue(lca_subset$ri_infected, c("Other"="2", "Infected_RI"="1"))
lca_subset$Health_Phys_Weeks<-revalue(lca_subset$aids, c("No"="2", "Yes"="1"))
lca_subset$Health_Mental_Weeks<-revalue(lca_subset$ri_born, c("No"="2", "Yes"="1"))

# make sure all variables are factors

lca_subset$ethnic<-as.factor(lca_subset$ethnic)

str(lca_subset$receptive_anal_cat)
str(lca_subset$total_mf_partners)
str(lca_subset$condom_use_frequency)
str(lca_subset$other_drugs_sex)
str(lca_subset$drugs_sex___1)
str(lca_subset$drugs_sex___2)
str(lca_subset$met_online)
str(lca_subset$rf_anymsm)
str(lca_subset$cd4_cat)
str(lca_subset$age_cat)
str(lca_subset$white)
str(lca_subset$ethnic)
str(lca_subset$insured)
str(lca_subset$low_income)
str(lca_subset$ri_infected)
str(lca_subset$aids)

#Second, define the LCA formula. Variables in parentheses are the latent class classification variables. 
#Variables outside of the parentheses are covariates (not included in the LCA). 
#Finally, run the LCA specifying a range of classes
f <- cbind(receptive_anal_cat,total_mf_partners,condom_use_frequency,other_drugs_sex,drugs_sex___1,
           drugs_sex___2,met_online,rf_anymsm,cd4_cat)~age_cat+white+ethnic+insured+low_income+ri_infected

# latent class analysis specifying 1-3 classes
lCA1 <- poLCA(f,lca_subset, nclass=1,nrep=15) 
lCA2 <- poLCA(f,lca_subset, nclass=2,nrep=15, graphs = T)
lCA3 <- poLCA(f,lca_subset, nclass=3,nrep=15, graphs = T)

# Calculate entropy (3-class mode)l- values closer to 1.0 indicate greater separation of the classes.
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(LCA3$P) # Class proportions
error_post <- mean(apply(LCA3$posterior, 1, entropy))
LCA3_entropy <- (error_prior - error_post) / error_prior
LCA3_entropy

#predicted class membership is in:
LCA3$predclass[1:30]

#could be used as another variable (part of the data):
lca_subset$class <- LCA3$predclass