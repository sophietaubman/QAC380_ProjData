
################################################################################
# Load libraries needed 
################################################################################

library(descr)
library(ggplot2)

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
# Deleting a Column
################################################################################

FOTM_BL_Sub_1 <- FOTM_BL_Sub_1[,-18]
View(FOTM_BL_Sub_1)

################################################################################
# Univariate Graphs for Each Variable of Interest 
################################################################################

# AGE # 

  # Univariate Bar Graph:

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(Age)))+ggtitle("Age")

# Create Categories

FOTM_BL_Sub_1$Age_Groups[FOTM_BL_Sub_1$Age >= 26 & FOTM_BL_Sub_1$Age < 63] <- "26-63 Years"
FOTM_BL_Sub_1$Age_Groups[FOTM_BL_Sub_1$Age >= 63 & FOTM_BL_Sub_1$Age < 71] <- "63-71 Years"
FOTM_BL_Sub_1$Age_Groups[FOTM_BL_Sub_1$Age >= 71 & FOTM_BL_Sub_1$Age < 78] <- "71-78 Years"
FOTM_BL_Sub_1$Age_Groups[FOTM_BL_Sub_1$Age >= 78] <- "Over 78"


  # Univariate Histogram: 

ggplot(FOTM_BL_Sub_1, aes(Age)) + geom_histogram(binwidth = 5) + ggtitle("Age Distribution") + 
  ylab("Number of People")

################################################################################

# PHYSICAL HEALTH #

# Bar graph ordered by number of poor health days 0 to 30

ggplot(data=FOTM_BL_Sub_1) + geom_bar(aes(x=factor(Health_Phys_Weeks, level=c("None","One Week","More Than One Week","Every Day")))) + 
  xlab("# of Days") + ylab("# of People") + ggtitle("Poor Physical Health in Past 30 Days")

# Change Physical Health from Character Variable to Numeric
FOTM_BL_Sub_1$HEALTH_PHYS <- as.numeric(as.character(FOTM_BL_Sub_1$HEALTH_PHYS))

# Create Categories
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS==0]<-"None"
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS>=1 & FOTM_BL_Sub_1$HEALTH_PHYS<8]<-"One Week"
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS>=8 & FOTM_BL_Sub_1$HEALTH_PHYS<30]<-"More Than One Week"
FOTM_BL_Sub_1$Health_Phys_Weeks[FOTM_BL_Sub_1$HEALTH_PHYS==30]<-"Every Day"
FOTM_BL_Sub_1$Health_Phys_Weeks <- as.factor(FOTM_BL_Sub_1$Health_Phys_Weeks)
freq(FOTM_BL_Sub_1$Health_Phys_Weeks, plot=F)
freq(FOTM_BL_Sub_1$HEALTH_PHYS, plot=F)

# Load tidyverse
library(tidyverse)

# Drop NA From Graphs
FOTM_BL_Sub_1 %>%
  drop_na(Health_Phys_Weeks) %>%
  ggplot(aes(x=factor(Health_Phys_Weeks, levels=c("None","One Week","More Than One Week","Every Day")))) +
  geom_bar() +
  xlab("# of Days") + ylab("# of People") + ggtitle("# of Days of Poor Physical Health in Past 30 Days")

################################################################################

# MENTAL HEALTH #

# Bar graph ordered by number of poor mental health days 0 to 30

ggplot(data=FOTM_BL_Sub_1) + geom_bar(aes(x=factor(HEALTH_MENTAL, level=c
                                                   ("0","1","2","3","4","5","6","7","8","9","10",
                                                     "11","12","13","14","15","16","17","18","19","20",
                                                     "21","22","23","24","25","26","27","28","29","30")))) + 
  xlab("# of Days") + ylab("# of People") + ggtitle("# of Days of Poor Mental Health in Past 30 Days")

# Change Mental Health from Character Variable to Numeric
FOTM_BL_Sub_1$HEALTH_MENTAL <- as.numeric(as.character(FOTM_BL_Sub_1$HEALTH_MENTAL))

# Create Categories
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL==0]<-"None"
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL>=1 & FOTM_BL_Sub_1$HEALTH_MENTAL<8]<-"One Week"
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL>=8 & FOTM_BL_Sub_1$HEALTH_MENTAL<30]<-"More Than One Week"
FOTM_BL_Sub_1$Health_Ment_Weeks[FOTM_BL_Sub_1$HEALTH_MENTAL==30]<-"Every Day"
FOTM_BL_Sub_1$Health_Ment_Weeks <- as.factor(FOTM_BL_Sub_1$Health_Ment_Weeks)
freq(FOTM_BL_Sub_1$Health_Ment_Weeks, plot=F)
freq(FOTM_BL_Sub_1$HEALTH_MENTAL, plot=F)

# Load tidyverse
library(tidyverse)

# Drop NA From Graphs
FOTM_BL_Sub_1 %>%
  drop_na(Health_Ment_Weeks) %>%
  ggplot(aes(x=factor(Health_Ment_Weeks, levels=c("None","One Week","More Than One Week","Every Day")))) +
  geom_bar() + xlab("# of Days") + ylab("# of People") + ggtitle("# of Days of Poor Mental Health in Past 30 Days")

################################################################################

# SNAP BENEFITS #

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(SNAP)))+ggtitle("Snap Benefits") + xlab("") + ylab("# of People") 

################################################################################

# INSURANCE #

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(INSURANCEYN)))+ggtitle("Insurance") + xlab("") + ylab("# of People") 

################################################################################

# HISPANIC #

FOTM_BL_Sub_1$HISPANIC[FOTM_BL_Sub_1$HISPANIC == "NO"] <- "No"
FOTM_BL_Sub_1$HISPANIC[FOTM_BL_Sub_1$HISPANIC == "YES"] <- "Yes"
FOTM_BL_Sub_1$HISPANIC[FOTM_BL_Sub_1$HISPANIC == "DON'T KNOW"] <- "Don't Know"

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(HISPANIC)))+ggtitle("Hispanic") + xlab("") + ylab("# of People") 

################################################################################

# EMPLOYMENT #

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(EMPLOYED)))+ggtitle("Employment") + xlab("") + ylab("# of People") 

################################################################################

# RETIREMENT/EMPLOYMENT #

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(RETIRED)))+ggtitle("Employment Status") + xlab("") + ylab("# of People") 

# This needs to be edited for the sub_1 data set in order to allow for changing NA title #

FOTM_BL_Sub$EMPLOYED[FOTM_BL_Sub$RETIRED == "YES" & FOTM_BL_Sub$EMPLOYED == "NO"] <- NA
FOTM_BL_Sub$RETIRED[FOTM_BL_Sub$EMPLOYED == "YES" & FOTM_BL_Sub$RETIRED == "NO"] <- NA

# Changing labels #

FOTM_BL_Sub_1$RETIRED[FOTM_BL_Sub_1$RETIRED == "NO"] <- "Unemployed"
FOTM_BL_Sub_1$RETIRED[FOTM_BL_Sub_1$RETIRED == "YES"] <- "Retired"
FOTM_BL_Sub_1$RETIRED[FOTM_BL_Sub_1$RETIRED == "DON'T KNOW"] <- "Don't Know"

FOTM_BL_Sub_1$Health_Ment_Weeks <- as.factor(FOTM_BL_Sub_1$Health_Ment_Weeks)

FOTM_BL_Sub_1$RETIRED <- as.factor(FOTM_BL_Sub_1$RETIRED)

freq(FOTM_BL_Sub_1$Health_Ment_Weeks, plot=F)
freq(FOTM_BL_Sub_1$HEALTH_MENTAL, plot=F)

# Load tidyverse
library(tidyverse)

# Drop NA From Graphs
FOTM_BL_Sub_1 %>%
  drop_na(Health_Ment_Weeks) %>%
  ggplot(aes(x=factor(Health_Ment_Weeks, levels=c("None","One Week","More Than One Week","Every Day")))) +
  geom_bar() + xlab("# of Days") + ylab("# of People") + ggtitle("# of Days of Poor Mental Health in Past 30 Days")


################################################################################

# DISABLED #

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(DISABLED)))+ggtitle("Disabled") + xlab("") + ylab("# of People")

FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "DON'T KNOW"] <- NA
FOTM_BL_Sub_1$DISABLED[FOTM_BL_Sub_1$DISABLED == "REFUSED"] <- NA

FOTM_BL_Sub_1 %>%
  drop_na(DISABLED) %>%
  ggplot(aes(x=factor(DISABLED, levels=c("NO","YES")))) +
  geom_bar() + xlab("") + ylab("# of People") + ggtitle("Ability Status")

################################################################################

# GENERAL HEALTH #

FOTM_BL_Sub_1$GENHEALTH[FOTM_BL_Sub_1$GENHEALTH == "Fair, or"] <- "Fair"
FOTM_BL_Sub_1$GENHEALTH[FOTM_BL_Sub_1$GENHEALTH == "DON'T KNOW"] <- NA

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(GENHEALTH,level=c(NA, "Poor","Fair","Good","Very Good","Excellent")))) +
   ggtitle("General Health Rating") + xlab("") + ylab("# of People") 

################################################################################
# Bivariate Graphing!
################################################################################

# Categorical-Categorical (Crosstabs)
  # tab1 <- table(myData$CategResponseVar, myData$CategExplanatoryVar)
  # tab1_colProp <- prop.table(tab1, 2) # column proportions
  # tab1_rowProp <- prop.table(tab1, 1) # row proportions
  # tab1_cellProp <- prop.table(tab1) # cell proportions

# Categorical-Categorical (Plot)
  # visualization - Assumes response variable is coded as 0/1
  # ggplot(data=graph_data) + stat_summary(aes(x=CategExplanatoryVar, y=BinaryResponseVar), fun=”mean”, geom=”bar”) + ylab(“Proportion of Subjects at each Response Level within each group”) + ggtitle(“Informative Title Here”)

# Categorical-Quantitative (Means by Group)
  # numbers
  # by(myData$QuantResponseVar, myData$CategExplanatoryVar, mean, na.rm = TRUE) 
  # by(myData$QuantResponseVar, myData$CategExplanatoryVar, sd, na.rm = TRUE) 
  # by(myData$QuantResponseVar, myData$CategExplanatoryVar, length) 

# Categorical-Quantitative (Plot)
  # Option 1: Bar plot
  # ggplot(data=myData)+ stat_summary(aes(x=CategExplanatoryVar, y=QuantResponseVar), fun=mean, geom=”bar”)

# Option 2: Boxplot
  # ggplot(data=myData)+ geom_boxplot(aes(x=CategExplanatoryVar, y=QuantResponseVar))+ ggtitle(“Descriptive Title Here”)

# Quantitative-Quantitative (Plot)
  # ggplot(data=myData)+ geom_point(aes(x=QuantExplanatoryVar, y=QuantResponseVar))+ geom_smooth(aes(x=QuantExplanatoryVar, y=QuantResponseVar), method="lm")

################################################################################
# SNAP vs. Age, Health (General, Mental, Physical)
################################################################################

# visualization - Assumes response variable is coded as 0/1
FOTM_BL_Sub_1$SNAP[FOTM_BL_Sub_1$SNAP == "NO"] <- 0
FOTM_BL_Sub_1$SNAP[FOTM_BL_Sub_1$SNAP == "YES"] <- 1

# AGE v. SNAP
ggplot(data=FOTM_BL_Sub_1) + stat_summary(aes(x=Age_Groups, y=SNAP), fun="mean", geom="bar") + ylab("Proportion of Subjects with SNAP Benefits") + xlab("Age Groups") + ggtitle("SNAP Benefits by Age Groups")

# General Health v. SNAP
ggplot(data=FOTM_BL_Sub_1) + stat_summary(aes(x=GENHEALTH, y=SNAP), fun="mean", geom="bar") + ylab("Proportion of Subjects with SNAP Benefits") + xlab("General Health Rating") + ggtitle("SNAP Benefits by General Health Ratings")

# Mental Health v. SNAP
ggplot(data=FOTM_BL_Sub_1) + stat_summary(aes(x=Health_Ment_Weeks, y=SNAP), fun="mean", geom="bar") + ylab("Proportion of Subjects with SNAP Benefits") + xlab("Mental Health") + ggtitle("SNAP Benefits by Mental Health")

# Physical Health v. SNAP
ggplot(data=FOTM_BL_Sub_1) + stat_summary(aes(x=Health_Phys_Weeks, y=SNAP), fun="mean", geom="bar") + ylab("Proportion of Subjects with SNAP Benefits") + xlab("Physical Health") + ggtitle("SNAP Benefits by Physical Health")

# Age v. Mental Health
ggplot(data=FOTM_BL_Sub_1) + stat_summary(aes(x=Age_Groups, y=Health_Ment_Weeks), fun="mean", geom="bar") + ylab("Proportion of Subjects with SNAP Benefits") + xlab("Mental Health") + ggtitle("SNAP Benefits by Mental Health")

################################################################################
# Simple linear regression if explanatory variable is quantitative
################################################################################

race.lm <- lm(GENHEALTH_BL ~ RACE, data = alldata_Sub) 
summary(race.lm)







