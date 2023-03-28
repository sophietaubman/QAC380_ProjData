# Uploading Data Set
MASTER_CLEAN_DATA_FOTM <- read_excel("/Volumes/qac380/Data and Codebooks 2023/RIPHI/FOTM Data/MASTER CLEAN DATA FOTM.xlsx")
View(MASTER_CLEAN_DATA_FOTM)

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
# Univariate Graphs for Each Variable of Interest 
################################################################################

# AGE # 

# Univariate Bar Graph:

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(Age)))+ggtitle("Age")

# Univariate Histogram: 

ggplot(FOTM_BL_Sub_1, aes(Age)) + geom_histogram(binwidth = 5) + ggtitle("Age Distribution") + 
  ylab("Number of People")

################################################################################

# PHYSICAL HEALTH #

# Bar graph ordered by number of poor health days 0 to 30

ggplot(data=FOTM_BL_Sub_1) + geom_bar(aes(x=factor(HEALTH_PHYS, level=c
                                                   ("0","1","2","3","4","5","6","7","8","9","10",
                                                     "11","12","13","14","15","16","17","18","19","20",
                                                     "21","22","23","24","25","26","27","28","29","30")))) + 
  xlab("# of Days") + ylab("# of People") + ggtitle("# of Days of Poor Health in Past 30 Days")

################################################################################

# MENTAL HEALTH #

# Bar graph ordered by number of poor mental health days 0 to 30

ggplot(data=FOTM_BL_Sub_1) + geom_bar(aes(x=factor(HEALTH_MENTAL, level=c
                                                   ("0","1","2","3","4","5","6","7","8","9","10",
                                                     "11","12","13","14","15","16","17","18","19","20",
                                                     "21","22","23","24","25","26","27","28","29","30")))) + 
  xlab("# of Days") + ylab("# of People") + ggtitle("# of Days of Poor Mental Health in Past 30 Days")

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

################################################################################

# DISABLED #

# Data looks ugly, should be reordered #

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(DISABLED)))+ggtitle("Disabled") + xlab("") + ylab("# of People") 

################################################################################

# GENERAL HEALTH #

# Clean Up Labels Later... 

ggplot(data=FOTM_BL_Sub_1)+geom_bar(aes(x=factor(GENHEALTH)))+ggtitle("General Health Rating") + xlab("") + ylab("# of People") 

################################################################################
# PERCENTAGE SUMMARY FOR DATAFRAME LEVELS #
# gender 
FOTM_BL_Sub_1 %>% group_by(GENDER) %>% summarise(Percentage=n()/nrow(.))
# race 
FOTM_BL_Sub_1 %>% group_by(RACE) %>% summarise(Percentage=n()/nrow(.))
# hispanic 
FOTM_BL_Sub_1 %>% group_by(HISPANIC) %>% summarise(Percentage=n()/nrow(.))
# insurance
FOTM_BL_Sub_1 %>% group_by(INSURANCEYN) %>% summarise(Percentage=n()/nrow(.))
# medicare
FOTM_BL_Sub_1 %>% group_by(MEDICARE) %>% summarise(Percentage=n()/nrow(.))
# medicaid
FOTM_BL_Sub_1 %>% group_by(MEDICAID) %>% summarise(Percentage=n()/nrow(.))
# employment status
FOTM_BL_Sub_1 %>% group_by(EMPLOYED) %>% summarise(Percentage=n()/nrow(.))
# retirement
FOTM_BL_Sub_1 %>% group_by(RETIRED) %>% summarise(Percentage=n()/nrow(.))
# disability
FOTM_BL_Sub_1 %>% group_by(DISABLED) %>% summarise(Percentage=n()/nrow(.))
# snap
FOTM_BL_Sub_1 %>% group_by(SNAP) %>% summarise(Percentage=n()/nrow(.))
# genhealth
FOTM_BL_Sub_1 %>% group_by(GENHEALTH) %>% summarise(Percentage=n()/nrow(.))

################################################################################
