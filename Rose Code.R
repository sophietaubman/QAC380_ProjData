# load alldata_Sub data set

# create time variable
alldata_Sub$time.1 <- "Baseline"
alldata_Sub$time.2 <- "6 month"
alldata_Sub$time.3 <- "12 month"

# reshape data
library(reshape2)
library(dplyr)

df <- read.csv("alldata_Sub.csv")
df_new <- reshape(df, varying=list(c("time.1", "time.2", "time.3"),
                                   c("Employment.1", "Employment.2", "Employment.3"),
                                   c("GENHEALTH.1", "GENHEALTH.2", "GENHEALTH.3")),
                  v.names=c("Time", "Employment", "Health"),
                  timevar="Period", times=c(1,2,3), direction="long") %>%
  arrange(LCN) %>%
  select(-X, -id, -Period) # kill unnecessary variables created in the process

# simple boxplot
ggplot(df_new,
       aes(x = factor(Time,level = c("Baseline",
                                     "6 month",
                                     "12 month")),
           y = Health,
           fill=Time)) +
  geom_boxplot(color="black",
               errorbar.draw = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="black", fill="black")+
  # scale_y_continuous(label = dollar) +
  labs(title = "General Health by Assessment Time",
       # subtitle = "for Racial Groups",
       x = "Time",
       y = "General Health") +
  theme_minimal() +
  theme(legend.position = "none")

# fancy boxplot
library(ggplot2)
library(ggpol)
library(scales)
ggplot(df_new,
       aes(x = factor(Time,level = c("Baseline",
                                     "6 month",
                                     "12 month")),
           y = Health,
           fill=Time)) +
  geom_boxjitter(color="black",
                 jitter.color = "darkgrey",
                 errorbar.draw = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="black", fill="black")+
  # scale_y_continuous(label = dollar) +
  labs(title = "General Health by Assessment Time",
       # subtitle = "for Racial Groups",
       x = "Time",
       y = "General Health") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df_new,
       aes(x = factor(Time,level = c("Baseline",
                                     "6 month",
                                     "12 month")),
           y = Health,
           fill=Time)) +
  geom_boxplot(color="black",
               errorbar.draw = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="black", fill="black")+
  # scale_y_continuous(label = dollar) +
  labs(title = "General Health by Assessment Time",
       # subtitle = "for Racial Groups",
       x = "Time",
       y = "General Health") +
  theme_minimal() +
  theme(legend.position = "none")

#grouped box plot######################################
# drop NAs
library(tidyverse)
grpplot1 <- df_new %>%
  drop_na(HISPANIC)


# group by HISPANIC
ggplot(grpplot1,
       aes(x = factor(Time,level = c("Baseline",
                                     "6 month",
                                     "12 month")),
           y = Health,
           fill=Time)) +
  geom_boxplot() +
  # scale_y_continuous(label = dollar) +
  labs(title = "General Health by Assessment Time and Ethnicity",
       # subtitle = "for Racial Groups",
       x = "Time",
       y = "General Health") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~HISPANIC)

# subset to only plot variables using the original wide data subset
emp_subset<-subset(alldata_Sub, select = c(Employment.1,Employment.2))
emp_subset<-na.omit(emp_subset)

library(forcats) # to reorder the factors
empplota <- emp_subset %>%
  count(Employment.1,Employment.2) %>%
  mutate(Employment.1 = fct_relevel(as.factor(Employment.1),
                                    c("Employed","Unemployed","Retired"))) %>%
  mutate(Employment.2 = fct_relevel(as.factor(Employment.2),
                                    c("Employed","Unemployed","Retired")))


library(alluvial)
empplot<-alluvial(empplota %>% select(-n),
                  freq=empplota$n, border=NA, alpha = 0.5,
                  col=case_when(empplota$Employment.1 == "Employed" ~ "turquoise",
                                empplota$Employment.1 == "Unemployed" ~ "cadetblue1",
                                empplota$Employment.1 == "Retired" ~ "skyblue",
                                TRUE ~ "coral1"), cex=0.75, axis_labels = c("Baseline Employment Status",
                                                                            "6 Month Employment Status"),
                  cex.axis = 1.5)
mtext("Changes in employment status baseline - 6 months", 3, line=3, font=2)


# subset to only plot variables using the original wide data subset
genhealth_subset<-subset(alldata_Sub, select = c(GENHEALTH.1,GENHEALTH.2))
genhealth_subset<-na.omit(genhealth_subset)

library(forcats) # to reorder the factors
healthplota <- genhealth_subset %>%
  count(GENHEALTH.1,GENHEALTH.2) %>%
  mutate(GENHEALTH.1 = fct_relevel(as.factor(GENHEALTH.1),
                                    c("Poor","Fair","Good","Very Good","Excellent"))) %>%
  mutate(GENHEALTH.2 = fct_relevel(as.factor(GENHEALTH.2),
                                    c("Poor","Fair","Good","Very Good","Excellent")))
library(alluvial)
healthplot<-alluvial(healthplota %>% select(-n),
                  freq=healthplota$n, border=NA, alpha = 0.5,
                  col=case_when(healthplota$GENHEALTH.1 == "1" ~ "deepskyblue4", 
                                healthplota$GENHEALTH.1 == "2" ~ "deepskyblue3", 
                                healthplota$GENHEALTH.1 == "3" ~ "skyblue", 
                                healthplota$GENHEALTH.1 == "4" ~ "aquamarine2",
                                healthplota$GENHEALTH.1 == "5" ~ "aquamarine3",
                                TRUE ~ "coral1"), cex=0.75, axis_labels = c("Baseline",
                                                                            "6 Month"),
                  cex.axis = 1.5)
mtext("Changes in general health status baseline - 6 months", 3, line=3, font=2)




# categorize
MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY[MASTER_CLEAN_DATA_FOTM$SUM_FS<=1]<-"High/Marginal Food Security"
MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY[MASTER_CLEAN_DATA_FOTM$SUM_FS>=2 & 
                                       MASTER_CLEAN_DATA_FOTM$SUM_FS<=4]<-"Low Food Security"
MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY[MASTER_CLEAN_DATA_FOTM$SUM_FS>4]<-"Very Low Food Security"
freq(MASTER_CLEAN_DATA_FOTM$FOOD_SECURITY, plot=F)


# group by HISPANIC
ggplot(grpplot1,
       aes(x = factor(Time,level = c("Baseline",
                                     "6 month",
                                     "12 month")),
           y = Health,
           fill=Time)) +
  geom_boxplot() +
  # scale_y_continuous(label = dollar) +
  labs(title = "General Health by Assessment Time and Ethnicity",
       # subtitle = "for Racial Groups",
       x = "Time",
       y = "General Health") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~HISPANIC)



# simple boxplot
ggplot(df_new,
       aes(x = factor(Time,level = c("Baseline",
                                     "6 month",
                                     "12 month")),
           y = Health,
           fill=Time)) +
  geom_boxplot(color="black",
               errorbar.draw = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="black", fill="black")+
  # scale_y_continuous(label = dollar) +
  labs(title = "General Health by Assessment Time",
       # subtitle = "for Racial Groups",
       x = "Time",
       y = "General Health") +
  theme_minimal() +
  theme(legend.position = "none")




