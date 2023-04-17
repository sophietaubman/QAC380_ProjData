library(readxl)
MASTER_CLEAN_DATA_FOTM <- read_excel("/Volumes/qac380/Data and Codebooks 2023/RIPHI/FOTM Data/MASTER CLEAN DATA FOTM.xlsx")
View(MASTER_CLEAN_DATA_FOTM)

freq_sub_hs <- MASTER_CLEAN_DATA_FOTM[,c("Loyalty card number", "SNAP", "HUNGRY")]
View(freq_sub_hs)

freq_sub_hs <- freq_sub_hs[-1,]
freq_sub_hs <- freq_sub_hs[-125,]
freq_sub_hs <- freq_sub_hs[-132,]
freq_sub_hs <- freq_sub_hs[-158,]


freq_sub_hs$SNAP[freq_sub_hs$SNAP == "DON'T KNOW"] <- NA
freq_sub_hs$SNAP[freq_sub_hs$SNAP == "REFUSED"] <- NA

freq_sub_hs$HUNGRY[freq_sub_hs$HUNGRY == "DON'T KNOW"] <- NA
freq_sub_hs$HUNGRY[freq_sub_hs$HUNGRY == "REFUSED"] <- NA

# freq_sub_hs$SNAP[freq_sub_hs$SNAP == "YES"] <- 1
# freq_sub_hs$SNAP[freq_sub_hs$SNAP == "NO"] <- 0
# 
# freq_sub_hs$HUNGRY[freq_sub_hs$HUNGRY == "YES"] <- 1
# freq_sub_hs$HUNGRY[freq_sub_hs$HUNGRY == "NO"] <- 0

tab1 <- table(FOTM_BL_Sub_1$HUNGRY, FOTM_BL_Sub_1$SNAP)
tab1_colprop <- prop.table(tab1, 2)
tab1_rowprop <- prop.table(tab1, 1)
tab1_cellprop <- prop.table(tab1)
# view(tab1)
# view(tab1_colprop)
# view(tab1_rowprop)
# view(tab1_cellprop)

chi_sqr <- chisq.test(freq_sub_hs$HUNGRY, freq_sub_hs$SNAP)
table <- chi_sqr$observed
View(table)

HS <- table(freq_sub_hs$HUNGRY, freq_sub_hs$SNAP)
summary(HS)
prop.table(HS)
View(HS)

