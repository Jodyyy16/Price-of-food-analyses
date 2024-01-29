###########################################################################################################################
#############################################CALCULATE PORTION SIZE FROM NDNS DATA#########################################
###########################################################################################################################

library(haven)
library(dplyr)

#Upload STATA files with portion size data of each recorded time an item was consumed
NDNS11 <- read_dta("V:/P8_PHI/DPH/Jody Hoenink/CPI/2022 CPI/Steps to undertake/2. Nutrition data/NDNS data/stata13_se/ndns_rp_yr11a_foodleveldietarydata_uk_20210831.dta")
NDNS10 <- read_dta("V:/P8_PHI/DPH/Jody Hoenink/CPI/2022 CPI/Steps to undertake/2. Nutrition data/NDNS data/stata13_se/ndns_rp_yr10a_foodleveldietarydata_uk_20210831.dta")
NDNS9 <- read_dta("V:/P8_PHI/DPH/Jody Hoenink/CPI/2022 CPI/Steps to undertake/2. Nutrition data/NDNS data/stata13_se/ndns_rp_yr9a_foodleveldietarydata_uk_20210831.dta")

##########################################CALCULATE FREQUENCY OF CONSUMPTION############################################

#Count number of times Foodnumber is observed in the three datasets
NDNS11_count <- table(NDNS11$FoodNumber)
NDNS11_count <- as.data.frame(NDNS11_count)

NDNS10_count <- table(NDNS10$FoodNumber)
NDNS10_count <- as.data.frame(NDNS10_count)

NDNS9_count <- table(NDNS9$FoodNumber)
NDNS9_count <- as.data.frame(NDNS9_count)

NDNS9_count$FoodNumber <- NDNS9_count$Var1
NDNS9_count$Var1 <- NULL

NDNS10_count$FoodNumber <- NDNS10_count$Var1
NDNS10_count$Var1 <- NULL

NDNS11_count$FoodNumber <- NDNS11_count$Var1
NDNS11_count$Var1 <- NULL

NDNS11_count$Freq_11 <- NDNS11_count$Freq
NDNS11_count$Freq <- NULL

NDNS10_count$Freq_10 <- NDNS10_count$Freq
NDNS10_count$Freq <- NULL

NDNS9_count$Freq_9 <- NDNS9_count$Freq
NDNS9_count$Freq <- NULL

#Upload document with food numbers from NDNS and add counts of NDNS 9, 10 and 11 to dataframe
NDNS_comb <- read.csv("V:/P8_PHI/DPH/Jody Hoenink/CPI/2022 CPI/Steps to undertake/2. Nutrition data/Food number 2023-07-04.csv")

NDNS_comb <- merge(NDNS_comb, NDNS9_count, by = "FoodNumber", all.x=TRUE)
NDNS_comb <- merge(NDNS_comb, NDNS10_count, by = "FoodNumber", all.x=TRUE)
NDNS_comb <- merge(NDNS_comb, NDNS11_count, by = "FoodNumber", all.x=TRUE)

# Recode NA to 0 in NDNS_comb dataframe
NDNS_comb[is.na(NDNS_comb)] <- 0

#Combine frequency of consumption and export as excel file
NDNS_comb$'Freq_cons_9-11' <- NDNS_comb$Freq_9 + NDNS_comb$Freq_10+ NDNS_comb$Freq_11

write.csv(NDNS_comb, "V:/P8_PHI/DPH/Jody Hoenink/CPI/2022 CPI/Steps to undertake/2. Nutrition data/Consumption frequency of NDNS 9-11 food items 2023-07-04.csv")


########################################################CALCULATE PORTION SIZE OF FOOD ITEMS#####################################################################
# Calculate mean and median for Total_grams by FoodNumber
Portion_11 <- NDNS11%>%
  group_by(FoodNumber)%>% 
  summarise(Mean=mean(TotalGrams), Median=median(TotalGrams))

Portion_10 <- NDNS10%>%
  group_by(FoodNumber)%>% 
  summarise(Mean=mean(TotalGrams), Median=median(TotalGrams))

Portion_9 <- NDNS9%>%
  group_by(FoodNumber)%>% 
  summarise(Mean=mean(TotalGrams), Median=median(TotalGrams))

Portion_9$Mean_portion_9 <- Portion_9$Mean
Portion_9$Mean <- NULL

Portion_10$Mean_portion_10 <- Portion_10$Mean
Portion_10$Mean <- NULL

Portion_11$Mean_portion_11 <- Portion_11$Mean
Portion_11$Mean <- NULL

Portion_9$Median_portion_9 <- Portion_9$Median
Portion_9$Median <- NULL

Portion_10$Median_portion_10 <- Portion_10$Median
Portion_10$Median <- NULL

Portion_11$Median_portion_11 <- Portion_11$Median
Portion_11$Median <- NULL

# Double check data
subset_data <- NDNS11[NDNS11$FoodNumber == 1952, ]

# Print the subsetted data
subset_data$TotalGrams

# Combine with NDNS frequency data
NDNS_comb <- merge(NDNS_comb, Portion_11, by = "FoodNumber", all.x=TRUE)
NDNS_comb <- merge(NDNS_comb, Portion_10, by = "FoodNumber", all.x=TRUE)
NDNS_comb <- merge(NDNS_comb, Portion_9, by = "FoodNumber", all.x=TRUE)

# Calculate mean of three portion median data, including missing data
NDNS_comb$Median_portion_9_11 <- rowMeans(NDNS_comb[, c("Median_portion_11", "Median_portion_10", "Median_portion_9")], na.rm = TRUE)

# Recode NaN values to missing values
NDNS_comb$Median_portion_9_11[is.nan(NDNS_comb$Median_portion_9_11)] <- NA

##########################################################UPLOAD NDNS NUTRIENT INFORMATION########################################################################
NDNS11_NB <- read_dta("V:/P8_PHI/DPH/Jody Hoenink/CPI/2022 CPI/Steps to undertake/2. Nutrition data/NDNS data/stata13_se/ndns_yr11_nutrientdatabank_2021-03-19.dta")

# Keep only specific columns in NDNS_11NB dataframe
NDNS11_NB <- NDNS11_NB[, c("FoodNumber", "FoodName", "PROT", "FAT", "CHO", "KCALS", "TOTSUG", "SATFA", "NA", "AOAC", "Fruit", "Tomatoes", "OtherVeg", "Nuts", "Brassicaceae", "DriedFruit",	"FruitJuice", "YellowRedGreen")]

# Merge nutrient data with consumption and portion size data
NDNS_comb2 <- merge(NDNS_comb, NDNS11_NB, by="FoodNumber")

NDNS_comb2$Veg <- NDNS_comb2$Tomatoes + NDNS_comb2$OtherVeg + NDNS_comb2$Brassicaceae + NDNS_comb2$YellowRedGreen

NDNS_comb2$Freq_9 <- NULL
NDNS_comb2$Freq_10 <- NULL
NDNS_comb2$Freq_11 <- NULL
NDNS_comb2$Mean_portion_11 <- NULL
NDNS_comb2$Median_portion_11 <- NULL
NDNS_comb2$Mean_portion_10 <- NULL
NDNS_comb2$Median_portion_10 <- NULL
NDNS_comb2$Mean_portion_9 <- NULL
NDNS_comb2$Median_portion_9 <- NULL
NDNS_comb2$OtherVeg <- NULL
NDNS_comb2$Tomatoes <- NULL
NDNS_comb2$Brassicaceae <- NULL
NDNS_comb2$YellowRedGreen <- NULL









