##################################################################################################################################################################
############################################CALCULATE PRICE PER 1000KCAL, PRICE PER 100G AND PRICE PER PORTION####################################################
##################################################################################################################################################################

#Load packages
library(readxl)
library(dplyr)
library(stringr)
library(dplyr)
library(tidyverse)

# Upload list of CPI items linked to NDNS items
NDNS_CPI1 <- read.csv("V:/P8_PHI/DPH/Jody Hoenink/Articles/Price of food/Data/10 year NDNS to CPI food items 2023-07-10.csv")

#Upload CPI information
CPI_info <- read.csv("V:/P8_PHI/DPH/Jody Hoenink/Articles/Price of food/Data/10 year CPI food item data 2023-07-05.csv")

#Merge median price with kcal information
NDNS_CPI2 <- NDNS_CPI1
NDNS_CPI2$FoodName <- NULL
NDNS_CPI2 <- merge(NDNS_CPI2, NDNS_comb2, by = "FoodNumber", all.x=TRUE)

## For calculation of kcals change all 0 to 1
NDNS_CPI2$`Freq_cons_9-11`[NDNS_CPI2$`Freq_cons_9-11` == 0] <- 1

#Some CPI items were linked to just one NDNS item that was not consumed (e.g. basmati rice), as such no portion could be calculated. Use portion size from Irish data for these foods.
NDNS_CPI2[NDNS_CPI2$FoodNumber == 43, "Median_portion_9_11"] <- 173
NDNS_CPI2[NDNS_CPI2$FoodNumber == 1265, "Median_portion_9_11"] <- 78
NDNS_CPI2[NDNS_CPI2$FoodNumber == 7164, "Median_portion_9_11"] <- 284

# Calculate kcal by CPI item
## Calculate the sum of total_consumption for each item_id group
sum_total_consumption <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  summarize(sum_total_consumption = sum(`Freq_cons_9-11`))

## Calculate the sum of kcals * food_consumption for each row with the same item_id
NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_kcals = sum(KCALS * `Freq_cons_9-11`))

## Divide the sum of kcals * food_consumption by the sum of total_consumption within each item_id group
NDNS_CPI2 <- NDNS_CPI2 %>%
  left_join(sum_total_consumption, by = "ITEM_ID") %>%
  mutate(kcal_adj = total_kcals / sum_total_consumption)

# Calculate portion information by CPI item
## Calculate the sum of portion size * food_consumption for each row with the same item_id
NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_portion = sum(Median_portion_9_11 * `Freq_cons_9-11`))

## Divide the sum of portion * food_consumption by the sum of total_consumption within each item_id group
NDNS_CPI2$portion_adj <-NDNS_CPI2$total_portion / NDNS_CPI2$sum_total_consumption

# Calculate sat fat by CPI item
NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_satfat = sum(SATFA * `Freq_cons_9-11`))

NDNS_CPI2$Satfat_adj <-NDNS_CPI2$total_satfat / NDNS_CPI2$sum_total_consumption

# Calculate sugar by CPI item
NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_sugar = sum(TOTSUG * `Freq_cons_9-11`))

NDNS_CPI2$Sugar_adj <-NDNS_CPI2$total_sugar / NDNS_CPI2$sum_total_consumption

# Calculate sodium by CPI item
NDNS_CPI2$SOD <- NDNS_CPI2$'NA' ##NA doesnt work in the code below so need to rename the variable NA

NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_sodium = sum(SOD * `Freq_cons_9-11`))

NDNS_CPI2$Sodium_adj <-NDNS_CPI2$total_sodium / NDNS_CPI2$sum_total_consumption

#Calculate AOAC fibre
NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_fibre_aoac = sum(AOAC * `Freq_cons_9-11`))

NDNS_CPI2$Fibre_aoac_adj <-NDNS_CPI2$total_fibre_aoac / NDNS_CPI2$sum_total_consumption

#Calculate protein
NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_protein = sum(PROT * `Freq_cons_9-11`))

NDNS_CPI2$Protein_adj <-NDNS_CPI2$total_protein / NDNS_CPI2$sum_total_consumption

#Calculate fruit, veg and nuts
## First need to calculate the % of fruit, veg etc.
NDNS_CPI2$Perc_fruit_veg_nuts <- NDNS_CPI2$Fruit + NDNS_CPI2$Nuts + NDNS_CPI2$Veg + NDNS_CPI2$FruitJuice + NDNS_CPI2$DriedFruit

NDNS_CPI2 <- NDNS_CPI2 %>%
  group_by(ITEM_ID) %>%
  mutate(total_fruit_veg_nut = sum(Perc_fruit_veg_nuts * `Freq_cons_9-11`))

NDNS_CPI2$Fruit_veg_nut_adj <-NDNS_CPI2$total_fruit_veg_nut / NDNS_CPI2$sum_total_consumption

# Add price and nutrient data to CPI dataframe
## Subset only kcal and remove duplicates
Nutr_cpi <- select(NDNS_CPI2, ITEM_ID, kcal_adj, portion_adj, Fruit_veg_nut_adj, Protein_adj, Fibre_aoac_adj, Sodium_adj, Sugar_adj, Satfat_adj)
Nutr_cpi <- Nutr_cpi[!duplicated(Nutr_cpi), ]

## Merge dataframes
CPI_price1 <- merge(CPI_info, Nutr_cpi, by = "ITEM_ID", all.x=TRUE)

#Calculate FSO grouping based on nutrient information
##Calculate point for kilojoule
CPI_price1$Kj_adj <- CPI_price1$kcal_adj * 4.184

CPI_price1$Energy_p <- 0
CPI_price1 = data.table::data.table(CPI_price1)
CPI_price1[Kj_adj >355 & Kj_adj<=670, Energy_p := 1]
CPI_price1[Kj_adj >670 & Kj_adj<=1005, Energy_p := 2]
CPI_price1[Kj_adj >1005 & Kj_adj<=1340, Energy_p := 3]
CPI_price1[Kj_adj >1340 & Kj_adj<=1675, Energy_p := 4]
CPI_price1[Kj_adj >1675 & Kj_adj<=2010, Energy_p := 5]
CPI_price1[Kj_adj >2010 & Kj_adj<=2345, Energy_p := 6]
CPI_price1[Kj_adj >2345 & Kj_adj<=2680, Energy_p := 7]
CPI_price1[Kj_adj >2680 & Kj_adj<=3015, Energy_p := 8]
CPI_price1[Kj_adj >3015 & Kj_adj<=3350, Energy_p := 9]
CPI_price1[Kj_adj >3350, Energy_p := 10]

##Saturated Fat
CPI_price1$Satfat_p <- 0
CPI_price1 = data.table::data.table(CPI_price1)
CPI_price1[Satfat_adj >1 & Satfat_adj<=2, Satfat_p := 1]
CPI_price1[Satfat_adj >2 & Satfat_adj<=3, Satfat_p := 2]
CPI_price1[Satfat_adj >3 & Satfat_adj<=4, Satfat_p := 3]
CPI_price1[Satfat_adj >4 & Satfat_adj<=5, Satfat_p := 4]
CPI_price1[Satfat_adj >5 & Satfat_adj<=6, Satfat_p := 5]
CPI_price1[Satfat_adj >6 & Satfat_adj<=7, Satfat_p := 6]
CPI_price1[Satfat_adj >7 & Satfat_adj<=8, Satfat_p := 7]
CPI_price1[Satfat_adj >8 & Satfat_adj<=9, Satfat_p := 8]
CPI_price1[Satfat_adj >9 & Satfat_adj<=10, Satfat_p := 9]
CPI_price1[Satfat_adj >10, Satfat_p := 10]

##Total sugar
CPI_price1[, Sugar_p := findInterval(Sugar_adj, vec = c(4.5, 9, 13.5, 18, 22.5, 27, 31, 36, 40, 45, Inf)) ] #Faster way of scoring

##Sodium
CPI_price1[, Sodium_p := findInterval(Sodium_adj, vec = c(90, 180, 270, 360, 450, 540, 630, 720, 810, 900, Inf)) ] 

##Fruit, veg and nuts
CPI_price1$Fruit_veg_nut_p <- 0
CPI_price1 = data.table::data.table(CPI_price1)
CPI_price1[Fruit_veg_nut_adj >40 & Fruit_veg_nut_adj<=60, Fruit_veg_nut_p := 1]
CPI_price1[Fruit_veg_nut_adj >60 & Fruit_veg_nut_adj<=80, Fruit_veg_nut_p := 2]
CPI_price1[Fruit_veg_nut_adj >80, Fruit_veg_nut_p := 5]

##AOAC fibre
CPI_price1[, Fibre_p := findInterval(Fibre_aoac_adj, vec = c(0.9, 1.9, 2.8, 3.7, 4.7, Inf)) ] 

##Protein
CPI_price1[, Protein_p := findInterval(Protein_adj, vec = c(1.6, 3.2, 4.8, 6.4, 8.0, Inf)) ] 

#Calculate A points
CPI_price1$A_points <- CPI_price1$Energy_p + CPI_price1$Satfat_p + CPI_price1$Sugar_p + CPI_price1$Sodium_p

#If a food or drink scores 11 or more ‘A’ points then it cannot score points for protein unless it also scores 5 points for fruit, vegetables and nuts.
CPI_price1[A_points>=11 & Fruit_veg_nut_p<5, Protein_p:=0]

#Calculate C points
CPI_price1$C_points <- CPI_price1$Protein_p + CPI_price1$Fruit_veg_nut_p + CPI_price1$Fibre_p

#Calculate FSA score
CPI_price1$FSA_score <- CPI_price1$A_points - CPI_price1$C_points

#Categorize foods based off of FSA score
CPI_price1$FSA_category <- as.character(CPI_price1$FSA_category)
CPI_price1$FSA_category <- 'More healthy'
CPI_price1[FSA_score>3 & FOOD_DRINK == "food", FSA_category := "Less healthy"]
CPI_price1[FSA_score>1 & FOOD_DRINK == "drink", FSA_category := "Less healthy"]


# Add price data to nutrient data
CPI_price2 <- merge(CPI_price1, med_price_wide , by= "ITEM_ID")
##No price data was collected for CPI item 211707 DELVD MILK SEMI-SKIMMED PER PT

#############################################################CALCULATE CPI FOOD ITEM PRICES#############################################################################
# Calculate the edible purchased mass
CPI_price2$edible_mass <- CPI_price2$YIELD_FACTOR * CPI_price2$WEIGHT_GR

# Calculate price per 100g for each quarter
CPI_price2$pp_100g_20131 <- (CPI_price2$'20131'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20132 <- (CPI_price2$'20132'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20133 <- (CPI_price2$'20133'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20134 <- (CPI_price2$'20134'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20141 <- (CPI_price2$'20141'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20142 <- (CPI_price2$'20142'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20143 <- (CPI_price2$'20143'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20144 <- (CPI_price2$'20144'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20151 <- (CPI_price2$'20151'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20152 <- (CPI_price2$'20152'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20153 <- (CPI_price2$'20153'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20154 <- (CPI_price2$'20154'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20161 <- (CPI_price2$'20161'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20162 <- (CPI_price2$'20162'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20163 <- (CPI_price2$'20163'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20164 <- (CPI_price2$'20164'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20171 <- (CPI_price2$'20171'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20172 <- (CPI_price2$'20172'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20173 <- (CPI_price2$'20173'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20174 <- (CPI_price2$'20174'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20181 <- (CPI_price2$'20181'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20182 <- (CPI_price2$'20182'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20183 <- (CPI_price2$'20183'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20184 <- (CPI_price2$'20184'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20191 <- (CPI_price2$'20191'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20192 <- (CPI_price2$'20192'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20193 <- (CPI_price2$'20193'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20194 <- (CPI_price2$'20194'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20201 <- (CPI_price2$'20201'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20202 <- (CPI_price2$'20202'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20203 <- (CPI_price2$'20203'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20204 <- (CPI_price2$'20204'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20211 <- (CPI_price2$'20211'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20212 <- (CPI_price2$'20212'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20213 <- (CPI_price2$'20213'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20214 <- (CPI_price2$'20214'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20221 <- (CPI_price2$'20221'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20222 <- (CPI_price2$'20222'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20223 <- (CPI_price2$'20223'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20224 <- (CPI_price2$'20224'/CPI_price2$edible_mass)*100
CPI_price2$pp_100g_20231 <- (CPI_price2$'20231'/CPI_price2$edible_mass)*100

# Calculate mean price per 100kcal
CPI_price2$pp_100kcal_20131 <- CPI_price2$pp_100g_20131/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20132 <- CPI_price2$pp_100g_20132/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20133 <- CPI_price2$pp_100g_20133/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20134 <- CPI_price2$pp_100g_20134/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20141 <- CPI_price2$pp_100g_20141/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20142 <- CPI_price2$pp_100g_20142/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20143 <- CPI_price2$pp_100g_20143/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20144 <- CPI_price2$pp_100g_20144/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20151 <- CPI_price2$pp_100g_20151/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20152 <- CPI_price2$pp_100g_20152/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20153 <- CPI_price2$pp_100g_20153/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20154 <- CPI_price2$pp_100g_20154/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20161 <- CPI_price2$pp_100g_20161/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20162 <- CPI_price2$pp_100g_20162/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20163 <- CPI_price2$pp_100g_20163/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20164 <- CPI_price2$pp_100g_20164/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20171 <- CPI_price2$pp_100g_20171/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20172 <- CPI_price2$pp_100g_20172/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20173 <- CPI_price2$pp_100g_20173/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20174 <- CPI_price2$pp_100g_20174/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20181 <- CPI_price2$pp_100g_20181/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20182 <- CPI_price2$pp_100g_20182/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20183 <- CPI_price2$pp_100g_20183/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20184 <- CPI_price2$pp_100g_20184/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20191 <- CPI_price2$pp_100g_20191/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20192 <- CPI_price2$pp_100g_20192/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20193 <- CPI_price2$pp_100g_20193/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20194 <- CPI_price2$pp_100g_20194/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20201 <- CPI_price2$pp_100g_20201/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20202 <- CPI_price2$pp_100g_20202/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20203 <- CPI_price2$pp_100g_20203/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20204 <- CPI_price2$pp_100g_20204/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20211 <- CPI_price2$pp_100g_20211/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20212 <- CPI_price2$pp_100g_20212/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20213 <- CPI_price2$pp_100g_20213/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20214 <- CPI_price2$pp_100g_20214/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20221 <- CPI_price2$pp_100g_20221/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20222 <- CPI_price2$pp_100g_20222/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20223 <- CPI_price2$pp_100g_20223/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20224 <- CPI_price2$pp_100g_20224/(CPI_price2$kcal_adj/100)
CPI_price2$pp_100kcal_20231 <- CPI_price2$pp_100g_20231/(CPI_price2$kcal_adj/100)


# Calculate mean price per 1000kcal
CPI_price2$pp_1000kcal_20131 <- CPI_price2$pp_100g_20131/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20132 <- CPI_price2$pp_100g_20132/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20133 <- CPI_price2$pp_100g_20133/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20134 <- CPI_price2$pp_100g_20134/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20141 <- CPI_price2$pp_100g_20141/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20142 <- CPI_price2$pp_100g_20142/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20143 <- CPI_price2$pp_100g_20143/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20144 <- CPI_price2$pp_100g_20144/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20151 <- CPI_price2$pp_100g_20151/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20152 <- CPI_price2$pp_100g_20152/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20153 <- CPI_price2$pp_100g_20153/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20154 <- CPI_price2$pp_100g_20154/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20161 <- CPI_price2$pp_100g_20161/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20162 <- CPI_price2$pp_100g_20162/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20163 <- CPI_price2$pp_100g_20163/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20164 <- CPI_price2$pp_100g_20164/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20171 <- CPI_price2$pp_100g_20171/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20172 <- CPI_price2$pp_100g_20172/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20173 <- CPI_price2$pp_100g_20173/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20174 <- CPI_price2$pp_100g_20174/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20181 <- CPI_price2$pp_100g_20181/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20182 <- CPI_price2$pp_100g_20182/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20183 <- CPI_price2$pp_100g_20183/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20184 <- CPI_price2$pp_100g_20184/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20191 <- CPI_price2$pp_100g_20191/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20192 <- CPI_price2$pp_100g_20192/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20193 <- CPI_price2$pp_100g_20193/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20194 <- CPI_price2$pp_100g_20194/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20201 <- CPI_price2$pp_100g_20201/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20202 <- CPI_price2$pp_100g_20202/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20203 <- CPI_price2$pp_100g_20203/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20204 <- CPI_price2$pp_100g_20204/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20211 <- CPI_price2$pp_100g_20211/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20212 <- CPI_price2$pp_100g_20212/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20213 <- CPI_price2$pp_100g_20213/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20214 <- CPI_price2$pp_100g_20214/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20221 <- CPI_price2$pp_100g_20221/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20222 <- CPI_price2$pp_100g_20222/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20223 <- CPI_price2$pp_100g_20223/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20224 <- CPI_price2$pp_100g_20224/(CPI_price2$kcal_adj/1000)
CPI_price2$pp_1000kcal_20231 <- CPI_price2$pp_100g_20231/(CPI_price2$kcal_adj/1000)


# Calculate price per portion
## Calculate edible portion
CPI_price2$edible_portion <- CPI_price2$YIELD_FACTOR * CPI_price2$portion_adj

CPI_price2$pp_portion_20131 <- CPI_price2$pp_100g_20131 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20132 <- CPI_price2$pp_100g_20132 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20133 <- CPI_price2$pp_100g_20133 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20134 <- CPI_price2$pp_100g_20134 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20141 <- CPI_price2$pp_100g_20141 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20142 <- CPI_price2$pp_100g_20142 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20143 <- CPI_price2$pp_100g_20143 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20144 <- CPI_price2$pp_100g_20144 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20151 <- CPI_price2$pp_100g_20151 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20152 <- CPI_price2$pp_100g_20152 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20153 <- CPI_price2$pp_100g_20153 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20154 <- CPI_price2$pp_100g_20154 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20161 <- CPI_price2$pp_100g_20161 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20162 <- CPI_price2$pp_100g_20162 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20163 <- CPI_price2$pp_100g_20163 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20164 <- CPI_price2$pp_100g_20164 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20171 <- CPI_price2$pp_100g_20171 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20172 <- CPI_price2$pp_100g_20172 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20173 <- CPI_price2$pp_100g_20173 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20174 <- CPI_price2$pp_100g_20174 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20181 <- CPI_price2$pp_100g_20181 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20182 <- CPI_price2$pp_100g_20182 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20183 <- CPI_price2$pp_100g_20183 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20184 <- CPI_price2$pp_100g_20184 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20191 <- CPI_price2$pp_100g_20191 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20192 <- CPI_price2$pp_100g_20192 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20193 <- CPI_price2$pp_100g_20193 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20194 <- CPI_price2$pp_100g_20194 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20201 <- CPI_price2$pp_100g_20201 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20202 <- CPI_price2$pp_100g_20202 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20203 <- CPI_price2$pp_100g_20203 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20204 <- CPI_price2$pp_100g_20204 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20211 <- CPI_price2$pp_100g_20211 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20212 <- CPI_price2$pp_100g_20212 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20213 <- CPI_price2$pp_100g_20213 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20214 <- CPI_price2$pp_100g_20214 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20221 <- CPI_price2$pp_100g_20221 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20222 <- CPI_price2$pp_100g_20222 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20223 <- CPI_price2$pp_100g_20223 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20224 <- CPI_price2$pp_100g_20224 * (CPI_price2$edible_portion/100)
CPI_price2$pp_portion_20231 <- CPI_price2$pp_100g_20231 * (CPI_price2$edible_portion/100)


