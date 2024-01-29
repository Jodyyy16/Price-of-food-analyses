##################################################################################################################################################################
###################################################ANALYSES FOR PRICE OF FOOD MANUSCRIPT##########################################################################
##################################################################################################################################################################

#Load packages
library(readxl)
library(dplyr)
library(stringr)
library(dplyr)
library(tidyverse)

# Upload all excel sheets with the quartely median price of CPI 10-year food items
## Set the directory where the Excel files are located
setwd("V:/P8_PHI/DPH/Jody Hoenink/CPI/2023 CPI/2. Calculate median price per quarter/10 year CPI data (i.e. price quotes from ONS website)")

## Get a list of all Excel files in the folder
file_list <- list.files(pattern = "*.csv")

## Create an empty list to store the data from each file
data_list <- list()

## Loop through each file and read the data into a data frame
for (file in file_list) {
  data <- read.csv(file)
  data_list[[file]] <- data
}

## Combine all the data frames into a single data frame
combined_med_price1 <- bind_rows(data_list)

#Remove all item_id's not of relevance
item_ids_to_keep <- c(211021,	211018,	210211,	211411,	211410,	210319,	211507,	210911,	212526,	210912,	212924,	210317,	210606,	212536,	211027,	210415,	210413,	
                      210501,	210502,	211025,	211407,	211022,	211010,	212361,	212311,	212310,	212399,	212309,	212360,	212019,	210115,	211813,	212026,	212021,	
                      213006,	213007,	213001,	213003,	212024,	211905,	212010,	211816,	211901,	211904,	212809,	212729,	210322,	212226,	212227,	212219,	212011,	
                      212727,	212012,	212735,	212716,	212717,	212710,	210808,	210802,	212724,	212603,	212719,	210212,	210416,	210323,	212733,	212223,	210213,	
                      210214,	210219,	211306,	211207,	212807,	211007,	212910,	212608,	212601,	212218,	210215,	211501,	211506,	212217,	211026,	210114,	212939,	
                      211808,	212937,	212936,	212222,	212211,	212938,	212225,	212025,	212023,	211024,	211031,	211016,	211023,	211029,	212923,	210221,	210301,	
                      210324,	211707,	212808,	212934,	210204,	211604,	211603,	211602,	212017,	212015,	210201,	210910,	211815,	211105,	210914,	212530,	212516,	
                      212518,	212504,	212531,	212510,	212511,	212520,	212519,	212532,	212515,	211101,	212016,	210905,	211019,	212928,	210414,	211211,	212405,	
                      211210,	212609,	211106,	212612,	212020,	212730,	212006,	212214,	212001,	212027,	212918,	212715,	212722,	211509,	212945,	210506,	210403,	
                      210507,	210703,	210406,	212107,	213005,	210216,	212933,	211011,	212106,	212712,	210102,	212732,	212008,	212228,	212930,	211028,	211030,	
                      212736,	212731,	211713,	211710,	212202,	212022,	211714,	211408,	212709,	210321,	212806,	211510,	211409,	212718,	212737,	212728,	210302,	
                      212726,	210218,	212402,	212404,	212409,	212408,	212527,	212940,	212613,	212941,	212734,	210217,	210913,	212944,	211014,	211709,	210106,	
                      212725,	211511,	211305,	212720,	212101,	212534,	212535,	212224,	212917,	212942,	212905,	212533,	210111,	210320,	210113,	211807,	211814,	
                      212943)


# Filter the data frame based on the ITEM_ID values
combined_med_price2 <- combined_med_price1[combined_med_price1$ITEM_ID %in% item_ids_to_keep, c("QUOTE_DATE", "ITEM_ID", "ITEM_DESC", "VALIDITY", "PRICE")]

# Only keep prices with validity of 3 and higher
combined_med_price2 <- combined_med_price2[combined_med_price2$VALIDITY > 2, ]

# Split the date into the year and the month
combined_med_price2$YEAR <- substr(combined_med_price2$QUOTE_DATE, 1, 4)
combined_med_price2$MONTH <- substr(combined_med_price2$QUOTE_DATE, 5, 6)

combined_med_price2$YEAR <- as.numeric(combined_med_price2$YEAR)
combined_med_price2$MONTH <- as.numeric(combined_med_price2$MONTH)

# Make quarter variable
combined_med_price2$QUARTER <- ifelse(combined_med_price2$MONTH %in% c(1, 2, 3), 1,
                                                                     ifelse(combined_med_price2$MONTH %in% c(4, 5, 6), 2,
                                                                            ifelse(combined_med_price2$MONTH %in% c(7, 8, 9), 3, 4)))

# Calculate the median price per food item per quote date
median_prices1 <- combined_med_price2 %>%
  group_by(YEAR, QUARTER, ITEM_ID) %>%
  summarise(median_price = median(PRICE))

median_prices1$YQ <- paste0(median_prices1$YEAR, median_prices1$QUARTER)

# Transform dataframe from long to wide format
median_prices2 <- median_prices1
median_prices2$YEAR <- NULL
median_prices2$QUARTER <- NULL

med_price_wide <- pivot_wider(median_prices2,
                              names_from = YQ,
                              values_from = median_price)

#No prices were collected in the first two quarters of 2019. As such, the prices of the last quarter of 2018 are used instead.
med_price_wide$'20191' <- med_price_wide$'20184'
med_price_wide$'20192' <- med_price_wide$'20184'

#Also, no price was collected for rotiserie chicken in 2021 Q1, impute 2020 Q4 data instead
med_price_wide$`20211`[med_price_wide$ITEM_ID == 210913] <- 4.97

#Export data
write.csv(med_price_wide, "V:/P8_PHI/DPH/Jody Hoenink/Articles/Price of food/Data/Median price of foods 2013-2023 2023-07-10.csv")



















