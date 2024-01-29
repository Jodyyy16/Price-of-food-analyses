#######################################################################################################################################################################
##################################################################RUN MAIN PRICE/100KCAL ANALYSES FOR MANUSCRIPT##########################################################################
#######################################################################################################################################################################

library(tidyverse)
library(ggplot2)
library(rstatix)
library(lme4)
library(tidyr)
library(RColorBrewer)
library(lme4)

# Check if there are any outliers in the data for 
##For price per 100kcal
mean2 <- mean(CPI_price2$pp_100kcal_20231)
sd2 <- sd(CPI_price2$pp_100kcal_20231)
mean2 - (sd2 * 3)
mean2 + (sd2 * 3)

subset(CPI_price2, pp_100kcal_20231 > 4.019)
###Outliers are 212520 mushroom and 212217 chewing gum

#Remove outliers from datasets
Excl_outlier_100kcal <- CPI_price2[!(CPI_price2$ITEM_ID %in% c(212520, 212217)), ]

# Calculate the mean price per 100 kcal by eatwell category
pp_100kcal_eatwell <- aggregate(cbind(pp_100kcal_20131, pp_100kcal_20132, pp_100kcal_20133, pp_100kcal_20134,
                                      pp_100kcal_20141, pp_100kcal_20142, pp_100kcal_20143, pp_100kcal_20144,
                                      pp_100kcal_20151, pp_100kcal_20152, pp_100kcal_20153, pp_100kcal_20154,
                                      pp_100kcal_20161, pp_100kcal_20162, pp_100kcal_20163, pp_100kcal_20164,
                                      pp_100kcal_20171, pp_100kcal_20172, pp_100kcal_20173, pp_100kcal_20174,
                                      pp_100kcal_20181, pp_100kcal_20182, pp_100kcal_20183, pp_100kcal_20184,
                                      pp_100kcal_20191, pp_100kcal_20192, pp_100kcal_20193, pp_100kcal_20194,
                                      pp_100kcal_20201, pp_100kcal_20202, pp_100kcal_20203, pp_100kcal_20204,
                                      pp_100kcal_20211, pp_100kcal_20212, pp_100kcal_20213, pp_100kcal_20214,
                                      pp_100kcal_20221, pp_100kcal_20222, pp_100kcal_20223, pp_100kcal_20224,
                                      pp_100kcal_20231) ~ EATWELL_CAT, data = CPI_price2, FUN = mean)

pp_100kcal_eatwell_excl <- aggregate(cbind(pp_100kcal_20131, pp_100kcal_20132, pp_100kcal_20133, pp_100kcal_20134,
                                           pp_100kcal_20141, pp_100kcal_20142, pp_100kcal_20143, pp_100kcal_20144,
                                           pp_100kcal_20151, pp_100kcal_20152, pp_100kcal_20153, pp_100kcal_20154,
                                           pp_100kcal_20161, pp_100kcal_20162, pp_100kcal_20163, pp_100kcal_20164,
                                           pp_100kcal_20171, pp_100kcal_20172, pp_100kcal_20173, pp_100kcal_20174,
                                           pp_100kcal_20181, pp_100kcal_20182, pp_100kcal_20183, pp_100kcal_20184,
                                           pp_100kcal_20191, pp_100kcal_20192, pp_100kcal_20193, pp_100kcal_20194,
                                           pp_100kcal_20201, pp_100kcal_20202, pp_100kcal_20203, pp_100kcal_20204,
                                           pp_100kcal_20211, pp_100kcal_20212, pp_100kcal_20213, pp_100kcal_20214,
                                           pp_100kcal_20221, pp_100kcal_20222, pp_100kcal_20223, pp_100kcal_20224,
                                           pp_100kcal_20231) ~ EATWELL_CAT, data = Excl_outlier_100kcal, FUN = mean)

# Calculate the mean price per 100 kcal by FSA category
pp_100kcal_fsa <- aggregate(cbind(pp_100kcal_20131, pp_100kcal_20132, pp_100kcal_20133, pp_100kcal_20134,
                                      pp_100kcal_20141, pp_100kcal_20142, pp_100kcal_20143, pp_100kcal_20144,
                                      pp_100kcal_20151, pp_100kcal_20152, pp_100kcal_20153, pp_100kcal_20154,
                                      pp_100kcal_20161, pp_100kcal_20162, pp_100kcal_20163, pp_100kcal_20164,
                                      pp_100kcal_20171, pp_100kcal_20172, pp_100kcal_20173, pp_100kcal_20174,
                                      pp_100kcal_20181, pp_100kcal_20182, pp_100kcal_20183, pp_100kcal_20184,
                                      pp_100kcal_20191, pp_100kcal_20192, pp_100kcal_20193, pp_100kcal_20194,
                                      pp_100kcal_20201, pp_100kcal_20202, pp_100kcal_20203, pp_100kcal_20204,
                                      pp_100kcal_20211, pp_100kcal_20212, pp_100kcal_20213, pp_100kcal_20214,
                                      pp_100kcal_20221, pp_100kcal_20222, pp_100kcal_20223, pp_100kcal_20224,
                                      pp_100kcal_20231) ~ FSA_category, data = CPI_price2, FUN = mean)

pp_100kcal_fsa_excl <- aggregate(cbind(pp_100kcal_20131, pp_100kcal_20132, pp_100kcal_20133, pp_100kcal_20134,
                                           pp_100kcal_20141, pp_100kcal_20142, pp_100kcal_20143, pp_100kcal_20144,
                                           pp_100kcal_20151, pp_100kcal_20152, pp_100kcal_20153, pp_100kcal_20154,
                                           pp_100kcal_20161, pp_100kcal_20162, pp_100kcal_20163, pp_100kcal_20164,
                                           pp_100kcal_20171, pp_100kcal_20172, pp_100kcal_20173, pp_100kcal_20174,
                                           pp_100kcal_20181, pp_100kcal_20182, pp_100kcal_20183, pp_100kcal_20184,
                                           pp_100kcal_20191, pp_100kcal_20192, pp_100kcal_20193, pp_100kcal_20194,
                                           pp_100kcal_20201, pp_100kcal_20202, pp_100kcal_20203, pp_100kcal_20204,
                                           pp_100kcal_20211, pp_100kcal_20212, pp_100kcal_20213, pp_100kcal_20214,
                                           pp_100kcal_20221, pp_100kcal_20222, pp_100kcal_20223, pp_100kcal_20224,
                                           pp_100kcal_20231) ~ FSA_category, data = Excl_outlier_100kcal, FUN = mean)

#Summarize price/100kcal data excluding outliers
##Q1 of 2023
summary(Excl_outlier_100kcal$pp_100kcal_20231)
sd(Excl_outlier_100kcal$pp_100kcal_20231)

###By Eatwell and fsa category
aggregate(Excl_outlier_100kcal$pp_100kcal_20231, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))

aggregate(Excl_outlier_100kcal$pp_100kcal_20231, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))

##Q1 of 2013
summary(Excl_outlier_100kcal$pp_100kcal_20131)
sd(Excl_outlier_100kcal$pp_100kcal_20131)

summary(Excl_outlier_100kcal$pp_100kcal_20213)
sd(Excl_outlier_100kcal$pp_100kcal_20213)

###By Eatwell and fsa category
aggregate(Excl_outlier_100kcal$pp_100kcal_20131, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))

aggregate(Excl_outlier_100kcal$pp_100kcal_20131, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))


#Calculate the absolute change in price
Excl_outlier_100kcal$abs_10y <- Excl_outlier_100kcal$pp_100kcal_20231 - Excl_outlier_100kcal$pp_100kcal_20131
Excl_outlier_100kcal$abs_pre_COL <- Excl_outlier_100kcal$pp_100kcal_20213 - Excl_outlier_100kcal$pp_100kcal_20131
Excl_outlier_100kcal$abs_post_COL <- Excl_outlier_100kcal$pp_100kcal_20231 - Excl_outlier_100kcal$pp_100kcal_20214

summary(Excl_outlier_100kcal$abs_10y, na.rm=TRUE)
IQR(Excl_outlier_100kcal$abs_10y, na.rm=TRUE)
summary(Excl_outlier_100kcal$abs_pre_COL, na.rm=TRUE)
IQR(Excl_outlier_100kcal$abs_pre_COL, na.rm=TRUE)
summary(Excl_outlier_100kcal$abs_post_COL, na.rm=TRUE)
IQR(Excl_outlier_100kcal$abs_post_COL, na.rm=TRUE)

aggregate(Excl_outlier_100kcal$abs_10y, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$abs_pre_COL, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$abs_post_COL, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(median = median(x),IQR = IQR(x), mean = mean(x), sd = sd(x)))

aggregate(Excl_outlier_100kcal$abs_10y, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$abs_pre_COL, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(median = median(x), IQR = IQR(x), mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$abs_post_COL, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(median = median(x),IQR = IQR(x), mean = mean(x), sd = sd(x)))


#Calculate the relative change in price
Excl_outlier_100kcal$rel_10y <- (Excl_outlier_100kcal$pp_100kcal_20231 - Excl_outlier_100kcal$pp_100kcal_20131)/ Excl_outlier_100kcal$pp_100kcal_20131
Excl_outlier_100kcal$rel_pre_COL <- (Excl_outlier_100kcal$pp_100kcal_20213 - Excl_outlier_100kcal$pp_100kcal_20131)/ Excl_outlier_100kcal$pp_100kcal_20131
Excl_outlier_100kcal$rel_post_COL <- (Excl_outlier_100kcal$pp_100kcal_20231 - Excl_outlier_100kcal$pp_100kcal_20214)/ Excl_outlier_100kcal$pp_100kcal_20214

summary(Excl_outlier_100kcal$rel_10y)
sd(Excl_outlier_100kcal$rel_10y)
summary(Excl_outlier_100kcal$rel_pre_COL)
sd(Excl_outlier_100kcal$rel_pre_COL)
summary(Excl_outlier_100kcal$rel_post_COL)
sd(Excl_outlier_100kcal$rel_post_COL)

aggregate(Excl_outlier_100kcal$rel_10y, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$rel_pre_COL, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$rel_post_COL, by = list(Excl_outlier_100kcal$EATWELL_CAT), FUN = function(x) c(mean = mean(x), sd = sd(x)))

aggregate(Excl_outlier_100kcal$rel_10y, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$rel_pre_COL, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(Excl_outlier_100kcal$rel_post_COL, by = list(Excl_outlier_100kcal$FSA_category), FUN = function(x) c(mean = mean(x), sd = sd(x)))


##############################################################RUN ANOVA ANALYSES FOR EATWELL CATEGORY################################################################################
#Run anova to see if the prices of eatwell and fsa categories in 2023 differ statistically significantly
stat_diff_eatwell <- aov(pp_100kcal_20231 ~ EATWELL_CAT, data = Excl_outlier_100kcal)
summary(stat_diff_eatwell)

stat_diff_eatwell <- aov(pp_100kcal_20231 ~ FSA_category, data = Excl_outlier_100kcal)
summary(stat_diff_eatwell)

#Run repeated measures anova model to see if the prices of all foods differ stat sign through time
##Tranform dataframe from wide to long
### First remove unnecessary columns
Excl_outlier_100kcal1 <- Excl_outlier_100kcal
Excl_outlier_100kcal1$WEIGHT_GR <- NULL
Excl_outlier_100kcal1$YIELD_FACTOR <- NULL
Excl_outlier_100kcal1$FOOD_DRINK <-  NULL
Excl_outlier_100kcal1$kcal_adj <- NULL
Excl_outlier_100kcal1$portion_adj <- NULL
Excl_outlier_100kcal1$edible_mass <- NULL
Excl_outlier_100kcal1$edible_portion <- NULL
Excl_outlier_100kcal1$abs_10y <- NULL
Excl_outlier_100kcal1$abs_pre_COL <- NULL
Excl_outlier_100kcal1$abs_post_COL <- NULL
Excl_outlier_100kcal1$rel_10y <- NULL
Excl_outlier_100kcal1$rel_pre_COL <- NULL
Excl_outlier_100kcal1$rel_post_COL <- NULL
Excl_outlier_100kcal1$A_points <- NULL
Excl_outlier_100kcal1$C_points <- NULL
Excl_outlier_100kcal1$Protein_adj <- NULL
Excl_outlier_100kcal1$Protein_p <- NULL
Excl_outlier_100kcal1$Sugar_adj <- NULL
Excl_outlier_100kcal1$Sugar_p <- NULL
Excl_outlier_100kcal1$Satfat_adj <- NULL
Excl_outlier_100kcal1$Satfat_p <- NULL
Excl_outlier_100kcal1$Sodium_adj <- NULL
Excl_outlier_100kcal1$Sodium_p <- NULL
Excl_outlier_100kcal1$Fruit_veg_nut_adj <- NULL
Excl_outlier_100kcal1$Fruit_veg_nut_p <- NULL
Excl_outlier_100kcal1$Fibre_aoac_adj <- NULL
Excl_outlier_100kcal1$Fibre_p <- NULL
Excl_outlier_100kcal1$Kj_adj <- NULL
Excl_outlier_100kcal1$Energy_p <- NULL
Excl_outlier_100kcal1$FSA_score <- NULL

Excl_outlier_100kcal1 <- Excl_outlier_100kcal1 %>%
  select(-starts_with("20"))
Excl_outlier_100kcal1 <- Excl_outlier_100kcal1 %>%
  select(-starts_with("pp_100g"))
Excl_outlier_100kcal1 <- Excl_outlier_100kcal1 %>%
  select(-starts_with("pp_portion"))
Excl_outlier_100kcal1 <- Excl_outlier_100kcal1 %>%
  select(-starts_with("pp_1000kcal"))

### Transform from wide to long format
Long_100kcal <- Excl_outlier_100kcal1 %>%
  pivot_longer(cols = starts_with("pp_100kcal"), # Select columns starting with 'pp_100kcal'
               names_to = "YQ", # Name of the new column for variable names
               values_to = "Price_100kcal") %>% # Name of the new column for variable values
  arrange(ITEM_ID) # Sort the dataframe by ITEM_ID

## Remove the first section of rows in 'YQ'
Long_100kcal$YQ <- gsub("^pp_100kcal_", "", Long_100kcal$YQ)

## Run repeated measures ANOVA to see if there was a statistically significant change in price over the period 2013-2023 for all foods 
res.aov <- anova_test(data = Long_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
###There is indeed a statistically significant difference


# Run repeated measures ANOVA separately for each EATWELL category to see if the prices differed over time
Bread_100kcal <- subset(Long_100kcal, Eatwell_num == 1)
HFSS_100kcal <- subset(Long_100kcal, Eatwell_num == 2)
Meat_100kcal <- subset(Long_100kcal, Eatwell_num == 3)
Milk_100kcal <- subset(Long_100kcal, Eatwell_num == 4)
Fruit_100kcal <- subset(Long_100kcal, Eatwell_num == 5)

res.aov <- anova_test(data = Bread_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
##No stat sign difference over time

res.aov <- anova_test(data = HFSS_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
##Stat sign difference over time

res.aov <- anova_test(data = Meat_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
##Stat sign difference over time

res.aov <- anova_test(data = Milk_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
##Stat sign difference over time

res.aov <- anova_test(data = Fruit_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
##Stat sign difference over time


# Run repeated measures ANOVA separately for each FSA category to see if the prices differed over time
Lesshealthy_100kcal <- subset(Long_100kcal, FSA_category == 'Less healthy')
Morehealthy_100kcal <- subset(Long_100kcal, FSA_category == 'More healthy')

res.aov <- anova_test(data = Lesshealthy_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
##Stat sign difference over time

res.aov <- anova_test(data = Morehealthy_100kcal, dv = Price_100kcal, wid = ITEM_ID, within = YQ)
get_anova_table(res.aov)
##Stat sign difference over time


######################################Based on reviewer comments, use mixed model to assess if time developments differed by groups before and after inflationary period as well as during the entire time period#############################

# First categorize FSA category into 0 and 1
Long_100kcal$FSA_num <- ifelse(Long_100kcal$FSA_category == 'More healthy', 0, 1)

# Remove quarter from YQ variable to increase the B from mixed model
## Ensure that YQ is a character string
Long_100kcal$Year <- as.character(Long_100kcal$YQ)

## Remove the last two digits
Long_100kcal$Year <- substr(Long_100kcal$Year, 1, nchar(Long_100kcal$Year) - 1)

Long_100kcal$Year <- as.numeric(Long_100kcal$Year)

# Different reference group Eatwell food groups
Long_100kcal$Eatwell_num_ref1 <- with(Long_100kcal, 
                                      ifelse(Eatwell_num == 1, 5, 
                                             ifelse(Eatwell_num == 2, 4, 
                                                    ifelse(Eatwell_num == 3, 3, 
                                                           ifelse(Eatwell_num == 4, 2,
                                                                  ifelse(Eatwell_num == 5, 1, Eatwell_num))))))



Long_100kcal$Eatwell_num_ref2 <- with(Long_100kcal, 
                                 ifelse(Eatwell_num == 3, 1, 
                                    ifelse(Eatwell_num == 4, 2, 
                                        ifelse(Eatwell_num == 5, 3, 
                                            ifelse(Eatwell_num == 1, 4,
                                                ifelse(Eatwell_num == 2, 5, Eatwell_num))))))


Long_100kcal$Eatwell_num_ref3 <- with(Long_100kcal, 
                                      ifelse(Eatwell_num == 2, 1, 
                                             ifelse(Eatwell_num == 3, 2, 
                                                    ifelse(Eatwell_num == 4, 3, 
                                                           ifelse(Eatwell_num == 5, 4,
                                                                  ifelse(Eatwell_num == 1, 5, Eatwell_num))))))

Long_100kcal$Eatwell_num_ref4 <- with(Long_100kcal, 
                                      ifelse(Eatwell_num == 4, 1, 
                                             ifelse(Eatwell_num == 3, 2, 
                                                    ifelse(Eatwell_num == 2, 3, 
                                                           ifelse(Eatwell_num == 1, 4,
                                                                  ifelse(Eatwell_num == 5, 5, Eatwell_num))))))


# Fit the mixed-effects model with interaction terms by Eatwell category
## First display the Eatwell cat and the corresponding numeric category for interpretation of results
unique(Long_100kcal[, c("EATWELL_CAT", "Eatwell_num")])

model <- lmer(Price_100kcal ~ YQ * Eatwell_num + (1|ITEM_ID), data = Long_100kcal)
summary(model)
confint(model)
###F&V = 5, bread = ref, HFSS = 2, meat = 3 and dairy = 4
###The price development over time of HFSS and meat differ stat sign from bread. Dairy and F&V do not differ stat sign from bread

## Rerun mixed model with different reference group
Long_100kcal$Eatwell_num_ref1 <- as.factor (Long_100kcal$Eatwell_num_ref1)
unique(Long_100kcal[, c("EATWELL_CAT", "Eatwell_num_ref1")])

model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref1 + (1|ITEM_ID), data = Long_100kcal)
summary(model)
confint(model)
###F&V = ref, bread = 5, HFSS = 4, meat = 3 and dairy = 2
###The price development of HFSS and meat differ stat sign from F&V. No other stat sign differences were found

## Rerun mixed model with different reference group
Long_100kcal$Eatwell_num_ref2 <- as.factor (Long_100kcal$Eatwell_num_ref2)
unique(Long_100kcal[, c("EATWELL_CAT", "Eatwell_num_ref2")])

model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref2 + (1|ITEM_ID), data = Long_100kcal)
summary(model)
confint(model)
###F&V = 3, bread = 4, HFSS = 5, meat = ref and dairy = 2
###All food groups differ stat sign from the price development of meat

## Rerun mixed model with different reference group
Long_100kcal$Eatwell_num_ref3 <- as.factor (Long_100kcal$Eatwell_num_ref3)
unique(Long_100kcal[, c("EATWELL_CAT", "Eatwell_num_ref3")])

model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref3 + (1|ITEM_ID), data = Long_100kcal)
summary(model)
confint(model)
###F&V = 4, bread = 5, HFSS = ref, meat = 2 and dairy = 3
###All food groups differ stat sign from the price development of fruit and vegetables

## Rerun mixed model with different reference group
Long_100kcal$Eatwell_num_ref4 <- as.factor (Long_100kcal$Eatwell_num_ref4)
unique(Long_100kcal[, c("EATWELL_CAT", "Eatwell_num_ref4")])

model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref4 + (1|ITEM_ID), data = Long_100kcal)
summary(model)
confint(model)
###F&V = 5, bread = 4, HFSS = 3, meat = 2 and dairy = ref
###The price development of meat and HFSS differ stat sign from dairy. No other stat sign differences were found.


# Fit the mixed-effects model with interaction terms by healthiness category
## First display the FSA cat and the corresponding numeric category for interpretation of results
unique(Long_100kcal[, c("FSA_category", "FSA_num")])

model <- lmer(Price_100kcal ~ YQ * FSA_num + (1|ITEM_ID), data = Long_100kcal)
summary(model)
confint(model)


# Now assess differences before and during cost-of-living period
## Develop a variable for before and during cost-of-living crisis period
Long_100kcal$CoL_period <- ifelse(Long_100kcal$YQ <= 20213, 0, 1)

## Make two new dataframes, before and during cost-of-living pressure
Long_100kcal_before_COL <- subset(Long_100kcal, CoL_period == 0)
Long_100kcal_during_COL <- subset(Long_100kcal, CoL_period == 1)

## Fit the mixed-effects model with interaction terms by Eatwell category
unique(Long_100kcal_before_COL[, c("EATWELL_CAT", "Eatwell_num")]) #First display the Eatwell cat and the corresponding numeric category for interpretation of results

### Before CoL
model <- lmer(Price_100kcal ~ YQ * Eatwell_num + (1|ITEM_ID), data = Long_100kcal_before_COL)
summary(model)
confint(model)
###F&V = 5, bread = ref, HFSS = 2, meat = 3 and dairy = 4
###Bread differed stat sign from fruit, no other stat sign differences were found.

### During CoL
model <- lmer(Price_100kcal ~ YQ * Eatwell_num + (1|ITEM_ID), data = Long_100kcal_during_COL)
summary(model)
confint(model)
###F&V = 5, bread = ref, HFSS = 2, meat = 3 and dairy = 4
###Price developments of F&V and meat differ stat sign from bread with no other differences

## Rerun mixed model with different reference group
unique(Long_100kcal_before_COL[, c("EATWELL_CAT", "Eatwell_num_ref1")])

### Before CoL
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref1 + (1|ITEM_ID), data = Long_100kcal_before_COL)
summary(model)
confint(model)
###F&V = ref, bread = 5, HFSS = 4, meat = 3 and dairy = 2
###F&V differed stat sign from all other food groups

### During CoL
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref1 + (1|ITEM_ID), data = Long_100kcal_during_COL)
summary(model)
confint(model)
###F&V = ref, bread = 5, HFSS = 4, meat = 3 and dairy = 2
###The price development of HFSS and bread differ stat sign from F&V. No other stat sign differences were found

## Rerun mixed model with different reference group
unique(Long_100kcal_before_COL[, c("EATWELL_CAT", "Eatwell_num_ref2")])

### Before CoL
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref2 + (1|ITEM_ID), data = Long_100kcal_before_COL)
summary(model)
confint(model)
###F&V = 3, bread = 4, HFSS = 5, meat = ref and dairy = 2
###F&V and dairy differed stat sign from meat before CoL period

### During CoL
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref2 + (1|ITEM_ID), data = Long_100kcal_during_COL)
summary(model)
confint(model)
###F&V = 3, bread = 4, HFSS = 5, meat = ref and dairy = 2
###Dairy and F&V differed stat sign from meat after CoL period

## Rerun mixed model with different reference group
unique(Long_100kcal_before_COL[, c("EATWELL_CAT", "Eatwell_num_ref3")])

### Before CoL period
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref3 + (1|ITEM_ID), data = Long_100kcal_before_COL)
summary(model)
confint(model)
###F&V = 4, bread = 5, HFSS = ref, meat = 2 and dairy = 3
###HFSS differs from F&V and dairy before COL

### During CoL period
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref3 + (1|ITEM_ID), data = Long_100kcal_during_COL)
summary(model)
confint(model)
###F&V = 4, bread = 5, HFSS = ref, meat = 2 and dairy = 3
###HFSS differs from meat and F&V after COL

## Rerun mixed model with different reference group
unique(Long_100kcal[, c("EATWELL_CAT", "Eatwell_num_ref4")])

### Before CoL period
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref4 + (1|ITEM_ID), data = Long_100kcal_before_COL)
summary(model)
confint(model)
###F&V = 5, bread = 4, HFSS = 3, meat = 2 and dairy = ref
###The price development of meat, HFSS and F&V differed stat sign

### During CoL period
model <- lmer(Price_100kcal ~ YQ * Eatwell_num_ref4 + (1|ITEM_ID), data = Long_100kcal_during_COL)
summary(model)
confint(model)
###F&V = 5, bread = 4, HFSS = 3, meat = 2 and dairy = ref
###The price development of meat and HFSS differ stat sign from dairy. No other stat sign differences were found.


## Mixed model for FSA category
unique(Long_100kcal[, c("FSA_category", "FSA_num")])

### Before CoL period
model <- lmer(Price_100kcal ~ YQ * FSA_num + (1|ITEM_ID), data = Long_100kcal_before_COL)
summary(model)
confint(model)

### During CoL period
model <- lmer(Price_100kcal ~ YQ * FSA_num + (1|ITEM_ID), data = Long_100kcal_during_COL)
summary(model)
confint(model)

############################################Plot the price development over time by eatwell group################################################################
#Eatwell category
# Add 'Q' before the last digit in the 'YQ' column
Long_100kcal_eatwell$YQ1 <- gsub("(\\d)$", " Q\\1", Long_100kcal_eatwell$YQ)


#Plot figure
display.brewer.all(colorblindFriendly = TRUE)

eatwell_colors <- brewer.pal(n = 5, name = "Set2")

plot_eatwell4 <- ggplot(Long_100kcal_eatwell, aes(x = factor(YQ1), y = Price_100kcal, color = EATWELL_CAT, group = EATWELL_CAT)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = EATWELL_CAT), size = 2) +
  scale_y_continuous(breaks = seq(0,1.2,0.05)) +
  scale_x_discrete(breaks=c("2013 Q1", "2014 Q1", "2015 Q1", "2016 Q1", "2017 Q1", "2018 Q1", "2019 Q1", "2020 Q1", "2021 Q1", "2022 Q1", "2023 Q1")) +
  labs(x = "Year",
       y = "Price (£/100 kcal)",
       color = "Eatwell category") +
  theme_classic() +
  scale_color_manual(values = eatwell_colors) +   # use color palette
  scale_shape_manual(values = c(16, 17, 15, 3, 4)) +  # set different shapes for each eatwell category
  guides(color = guide_legend(title = "Eatwell food group", override.aes = list(shape = c(16, 17, 15, 3, 4))), shape = "none")  +  # combine legend for color and shape
  theme(legend.position = "top")

plot_eatwell4

ggsave("V:/P8_PHI/DPH/Jody Hoenink/Articles/Price of food/Results/Plot price per 100kcal by eatwell cat quarter 2024-01-25.png", width = 15, height = 7, dpi = 300)


#FSA category
## Add 'Q' before the last digit in the 'YQ' column
Long_100kcal_fsa$YQ1 <- gsub("(\\d)$", " Q\\1", Long_100kcal_fsa$YQ)

display.brewer.all(colorblindFriendly = TRUE)

fsa_colors <- brewer.pal(n = 2, name = "Set2")

## Reverse the order of colors in the fsa_colors vector
fsa_colors <- rev(fsa_colors)

plot_fsa1 <- ggplot(Long_100kcal_fsa, aes(x = factor(YQ1), y = Price_100kcal, color = FSA_category, group = FSA_category)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = FSA_category), size = 2) +
  scale_y_continuous(breaks = seq(0,1.2,0.05)) +
  scale_x_discrete(breaks=c("2013 Q1", "2014 Q1", "2015 Q1", "2016 Q1", "2017 Q1", "2018 Q1", "2019 Q1", "2020 Q1", "2021 Q1", "2022 Q1", "2023 Q1")) +
  labs(x = "Year",
       y = "Price (£/100 kcal)",
       color = "FSA category") +
  theme_classic() +
  scale_color_manual(values = fsa_colors) +   # use color palette
  scale_shape_manual(values = c(17, 15)) +  # set different shapes for each eatwell category
  guides(color = guide_legend(title = "Healthiness category", override.aes = list(shape = c(17, 15))), shape = "none") +  # combine legend for color and shape
  theme(legend.position = "top")

plot_fsa1

ggsave("V:/P8_PHI/DPH/Jody Hoenink/Articles/Price of food/Results/Plot price per 100kcal by fsa cat quarter 2023-07-11.png", width = 13, height = 7, dpi = 300)




#Display plot by eatwell category in panels seperately
plot_eatwell5 <- ggplot(Long_100kcal_eatwell, 
                        aes(x = factor(YQ1), 
                            y = Price_100kcal)) +
  geom_line(linewidth = 1, group = Long_100kcal_eatwell$EATWELL_CAT) +
  geom_point(shape = 16, size = 2) + 
  scale_x_discrete(breaks=c("2013 Q1", "2014 Q1", "2015 Q1", "2016 Q1", 
                            "2017 Q1", "2018 Q1", "2019 Q1", "2020 Q1", 
                            "2021 Q1", "2022 Q1", "2023 Q1")) +
  labs(x = "Year",
       y = "Price (£/100 kcal)") +
  theme_classic() +
  theme(strip.background = element_rect(colour = "black", fill = NA),
        strip.text = element_text(face = "bold", size = 10, color = "black"),  # Style the strip text
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill = NA)) +
  facet_wrap(~ EATWELL_CAT, scales = "free_y", ncol = 1)  

plot_eatwell5

ggsave("V:/P8_PHI/DPH/Jody Hoenink/Articles/Price of food/Results/Plot price per 100kcal by eatwell cat quarter panels 2023-10-11.png", width = 11, height = 9, dpi = 300)


#FSA category in seperate panels
plot_fsa2 <- ggplot(Long_100kcal_fsa, 
                        aes(x = factor(YQ1), 
                            y = Price_100kcal)) +
  geom_line(linewidth = 1, group = Long_100kcal_fsa$FSA_category) +
  geom_point(shape = 16, size = 2) + 
  scale_x_discrete(breaks=c("2013 Q1", "2014 Q1", "2015 Q1", "2016 Q1", 
                            "2017 Q1", "2018 Q1", "2019 Q1", "2020 Q1", 
                            "2021 Q1", "2022 Q1", "2023 Q1")) +
  labs(x = "Year",
       y = "Price (£/100 kcal)") +
  theme_classic() +
  theme(strip.background = element_rect(colour = "black", fill = NA),
        strip.text = element_text(face = "bold", size = 10, color = "black"),  # Style the strip text
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill = NA)) +
  facet_wrap(~ FSA_category, scales = "free_y", ncol = 1)  

plot_fsa2

ggsave("V:/P8_PHI/DPH/Jody Hoenink/Articles/Price of food/Results/Plot price per 100kcal by fsa cat quarter panels 2023-10-11.png", width = 11, height = 9, dpi = 300)


