################################################################################
# PROGRAM NAME:    100_create_gini_coefficients
# PROGRAM AUTHOR:  Tom Byrne (tbyrne@bu.edu)
# PROGRAM PURPOSE: To use aggregate income data from 1-Year ACS data from 2006-2018
#                  ACS as well 2006-2018 ACS microdata from IPUMS to create
#                  county-level Gini coefficient estimates as well as well as
#                  instrumental variables for Gini coefficients using 
#                  approach by Boustan et al (2013)
################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(spatstat)
library(matrixStats)
library(Hmisc)
library(reldist)
library(haven)
library(foreign)

setwd("./data/raw data/county_acs_income")

# Read in micro data

# Read in income micro data and unduplicate by household
micro_data <- read.csv("acs_micro_data.csv") %>%
  distinct(SAMPLE, SERIAL, .keep_all = TRUE)
  

micro_data18 <- read_dta("acs_micro_data_2018.dta")

names(micro_data18) <- toupper(names(micro_data18))

micro_data18 <- micro_data18 %>%
  distinct(SAMPLE, SERIAL, .keep_all = TRUE) %>%
  mutate(HHINCOME = ifelse(HHINCOME == 9999999, NA, HHINCOME)) %>%
  filter(!(is.na(HHINCOME)) & HHINCOME >= 0)



micro_data2 <- bind_rows(micro_data, micro_data18)


# Read in aggregate ACS income data
# For 2010-2018, we were able to use the IPUMS NHGIS data
# to get table B19001
# For 2007-2009, we used Factfinder which is formatted differently
# so we read them in separately 

temp <- list.files(pattern= "*county.csv")

temp2 <-  list.files(pattern= "*with_ann.csv")

inc_1018 <- lapply(temp, read.csv, stringsAsFactors = FALSE)

inc_0609 <- lapply(temp2, read.csv, stringsAsFactors = FALSE)

# Name the objects for later use
names(inc_0609) <- c(2006:2009)

# Just keep the fields we need from each set of years 

all_income_0609 <- lapply(inc_0609, function(x){
  z = x[,c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36)]
  colnames(z) = c("fips", 
                  "All households",
                  "< $10,000",
                  "$10,000 to $14,999",
                  "$15,000 to $19,999", 
                  "$20,000 to $24,999",
                  "$25,000 to $29,999",
                  "$30,000 to $34,999",
                  "$35,000 to $39,999",
                  "$40,000 to $44,999",
                  "$45,000 to $49,999",
                  "$50,000 to $59,999",
                  "$60,000 to $74,999",
                  "$75,000 to $99,999",
                  "$100,000 to $124,999",
                  "$125,000 to $149,999",
                  "$150,000 to $199,999",
                  "$200,000 or more")
  return(z)
}
)


income_groups_0609 <- bind_rows(all_income_0609, .id = "year")


# get rid of first row, which has labels
income_groups_0609 <- income_groups_0609[-1, ]


all_income_1018 <- lapply(inc_1018, function(x){
  z = x[,c(2, 6, 8, 26:42)]
  colnames(z) = c("year", 
                  "state",
                  "county", 
                  "All households",
                  "< $10,000",
                  "$10,000 to $14,999",
                  "$15,000 to $19,999", 
                  "$20,000 to $24,999",
                  "$25,000 to $29,999",
                  "$30,000 to $34,999",
                  "$35,000 to $39,999",
                  "$40,000 to $44,999",
                  "$45,000 to $49,999",
                  "$50,000 to $59,999",
                  "$60,000 to $74,999",
                  "$75,000 to $99,999",
                  "$100,000 to $124,999",
                  "$125,000 to $149,999",
                  "$150,000 to $199,999",
                  "$200,000 or more")
  return(z)
}
)


# combine all years and create fips code
income_groups_1018 <- bind_rows(all_income_1018) %>%
  mutate(fips = paste0(state, county)) %>%
  select(-state, - county)

# Drop first row that has lables
income_groups_1018 <- income_groups_1018[-1, ]


# combine the files and get rid of excess rows 
income_groups_final <- bind_rows(income_groups_0609, income_groups_1018) %>%
  filter(year != "Data File Year")

# Reshape to long 
income_long <- income_groups_final %>%
  gather(inc_group, n_households, 3:19) %>%
  select(year, fips, inc_group, n_households) %>%
  mutate(year = as.numeric(year),
         state = substr(fips, 1, 2))


# Calculate median income within each of the 
# income categories by year and county
# Caclulate median in 2 ways:
# 1) not accounting for household  weights
# and 2) a weighted median accounting for them 
# also calculate median income in each income
# category for the entire country, which we
# will use to construct our instrumental variable 

groups = c("< $10,000",
           "$10,000 to $14,999",
           "$15,000 to $19,999", 
           "$20,000 to $24,999",
           "$25,000 to $29,999",
           "$30,000 to $34,999",
           "$35,000 to $39,999",
           "$40,000 to $44,999",
           "$45,000 to $49,999",
           "$50,000 to $59,999",
           "$60,000 to $74,999",
           "$75,000 to $99,999",
           "$100,000 to $124,999",
           "$125,000 to $149,999",
           "$150,000 to $199,999",
           "$200,000 or more")

income_by_county <- micro_data2 %>%
  mutate(state = str_pad(STATEFIP, 2, pad = "0"),
         county = str_pad(COUNTYFIP, 3, pad = "0"),
         HHINCOME = ifelse(HHINCOME == 9999999, NA, HHINCOME),
         inc_group = cut(HHINCOME, breaks = c(-Inf, 9999, 14999, 19999, 24999, 29999, 34999, 39999, 
                         44999, 49999, 59999, 74999, 99999, 124999, 149999, 
                         199999, Inf), labels = groups),
         fips = paste0(state, county),
         year = YEAR,
         inc_group = as.character(inc_group)) %>%
  group_by(year, fips, inc_group) %>%
  dplyr::summarize(median_inc = median(HHINCOME),
            w_median_inc = weightedMedian(HHINCOME, HHWT)) 

# Since not all counties in 1-Year ACS aggregate tables
# are identifiable in microdata, we will
# use STATE median income in each income
# bin to calculate Gini coefficient
# In other words, we will assume households 
# in each income bin in a given county have the 
# median income of all households in that income bin 
# in their state


income_by_state <- micro_data2 %>%
  mutate(state = str_pad(STATEFIP, 2, pad = "0"),
         HHINCOME = ifelse(HHINCOME == 9999999, NA, HHINCOME),
         inc_group = cut(HHINCOME, breaks = c(-Inf, 9999, 14999, 19999, 24999, 29999, 34999, 39999, 
                                              44999, 49999, 59999, 74999, 99999, 124999, 149999, 
                                              199999, Inf), labels = groups),
         year = YEAR,
         inc_group = as.character(inc_group)) %>%
  group_by(year, state, inc_group) %>%
  dplyr::summarize(median_inc = median(HHINCOME),
                   w_median_inc = weightedMedian(HHINCOME, HHWT)) %>%
  ungroup()


# Join micro data and ACS tables to calculate Gini coefficient
# for  each STATE, by assigning median income in each income
# bin dervied from micro data to all households in income bin
# in ACS data
# Because not all counties that are in ACS aggregate table had observations in 
# micro data  we use the STATE median income in each bin so we can 
# calculate gini coefficient for all county/year combinations 


gini <- left_join(income_long, income_by_state, by = c("year", "state", "inc_group")) %>%
  filter(inc_group != "All households") %>%
  mutate(inc_group = factor(inc_group, 
                            levels = c("< $10,000",
                                       "$10,000 to $14,999",
                                       "$15,000 to $19,999", 
                                       "$20,000 to $24,999",
                                       "$25,000 to $29,999",
                                       "$30,000 to $34,999",
                                       "$35,000 to $39,999",
                                       "$40,000 to $44,999",
                                       "$45,000 to $49,999",
                                       "$50,000 to $59,999",
                                       "$60,000 to $74,999",
                                       "$75,000 to $99,999",
                                       "$100,000 to $124,999",
                                       "$125,000 to $149,999",
                                       "$150,000 to $199,999",
                                       "$200,000 or more"))) %>%
  group_by(year, fips) %>%
  mutate(n_households = as.numeric(n_households),
         total_inc = median_inc * n_households,
         total_inc2 = w_median_inc * n_households) %>%
  mutate(pct_hh = n_households / sum(n_households),
         pct_inc = total_inc / sum(total_inc),
         pct_inc2 = total_inc2 / sum(total_inc2)) %>%
  arrange(year, fips, inc_group) %>%
  mutate(cum_sum = cumsum(pct_hh),
         pct_richer = 1 - cum_sum,
         score = pct_inc * (pct_hh + 2 * pct_richer),
         score2 = pct_inc2 * (pct_hh + 2 * pct_richer),
         total_score = sum(score),
         total_score2 = sum(score2),
         gini1 = 1 - total_score,
         gini2 = 1 - total_score2)


# Keep one row per county year
gini_final <- distinct(gini, year, fips, .keep_all = TRUE) %>%
  select(year, fips, gini1, gini2)


###########################################################
# Create instrumental variable
##########################################################

# To do this we use following steps:
  # 1) Convert the end points of each income bin in 2006 
  #    from absolute income levels into percentiles of the 
  #    NATIONAL income distribution, based on 2006 ACS microdata

  #  2) Calculate annual growth rate within each of these percentile ranges 
  #     for reach year from 2006-2017
  # 
  #  3) Calculate a predicted income distribution for each county based
  #     on these growth trends.  Specifically, apply the growth rate 
  #     for the percentile ranges to the 2006 household bins 
  #     and then recalculate gini coefficient



# 1)  Concvert end points of each income bin in 2006 from 
#     absolute income levels into percentiles of national income
#     distribution 

national_percentile <- micro_data2 %>%
  mutate(HHINCOME = ifelse(HHINCOME == 9999999, NA, HHINCOME)) %>%
  filter(YEAR == 2006) %>%
  mutate(percentile = percent_rank(HHINCOME)) %>%
  # keep bin end points 
  filter(HHINCOME %in% c(10000, 15000, 20000, 25000, 30000,
                         35000, 40000, 45000, 50000, 60000,
                         75000, 100000, 125000, 150000,
                         200000)) %>%
  # keep one observation per bin end point 
  distinct(HHINCOME, .keep_all = TRUE) %>%
  dplyr::select(HHINCOME, percentile) %>%
  arrange(HHINCOME)

# Create a dummy dataset to make end points for each bin
# 

HHINCOME <- 0
percentile <- 0

dummy_row <- data.frame(HHINCOME, percentile)

groups <- bind_rows(dummy_row, national_percentile) %>%
  mutate(lbound1 = round(percentile, 3), 
         ubound1 = round(lead(percentile),3),
         ubound1 = ifelse(is.na(ubound1), 1, ubound1))

 # print(groups)
 # HHINCOME percentile lbound1 ubound1
 # 1         0 0.00000000   0.000   0.074
 # 2     10000 0.07368598   0.074   0.132
 # 3     15000 0.13225002   0.132   0.187
 # 4     20000 0.18720892   0.187   0.245
 # 5     25000 0.24455319   0.245   0.299
 # 6     30000 0.29891812   0.299   0.355
 # 7     35000 0.35496698   0.355   0.406
 # 8     40000 0.40622620   0.406   0.457
 # 9     45000 0.45732123   0.457   0.503
 # 10    50000 0.50292004   0.503   0.589
 # 11    60000 0.58907011   0.589   0.696
 # 12    75000 0.69592003   0.696   0.815
 # 13   100000 0.81543862   0.815   0.887
 # 14   125000 0.88733580   0.887   0.926
 # 15   150000 0.92560915   0.926   0.962
 # 16   200000 0.96189771   0.962   1.000

#  2) Calculate annual growth rate within each of these percentile ranges 
#     for reach year from 2006-2017.  To do this, we re-group households
#     into percenitle income groups for each year that correspond to the 
#     2006 bin end points, and then calculate median income within each of 
#     these percentile group. 


national_income <- micro_data2 %>%
  group_by(YEAR) %>%
  mutate(HHINCOME = ifelse(HHINCOME == 9999999, NA, HHINCOME),
         pctile = percent_rank(HHINCOME),
         inc_group2 = cut(pctile, breaks = c(-Inf, .074, .132, .187,
                                              .245, .299, .355, .406, 
                                              .457, .503, .589, .696,
                                              .815, .887, .926, .962, 1)),
                           
         inc_group = cut(pctile, breaks = c(-Inf, .074, .132, .187,
                                            .245, .299, .355, .406, 
                                            .457, .503, .589, .696,
                                            .815, .887, .926, .962, 1),
                         # Label income groups according to their 2006 bin end points for merging later
                         labels = c("< $10,000",
                                    "$10,000 to $14,999",
                                    "$15,000 to $19,999", 
                                    "$20,000 to $24,999",
                                    "$25,000 to $29,999",
                                    "$30,000 to $34,999",
                                    "$35,000 to $39,999",
                                    "$40,000 to $44,999",
                                    "$45,000 to $49,999",
                                    "$50,000 to $59,999",
                                    "$60,000 to $74,999",
                                    "$75,000 to $99,999",
                                    "$100,000 to $124,999",
                                    "$125,000 to $149,999",
                                    "$150,000 to $199,999",
                                    "$200,000 or more")))


# calculate median income in each bin in each year  


annual_growth <- national_income %>%
  group_by(YEAR, inc_group, inc_group2) %>%
  dplyr::summarize(median_income = median(HHINCOME),
                   w_median_inc = weightedMedian(HHINCOME, HHWT)) %>%
  mutate(year = YEAR) %>%
  arrange(inc_group, YEAR)


#  3) Calculate a predicted income distribution for each county based
#     on these growth trends.  Specifically, apply the growth rate 
#     for the percentile ranges to the 2006 household bins 
#     and then recalculate gini coefficient
#     Create variables for base year (2006) median income (both accounting for weights and not )
#     Also create variables for number of households in each income bin in 2006 (base year)
#     Since we calculate predicted future income distribution based on these households 


# Figure out the what national level income bin each local bin belongs in 
county_income_pred <- left_join(income_long, income_by_state, by = c("year", "state", "inc_group")) %>%
  filter(year == 2006) %>%
  mutate(county_median_inc = median_inc, 
         count_w_median_inc = w_median_inc) %>%
  filter(inc_group != "All households") %>%
  select(-year, -w_median_inc, -median_inc) %>%
  left_join(., annual_growth, by = c("inc_group")) %>%
  filter(year != 2006)

# Calculate new gini coefficeint based off this predicted income distribution

gini_instrument <- county_income_pred %>% 
  group_by(year, fips) %>%
  mutate(n_households = as.numeric(n_households),
         gini3 = gini(median_income, weights = n_households),
         gini4 = gini(w_median_inc, weights = n_households))

# Keep one row per county year
gini_instrument_final <- distinct(gini_instrument, year, fips, .keep_all = TRUE) %>%
  select(year, fips, gini3, gini4)
  


# Merge together the observed gini and the predicted gini and drop values 
# for 2006
# Recode NaN as missing

both_gini <- left_join(gini_final, gini_instrument_final, by = c("year", "fips")) %>%
  filter(year > 2006)

# rename the variables

names(both_gini) <- c("year", "fips", "gini_uw", "gini_w", "iv_gini_uw", "iv_gini_w")

# output the final file 

write.csv(both_gini, file = "./data/cleaned data/county_gini.csv", na = "", row.names = FALSE)


