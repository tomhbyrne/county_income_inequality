################################################################################
# PROGRAM NAME:    400_create_final_dataset
# PROGRAM AUTHOR:  Tom Byrne (tbyrne@bu.edu)
# PROGRAM PURPOSE: To combine county level measures into final panel dataset
################################################################################
library(dplyr)
library(tidyr)
library(stringr)


# Read in cleaned county level files 
county_gini <- read.csv("./data/cleaned data/county_gini.csv", stringsAsFactors = FALSE) %>%
  mutate(fips = str_pad(fips, width = 5, pad = "0"))

county_acs <- read.csv("./data/cleaned data/acs_county_vars_new.csv", stringsAsFactors = FALSE) %>%
  mutate(fips = str_pad(fips, width = 5, pad = "0")) 

county_rent <- read.csv("./data/cleaned data/county_rent.csv", stringsAsFactors = FALSE) %>%
  mutate(fips = str_pad(fips, width = 5, pad = "0"))

# county GDP
county_real_gdp <- read.csv("./data/raw data/CAGDP9/CAGDP9__ALL_AREAS_2001_2018.csv", stringsAsFactors = FALSE) %>%
  filter(GeoName != "United States" & Description == "All industry total") %>%
  gather(year, gdp, X2007:X2018) %>%
  mutate(fips1 = as.numeric(GeoFIPS),
         fips = str_pad(fips1, width = 5, pad = "0"),
         year = as.numeric(substring(year, 2,5)),
         gdp2 = as.numeric(gdp),
         real_gdp = gdp2 / 1000000) %>%
  select(fips, year,real_gdp)


# county rural urban continuum coes
county_rural <- read.csv("./data/raw data/ruralurbancodes2013.csv", stringsAsFactors = FALSE) %>%
  mutate(fips = str_pad(ï..FIPS, width = 5, pad = "0"),
         rucc_2013 = RUCC_2013) %>%
  select(fips, rucc_2013)
  

# Join all files and create variables where needed 

county_final <- left_join(county_gini, county_acs, by = c("fips", "year")) %>%
  left_join(., county_rent, by = c("fips", "year")) %>%
  left_join(., county_real_gdp, by = c("fips", "year")) %>%
  left_join(., county_rural, by = "fips") %>%
  mutate(acs_gini = gini3,
         poverty_rate = pov_pop / pov_universe * 100,
         extreme_pov_rate = extreme_pov_pop / pov_universe * 100,
         pct_black = black_pop / total_population * 100,
         pct_hispanic = hispanic_pop / total_population * 100,
         pop_density = total_population / land_area,
         pct_burdened_owners = burdened_owners1 / owner_units * 100,
         pct_burdened_renters = burdened_renters / renter_units * 100,
         pct_poor_burdened = poor_burdened_renters / poor_renter_units * 100) %>%
  dplyr::select(-gini3, -no_burden_info4)



write.csv(county_final, file = "./data/final dataset/county_all_final.csv",
          na = "", row.names  = FALSE)



