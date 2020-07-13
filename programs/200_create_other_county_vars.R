################################################################################
# PROGRAM NAME:    200_create_other_county_vars
# PROGRAM AUTHOR:  Tom Byrne (tbyrne@bu.edu)
# PROGRAM PURPOSE: To create county-level covariates based on aggregate 
#                  ACS data
################################################################################

library(dplyr)
library(tidyr)
library(stringr)


# Read in other county measures

temp <- list.files(path = "./data/raw data/county_acs_other", pattern = "*.csv")

temp2 <- paste0("./data/raw data/county_acs_other/", temp)


county_vars <-  lapply(temp2, read.csv, stringsAsFactors = FALSE)  

# Name the objects for later use
names(county_vars) <- c(2007:2018)

# Just keep the fields we need from each set of years 
county_vars2 <- lapply(county_vars, function(x){
  z = x[,c(1, 54, 56, 57, 68, 69, 70, 85, 107, 176, 
           177, 178, 179, 182, 183, 184, 187, 
           188, 189, 200, 201, 204, 208, 212, 216, 
           220, 221, 222, 245, 246, 259)]
  colnames(z) = c("fips", 
                  "total_population",
                  "land_area",
                  "pop_density",
                  "pop_65_74",
                  "pop_75_84",
                  "pop_85_up",
                  "black_pop",
                  "hispanic_pop",
                  "hh_inc",
                  "gini3",
                  "home_value",
                  "owner_occupied_units",
                  "owner_burden1",
                  "owner_burden2",
                  "no_burden_info1",
                  "owner_burden3", 
                  "owner_burden4",
                  "no_burden_info2",
                  "renter_occupied_units",
                  "poor_renter_units",
                  "renter_burden1",
                  "renter_burden2",
                  "renter_burden3",
                  "renter_burden4",
                  "renter_burden5",
                  "no_burden_info3",
                  "no_burden_info4",
                  "pov_universe",
                  "extreme_pov_pop",
                  "pov_pop")
  return(z)
}
)

# Put the years together and get rid of rows from each 
# dataset hat have labels 

all_county_vars <- bind_rows(county_vars2, .id = "year") %>%
  filter(fips != "Geo_FIPS")

# convert variables to numeric

all_county_vars2 <- cbind(all_county_vars[, 1:2], lapply(all_county_vars[, c(3:32)], as.numeric))


# Create variable as needed
# In creating cost burden variables, we 
# have to subract out owner and renter houeholds
# for whom cost burden is not determined to 
# remove them from denominator 
# For owners, we can calculate burdened (burdended_owners1)
# and severely burdened (burdened_owners2)



all_county_vars_final <- all_county_vars2 %>%
  mutate(owner_units = owner_occupied_units - (no_burden_info1 + no_burden_info2),
         burdened_owners1 = owner_burden1 + owner_burden3,
         burdened_owners2 = owner_burden2 + owner_burden2,
         poor_burdened_renters = renter_burden1,
         renter_units = renter_occupied_units - (no_burden_info3 + no_burden_info4),
         burdened_renters = renter_burden1 + renter_burden2 +  renter_burden3 + renter_burden4 + renter_burden5) %>%
  dplyr::select(-owner_occupied_units, -owner_burden1, -owner_burden2, -owner_burden3, - owner_burden4,
                -no_burden_info1, -no_burden_info2, -no_burden_info3, no_burden_info4, -renter_occupied_units,
                -renter_burden1, -renter_burden2, -renter_burden3, -renter_burden4, -renter_burden5)





# write out county vars 
write.csv(all_county_vars_final, "./data/cleaned data/acs_county_vars_new.csv", na = "", row.names = FALSE)
