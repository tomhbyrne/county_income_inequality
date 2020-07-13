################################################################################
# PROGRAM NAME:    300_create_hud_rent_vars
# PROGRAM AUTHOR:  Tom Byrne (tbyrne@bu.edu)
# PROGRAM PURPOSE: To pull in 2007-2018 HUD 50th percentile rent data
#                  clean it and turn it into county-level measures
################################################################################

library(dplyr)
library(tidyr)
library(stringr)

# Read in HUD rent data 

temp <- list.files(path = "./data/raw data/hud_rent", pattern = "*.csv")

temp2 <- paste0("./data/raw data/hud_rent/", temp)


rent_vars <-  lapply(temp2, read.csv, stringsAsFactors = FALSE)  


names(rent_vars) <- c(2007:2018)

# Split data into 2007-2010, which just has one fips code
# and then 2011 onwards which has fips200 and fips 2010

rent_07_12 <- rent_vars[1:6]
rent_13_18 <- rent_vars[7:12]


# Just keep the fields we need from each set of years 

rent_07_12 <- lapply(rent_07_12, function(x){
  
  colnames(x) <- tolower(colnames(x))
  z = x[, c("fips","rent50_2")]
  
  colnames(z) = c("fips", "med_rent_2br")
  
  z$fips2 <- str_pad(z$fips, width = 10, pad = "0")
  z$fips  <- substr(z$fips2, 1, 5)
  
  z <- select(z, -fips2)
  return(z)
}
)

rent_13_18 <- lapply(rent_13_18, function(x){
  colnames(x) <- tolower(colnames(x))
  
  z = x[,c("fips2010", "rent50_2")]
  colnames(z) = c("fips", 
                  "med_rent_2br")
  
  z$fips2 <- str_pad(z$fips, width = 10, pad = "0")
  z$fips  <- substr(z$fips2, 1, 5)
  
  z <- select(z, -fips2)
  return(z)
}
)

# Combine and keep one row per county/per year (maximum value)
county_rent <- bind_rows(rent_07_12, rent_13_18, .id = "year") %>%
  dplyr::group_by(fips, year) %>%
  dplyr::summarize(med_rent_2br = max(med_rent_2br))


# output file
write.csv(county_rent, file = "./data/cleaned data/county_rent.csv", na = "", row.names = FALSE)