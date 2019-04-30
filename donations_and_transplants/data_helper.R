library(fs)
library(janitor)
library(tidyverse)

# Works in the helper script

# file_list <- dir_ls("../transplant_data")

# Read donation data and transplant data from CSVs in dedicated folder

donation_list <- dir_ls("../donation_data")

transplant_list <- dir_ls("./transplant_data")

# Map the read_csv function over each list to get raw data with column specs

raw_donations <- map_dfr(donation_list, 
                           read_csv, 
                           .id = NULL,
                           col_names = c("country",
                                         "year",
                                         "type",
                                         "donations",
                                         "measure",
                                         "donor_status"),
                           col_types = cols(
                             country = col_character(),
                             year = col_integer(),
                             type = col_character(),
                             donations = col_character(),
                             measure = col_character(),
                             donor_status = col_character()
                           ))

raw_transplants <- map_dfr(transplant_list, 
                           read_csv, 
                           .id = NULL,
                           col_names = c("country",
                                         "year",
                                         "organ",
                                         "transplants",
                                         "measure",
                                         "donor_status"),
                           col_types = cols(
                             country = col_character(),
                             year = col_integer(),
                             organ = col_character(),
                             transplants = col_character(),
                             measure = col_character(),
                             donor_status = col_character()
                           ))

all_transplants <- raw_transplants %>% 
  
  # Fixed duplication bug
  
  distinct() %>% 
  
  # I strived to filter out all missing or irrelevant. I decided that it was
  # unlikely in general that a country would have 0 donations/transplants, and I
  # decided to treat those values as missing data.
  
  filter(!transplants %in% c("0", "-")) %>% 
  
  # Parsed doubles as doubles
  
  mutate(transplants = parse_double(transplants)) %>% 
  group_by(country, year, organ, measure) %>% 
  
  # For transplant data, I decided not to distinguish between living and deceased donors
  
  summarize(transplants = sum(transplants)) %>% 
  ungroup()

# In order to select only those countries with data, I took the country names
# from the transplant database

countries <- all_transplants %>% 
  distinct(country) %>% 
  select(country) %>% 
  unlist(use.names = FALSE)

# Created a vector of organ types for use in dropdown menus

organs <- c("Kidney" = "kidney",
            "Liver" = "liver",
            "Pancreas" = "pancreas",
            "Heart" = "heart",
            "Lung" = "lung")