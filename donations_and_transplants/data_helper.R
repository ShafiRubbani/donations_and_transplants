library(fs)
library(janitor)
library(tidyverse)

# Works in the helper script

# file_list <- dir_ls("../transplant_data")

# Works in the app

file_list <- dir_ls("../../transplant_data")

raw_transplants <- map_dfr(file_list, 
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
  filter(!transplants %in% c("0", "-")) %>% 
  mutate(transplants = parse_double(transplants)) %>% 
  group_by(country, year, organ, measure) %>% 
  summarize(transplants = sum(transplants)) %>% 
  ungroup()

countries <- all_transplants %>% 
  distinct(country) %>% 
  select(country) %>% 
  unlist(use.names = FALSE)

organs <- c("Kidney" = "kidney",
            "Liver" = "liver",
            "Pancreas" = "pancreas",
            "Heart" = "heart",
            "Lung" = "lung",
            "Heart/Lung" = "heart/lung")