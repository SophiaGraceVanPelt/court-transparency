library(tidyverse)
library(hexbin)

options(scipen = 10)

# what directory is the file in
path <- "/home/sophie/Documents/projects/judicial_foia/02-data/unprocessed/washington/"

# set the path to the directory the file is in
setwd(path)

# I don't like copying and pasting the file name all the time
file <- "jlarc_full_dataset.csv"

# load data 
df <- read_csv(file)

head(df)

# remove duplicates

df <- df %>%
  distinct()

# check for NAs

colnames(df)

df_full <- df

df_full$above_100k <- df$reporting_status == "Above the $100k threshold. Report submittal required."

colnames(df_dropped_nas)


df_dropped_nas <- df %>% 
  filter(total_requests & est_staff_hours_spent & avg_est_staff_hours & requests_cost & avg_est_total_cost & total_litigation_cost != "NA")
# agency_name, agency_category, report_status, all don't have NAs
# est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, total_requests are all missing a few hundred

df_dropped_nas <- df_dropped_nas %>%
  select(agency_name, agency_category, agency_type, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, total_requests, year, above_100k)

write_csv(df_dropped_nas, file = "jlarc_dropped_nas_dataset.csv", col_names = TRUE, append = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(df$agency_category)

counties <- df_dropped_nas %>% 
  filter(agency_category == "County") %>%
  separate(agency_name, into = c("county", "subtype"), sep = "-")

counties$subtype <- trimws(counties$subtype, which = c("left"))

colnames(counties)

counties <- counties %>%
  select(county, subtype, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, total_requests, year, above_100k)

head(counties)

write_csv(counties, file = "jlarc_counties_dataset.csv", col_names = TRUE, append = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

county_clerk <- counties %>%
  filter(subtype == "Clerk")

write_csv(county_clerk, file = "jlarc_county_clerk_dataset.csv", col_names = TRUE, append = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

city_town <- df_dropped_nas %>% 
  filter(agency_category %in% c("City/Town", "City/town"))

write_csv(city_town, file = "jlarc_city_town_dataset.csv", col_names = TRUE, append = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table(counties$subtype)

county_centralized <- counties %>%
  filter(subtype == "Centralized")

write_csv(county_centralized, file = "jlarc_county_centralized_dataset.csv", col_names = TRUE, append = FALSE)
