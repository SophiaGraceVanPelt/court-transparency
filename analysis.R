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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(df$agency_category)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

counties <- df %>% 
  filter(agency_category == "County")


ggplot(data = counties) +
  geom_histogram(mapping = aes(x = requests_cost))

ggplot(data = counties) +
  geom_histogram(mapping = aes(x = est_staff_hours_spent))


ggplot(data = counties, mapping = aes(x = requests_cost, y = reporting_status)) +
  geom_boxplot()

ggplot(data = counties) + 
  geom_hex(mapping = aes(x = total_requests, y = requests_cost))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

city_town <- df %>% 
  filter(agency_category %in% c("City/Town", "City/town"))

ggplot(data = city_town) +
  geom_histogram(mapping = aes(x = requests_cost))

ggplot(data = city_town) +
  geom_histogram(mapping = aes(x = est_staff_hours_spent))


ggplot(data = city_town, mapping = aes(x = requests_cost, y = reporting_status)) +
  geom_boxplot()

ggplot(data = city_town) + 
  geom_hex(mapping = aes(x = total_requests, y = requests_cost))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df %>%
  sapply(mean, sd, var, min, max, na.rm = TRUE)

range(counties$requests_cost, na.rm = TRUE)
