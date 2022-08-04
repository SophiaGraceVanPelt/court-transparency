library(tidyverse)
library(hexbin)
library(dplyr)

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

counties <- counties %>%
  separate(agency_name, into = c("county", "subtype"), sep = "-")

# write_csv(counties, file = "counties_dataset.csv", col_names = TRUE, append = TRUE)

write_csv(clerks, file = "clerks_dataset.csv", col_names = TRUE, append = TRUE)

head(counties$subtype)

clerks <- counties %>%
  filter(subtype == " Clerk")

ggplot(data = clerks) +
  geom_histogram(mapping = aes(x = requests_cost))

clerks_minus_outlier <- clerks[-2,]

ggplot(data = clerks_minus_outlier) +
  geom_histogram(mapping = aes(x = avg_est_total_cost)) +
  labs(x = "Average Estimated Cost Per Request", y = "Number of Counties", title = "Washington Counties Average Cost of Request", subtitle = "Minus King County in 2020")

ggplot(data = clerks) +
  geom_histogram(mapping = aes(x = avg_est_staff_hours)) +
  labs(x = "Average Estimated Staff Time Spent per Request", y = "Number of Counties", title = "Washington Counties Average Staff Hours Spent per Request")

ggplot(data = clerks,mapping = aes(x = total_requests)) +
  stat_bin(bins = 10) +
  labs(x = "Total Number of Requests", y = "Number of Counties", title = "Total Number of Requests Reported by Washington Counties")


ggplot(data = clerks_minus_outlier, mapping = aes(x = reporting_status, y = avg_est_total_cost)) +
  geom_boxplot()

clerks %>%
  sapply(sd, na.rm = TRUE)


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
