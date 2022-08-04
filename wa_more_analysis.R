library(tidyverse)
library(hexbin)
library(gtsummary)

options(scipen = 10)

# what directory is the file in
path <- "/home/sophie/Documents/projects/judicial_foia/02-data/unprocessed/washington/"

# set the path to the directory the file is in
setwd(path)

# load data 
df_dropped_nas <- read_csv("jlarc_dropped_nas_dataset.csv")
df_full_dataset <- read_csv("jlarc_full_dataset.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Difference in full and dropped
df_full_dataset$complete_case <- df_full_dataset %>% 
  select(total_requests, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost) %>%
  complete.cases()

# df_full_dataset$above_100k <- df_full_dataset$reporting_status == "Above the $100k threshold. Report submittal required."

table(df_full_dataset$complete_case)

df_dropped_nas2 <- df_full_dataset %>%
  filter(complete_case == TRUE)

df_full_dataset %>%
  select(
    `Total Staff Hours Spent` = est_staff_hours_spent,
    `Average Staff Hours Per Request` = avg_est_staff_hours,
    `Total Cost Of Requests` = requests_cost,
    `Average Cost Per Request` = avg_est_total_cost,
    `Total Litigation Cost` = total_litigation_cost,
    `Total Number Of Requests` = total_requests,
    `Complete Cases` = complete_case
  ) %>%
  tbl_summary(by = `Complete Cases`) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_n()
  

# Summary Statistics

summary_stats <- df_dropped_nas2 %>%
  select(
    `Total Staff Hours Spent` = est_staff_hours_spent,
    `Average Staff Hours Per Request` = avg_est_staff_hours,
    `Total Cost Of Requests` = requests_cost,
    `Average Cost Per Request` = avg_est_total_cost,
    `Total Litigation Cost` = total_litigation_cost,
    `Total Number Of Requests` = total_requests
  ) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean}", "{median} ({p25}, {p75})", "{sd}", "{min}, {max}")
    )

summary_stats

summary_stats_tibble <- as_tibble(summary_stats)

df_dropped_nas2 %>%
  select(
    `Total Number Of Requests` = total_requests,
    `Total Staff Hours Spent` = est_staff_hours_spent,
    `Average Staff Hours Per Request` = avg_est_staff_hours
  ) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean}", "{median} ({p25}, {p75})", "{sd}", "{min}, {max}")
  )



df_dropped_nas2 %>%
  select(
    `Average Cost Per Request` = avg_est_total_cost,
    `Total Number Of Requests` = total_requests,
    `Total Litigation Cost` = total_litigation_cost
  ) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean}", "{median} ({p25}, {p75})", "{sd}", "{min}, {max}")
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

counties <- df_dropped_nas2 %>% 
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

city_town <- df_dropped_nas2 %>% 
  filter(agency_category %in% c("City/Town", "City/town"))

write_csv(city_town, file = "jlarc_city_town_dataset.csv", col_names = TRUE, append = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table(counties$subtype)

county_centralized <- counties %>%
  filter(subtype == "Centralized")

county_centralized %>%
  select(
    `Total Number Of Requests` = total_requests,
    `Total Staff Hours Spent` = est_staff_hours_spent,
    `Average Staff Hours Per Request` = avg_est_staff_hours
  ) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean}", "{median} ({p25}, {p75})", "{sd}", "{min}, {max}")
  )

county_centralized %>%
  sapply(mean, na.rm = TRUE)


ggplot(data = county_centralized, mapping = aes(x = total_requests %>% group_by(county))) +
  geom_bar() +
  coord_flip()



county_sum <- county_centralized %>%
  group_by(county) %>%
  summarise(Freq = sum(total_requests)) 

ggplot(data = county_sum, mapping = aes(x = county, y = Freq)) +
  geom_col() +
  coord_flip()

county_centralized %>%
  select(
    `Total Number Of Requests` = total_requests,
    `Total Cost Of Requests` = requests_cost,
    `Average Cost Per Request` = avg_est_total_cost,
    `Total Litigation Cost` = total_litigation_cost,
  ) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean}", "{median} ({p25}, {p75})", "{sd}", "{min}, {max}")
  )

ggplot(data = county_centralized) +
  geom_histogram(mapping = aes(x = total_litigation_cost))

ggplot(data = county_centralized) +
  geom_histogram(mapping = aes(x = avg_est_total_cost))
