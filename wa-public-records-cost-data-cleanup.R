library(tidyverse)
library(readxl)
library(dplyr)

# what directory is the file in
path <- "/home/sophie/Documents/projects/judicial_foia/02-data/unprocessed/washington/"

# set the path to the directory the file is in
setwd(path)

# I don't like copying and pasting the file name all the time
file <- "JLARC Public Records Full Dataset.xlsx"

# get list of excel file's sheets
print(excel_sheets(file))
print(excel_sheets(file)[c(16, 17, 18, 19, 20, 21)])

# can't find a cleaner way to do this - just going to load each sheet individually
# the names will be wa (for Washington), the year of the report, and then s# for the sheet number
wa_2017_s16 <- read_excel(file, sheet = 16, skip = 1)
wa_2017_s17 <- read_excel(file, sheet = 17, skip = 1)
wa_2017_s18 <- read_excel(file, sheet = 18, skip = 1)
wa_2017_s19 <- read_excel(file, sheet = 19, skip = 1)
wa_2017_s20 <- read_excel(file, sheet = 20, skip = 1)
wa_2017_s21 <- read_excel(file, sheet = 21, skip = 1)

# Looking at some of the shared column names
colnames(wa_2017_s16)
colnames(wa_2017_s17)
# the shared columns are: "Agency Name", "Agency Category", "Agency Type", "Reporting Status"

# There has to be a better way - but I'm just gonna tediously combine all of these

df <- full_join(wa_2017_s16, wa_2017_s17, by = c("Agency Name", "Agency Category", "Agency Type", "Reporting Status")) %>%
  full_join(., wa_2017_s18, by = c("Agency Name", "Agency Category", "Agency Type", "Reporting Status")) %>%
  full_join(., wa_2017_s19, by = c("Agency Name", "Agency Category", "Agency Type", "Reporting Status")) %>%
  full_join(., wa_2017_s20, by = c("Agency Name", "Agency Category", "Agency Type", "Reporting Status")) %>%
  full_join(., wa_2017_s21, by = c("Agency Name", "Agency Category", "Agency Type", "Reporting Status"))

# Now cleaning the combined df

counties <- df %>%
  filter(`Agency Category` == "County") %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

table(df$`Agency Category`)

agencies_etc <- df %>%
  filter(`Agency Category` == "State agency, commission, or board")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

counties <- counties %>%
  select(- `Agency Category`, `Agency Type`)
counties$year <- 2017

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Awesome! looks like the 2017 counties is pretty cleaned up - next lets bring in the 
# 2018, 2019, and 2020 reports and clean them up as well

wa_2018 <-"2018 JLARC Public Records Full Dataset.xlsx"
wa_2019 <- "2019 JLARC Public Records Full Dataset.xlsx"
wa_2020 <- "2020 JLARC Public Records Full Dataset.xlsx"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Lets look at the sheet names - see if there are any differences
print(excel_sheets(wa_2018))
print(excel_sheets(wa_2019))
print(excel_sheets(wa_2020))

# I'll want sheets: 14, 15, 16, 17, 18, 19, and they look like they should contain the same data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wa_2018_s14 <- read_excel(wa_2018, sheet = 14, skip = 1)
wa_2018_s15 <- read_excel(wa_2018, sheet = 15, skip = 1)
wa_2018_s16 <- read_excel(wa_2018, sheet = 16, skip = 1)
wa_2018_s17 <- read_excel(wa_2018, sheet = 17, skip = 1)
wa_2018_s18 <- read_excel(wa_2018, sheet = 18, skip = 1)
wa_2018_s19 <- read_excel(wa_2018, sheet = 19, skip = 1)

wa_2019_s14 <- read_excel(wa_2019, sheet = 14, skip = 1)
wa_2019_s15 <- read_excel(wa_2019, sheet = 15, skip = 1)
wa_2019_s16 <- read_excel(wa_2019, sheet = 16, skip = 1)
wa_2019_s17 <- read_excel(wa_2019, sheet = 17, skip = 1)
wa_2019_s18 <- read_excel(wa_2019, sheet = 18, skip = 1)
wa_2019_s19 <- read_excel(wa_2019, sheet = 19, skip = 1)

wa_2020_s14 <- read_excel(wa_2020, sheet = 14, skip = 1)
wa_2020_s15 <- read_excel(wa_2020, sheet = 15, skip = 1)
wa_2020_s16 <- read_excel(wa_2020, sheet = 16, skip = 1)
wa_2020_s17 <- read_excel(wa_2020, sheet = 17, skip = 1)
wa_2020_s18 <- read_excel(wa_2020, sheet = 18, skip = 1)
wa_2020_s19 <- read_excel(wa_2020, sheet = 19, skip = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cool, now that they are all loaded lets combine them and add the year, and get the really big df!
# have some issues with cases - let's make them all lower case

names(wa_2018_s14) <- tolower(names(wa_2018_s14))
names(wa_2018_s15) <- tolower(names(wa_2018_s15))
names(wa_2018_s16) <- tolower(names(wa_2018_s16))
names(wa_2018_s17) <- tolower(names(wa_2018_s17))
names(wa_2018_s18) <- tolower(names(wa_2018_s18))
names(wa_2018_s19) <- tolower(names(wa_2018_s19))

names(wa_2019_s14) <- tolower(names(wa_2019_s14))
names(wa_2019_s15) <- tolower(names(wa_2019_s15))
names(wa_2019_s16) <- tolower(names(wa_2019_s16))
names(wa_2019_s17) <- tolower(names(wa_2019_s17))
names(wa_2019_s18) <- tolower(names(wa_2019_s18))
names(wa_2019_s19) <- tolower(names(wa_2019_s19))


wa_2019_s14 <- wa_2019_s14 %>%
  rename(`agency type` = agencytype)

names(wa_2020_s14) <- tolower(names(wa_2020_s14))
names(wa_2020_s15) <- tolower(names(wa_2020_s15))
names(wa_2020_s16) <- tolower(names(wa_2020_s16))
names(wa_2020_s17) <- tolower(names(wa_2020_s17))
names(wa_2020_s18) <- tolower(names(wa_2020_s18))
names(wa_2020_s19) <- tolower(names(wa_2020_s19))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_2018 <- full_join(wa_2018_s14, wa_2018_s15, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2018_s16, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2018_s17, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2018_s18, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2018_s19, by = c("agency name", "agency category", "agency type", "reporting status"))

df_2019 <- full_join(wa_2019_s14, wa_2019_s15, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2019_s16, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2019_s17, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2019_s18, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2019_s19, by = c("agency name", "agency category", "agency type", "reporting status"))

df_2020 <- full_join(wa_2020_s14, wa_2020_s15, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2020_s16, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2020_s17, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2020_s18, by = c("agency name", "agency category", "agency type", "reporting status")) %>%
  full_join(., wa_2020_s19, by = c("agency name", "agency category", "agency type", "reporting status"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rbind(df_2018, df_2019)

colnames(df_2018)
table(colnames(df_2019))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# lets dive back into the raw data - understand the variables a bit better and see what we need and what we don't

#~~~~
# 2018
colnames(df_2018)

df_2018 <- df_2018 %>%
  rename(agency_name = `agency name`,
         agency_type = `agency type`,
         est_staff_hours_spent = `estimated staff hours spent`,
         total_requests = `total requests`,
         avg_est_staff_hours_no_rounding = `average estimated staff hours spent\r\n(no rounding)`,
         staff_hours_comments = `comments.x`,
         agency_category = `agency category`,
         reporting_status = `reporting status`,
         avg_cost_per_request = `average cost per request`,
         total_est_cost = `total estimated cost`,
         applied_overhead_rate_for_request = `agency applied an overhead rate`,
         overhead_rate_comments = comments.y,
         total_claims = `total claims`,
         violation_type = `violation type`,
         violation_comments = comments.x.x,
         total_litigation_cost = `total litigation cost`,
         litigation_comments = comments.y.y,
         staff_costs = `staff costs`,
         system_costs = `system costs`,
         service_costs = `service costs`,
         third_party_costs = `third party costs`,
         total_est_costs = `total estimated costs`,
         overhead_rate_for_management = `agency applied overhead rate`,
         overhead_managment_costs_comments = comments.x.x.x,
         expenses_recovered = `expenses recovered`,
         customized_service_charge_description = `customized service charge description`,
         customized_service_charge = `customized service charges`,
         customized_service_charge_comments = comments.y.y.y
         )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019
colnames(df_2019)

df_2019 <- df_2019 %>%
  rename(agency_name = `agency name`,
         agency_type = `agency type`,
         est_staff_hours_spent = `estimated staff hours spent`,
         total_requests = `total requests`,
         avg_est_staff_hours_no_rounding = `average estimated staff hours spent (no rounding)`,
         staff_hours_comments = `comments.x`,
         agency_category = `agency category`,
         reporting_status = `reporting status`,
         avg_cost_per_request = `average cost per request`,
         total_est_cost = `total estimated cost`,
         applied_overhead_rate_for_request = `agency applied an overhead rate.x`,
         overhead_rate_comments = comments.y,
         total_claims = `total claims`,
         violation_type = `violation type`,
         violation_comments = comments.x.x,
         total_litigation_cost = `total litigation cost`,
         litigation_comments = comments.y.y,
         staff_costs = `staff costs`,
         system_costs = `system costs`,
         service_costs = `service costs`,
         third_party_costs = `third party costs`,
         total_est_costs = `total estimated costs`,
         overhead_rate_for_management = `agency applied an overhead rate.y`,
         overhead_managment_costs_comments = comments.x.x.x,
         expenses_recovered = `expenses recovered`,
         customized_service_charge_description = `customized service charge description`,
         customized_service_charge = `customized service charges`,
         customized_service_charge_comments = comments.y.y.y
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2020
colnames(df_2020)

df_2020 <- df_2020 %>%
  rename(agency_name = `agency name`,
         agency_category = `agency category`,
         agency_type = `agency type`,
         reporting_status = `reporting status`,
         est_staff_hours_spent = `estimated staff hours spent`,
         avg_est_staff_hours = `average estimated staff hours spent (no rounding)`,
         requests_cost = `total cost estimated`,
         avg_est_total_cost = `average estimated cost per request`,
         total_litigation_cost = `total litigation cost`,
         total_requests = `total requests (open + received)`
         )

df_2020 <- df_2020 %>%
  select(agency_name, agency_category, agency_type, reporting_status, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, total_requests)

df_2020$year <- 2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_2018$year <- 2018
df_2019$year <- 2019

df_2018_2019 <- rbind(df_2018, df_2019)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


df <- df %>% 
  rename_all(., .funs = tolower)
colnames(df)

df <- df %>%
  rename(agency_name = `agency name`,
         agency_type = `agency type`,
         est_staff_hours_spent = `estimated staff hours spent`,
         total_requests_closed = `total requests closed`,
         avg_est_staff_hours_no_rounding = `average estimated staff hours spent\r\n(no rounding)`,
         agency_category = `agency category`,
         reporting_status = `reporting status`,
         total_litigation_cost = `total litigation cost`,
         staff_costs = `staff cost`,
         system_costs = `system cost`,
         service_costs = `service cost`,
         third_party_costs = `third party cost`,
         est_total_costs = `estimated total cost`,
         total_est_costs = `total estimated cost`,
         avg_est_total_cost = `average estimated total cost`,
         total_claims = `total claims`,
         claim_type = `claim type`,
         expenses_recovered = `expenses recovered`,
         customized_service_charge = `customized service charges`,
         customized_service_charge_description = `customized service charge description`
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(df)

df <- df %>%
  rename(avg_est_staff_hours = avg_est_staff_hours_no_rounding,
         est_total_cost = est_total_costs,
         staff_cost = staff_costs,
         system_cost = system_costs,
         service_cost = service_costs,
         third_party_cost = third_party_costs,
         total_est_cost = total_est_costs
         )

df <- df %>% rename(requests_cost = est_total_cost,
                    management_cost = total_est_cost)

df <- df %>% select(agency_name, agency_category, agency_type, reporting_status, total_requests_closed, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, staff_cost, system_cost, service_cost, third_party_cost, management_cost)

colnames(df)
excel_sheets(file)
wa_2017_s4 <- read_excel(file, sheet = 4, skip = 1)

colnames(wa_2017_s4)

wa_2017_s4 <- wa_2017_s4 %>%
  rename(agency_name = `Agency Name`,
         agency_category = `Agency Category`,
         agency_type = `Agency Type`,
         total_open_requests = `Total Open Requests`,
         total_requests_received = `Total Requests Received`,
         total_requests_closed = `Total Requests Closed`,
         reporting_status = `Reporting Status`
         )

df <- full_join(df, wa_2017_s4, by = c("agency_name", "agency_category", "agency_type", "reporting_status", "total_requests_closed"))

df <- df %>% mutate(total_requests = total_requests_received + total_open_requests)

df$year <- 2017

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


colnames(df_2018_2019)

df_2018_2019 <- df_2018_2019 %>%
  rename(avg_est_staff_hours = avg_est_staff_hours_no_rounding,
         requests_cost = total_est_cost,
         management_cost= total_est_costs,
         avg_est_total_cost = avg_cost_per_request,
         staff_cost = staff_costs,
         system_cost = system_costs,
         service_cost = service_costs,
         third_party_cost = third_party_costs
         )

df_2018_2019 <- df_2018_2019 %>%
  select(agency_name, year, agency_category, agency_type, reporting_status, total_requests, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, staff_cost, system_cost, service_cost, third_party_cost, management_cost, total_requests)

colnames(df)
colnames(df_2018_2019)

df <- df %>%
  select(agency_name, year, agency_category, agency_type, reporting_status, total_requests, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, staff_cost, system_cost, service_cost, third_party_cost, management_cost, total_requests)

df_2017_2018_2019 <- rbind(df, df_2018_2019)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(df_2020)

df_2017_2018_2019 <- df_2017_2018_2019 %>%
  select(agency_name, agency_category, agency_type, reporting_status, est_staff_hours_spent, avg_est_staff_hours, requests_cost, avg_est_total_cost, total_litigation_cost, total_requests, year)

df_all <- rbind(df_2020, df_2017_2018_2019)
colnames(df_2020)
colnames(df_2017_2018_2019)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(df_all, file = "jlarc_full_dataset.csv", col_names = TRUE, append = FALSE)
