library(tidyverse)
library(readxl)
library(dplyr)

wa_2017_s13 <- read_excel("/Users/svanpelt/Documents/projects/court_transparency/unprocessed_data/JLARC Public Records Full Dataset.xlsx", sheet = "13. Estimated Costs Responding", range = "A2:F184")

# wa_2017_agency <- wa_2017_s13 %>%
#  filter(`Agency Category` == "State agency, commission, or board")

unique(wa_2017_s13$`Agency Category`)

wa_2017_s13_County <- wa_2017_s13 %>%
  filter(`Agency Category`== "County")

wa_2017_s13_county <- wa_2017_s13_County %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

unique(wa_2017_s13_county$subtype)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wa_2017_sbasline <- read_excel("/Users/svanpelt/Documents/projects/court_transparency/unprocessed_data/JLARC Public Records Full Dataset.xlsx", sheet = "Baseline Data", range = "A2:G187")

wa_2017_sbaseline_county <- wa_2017_sbasline %>%
  filter(`Agency Category` == "County") %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wa_2017_s5 <- read_excel("/Users/svanpelt/Documents/projects/court_transparency/unprocessed_data/JLARC Public Records Full Dataset.xlsx", sheet = "5. Avg Time vs. Actual Time", range = "A2:G185")

wa_2017_s5_county <- wa_2017_s5 %>%
  filter(`Agency Category` == "County") %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wa_2017_s12 <- read_excel("/Users/svanpelt/Documents/projects/court_transparency/unprocessed_data/JLARC Public Records Full Dataset.xlsx", sheet = "12. Staff Time Spent", range = "A2:G184")

wa_2017_s12_county <- wa_2017_s13 %>%
  filter(`Agency Category` == "County") %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wa_2017_s15 <- read_excel("/Users/svanpelt/Documents/projects/court_transparency/unprocessed_data/JLARC Public Records Full Dataset.xlsx", sheet = "15. Litigation Costs", range = "A2:E186")

wa_2017_s15_county <- wa_2017_s13 %>%
  filter(`Agency Category` == "County") %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wa_2017_s15 <- read_excel("/Users/svanpelt/Documents/projects/court_transparency/unprocessed_data/JLARC Public Records Full Dataset.xlsx", sheet = "16. Estimated Costs Managing", range = "A2:I181")

wa_2017_s16_county <- wa_2017_s15 %>%
  filter(`Agency Category` == "County") %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wa_2017_s17 <- read_excel("/Users/svanpelt/Documents/projects/court_transparency/unprocessed_data/JLARC Public Records Full Dataset.xlsx", sheet = "17. Expenses Recovered", range = "A2:G185")

wa_2017_s17_county <- wa_2017_s17 %>%
  filter(`Agency Category` == "County") %>%
  separate(`Agency Name`, into = c("county", "subtype"), sep = "-")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_list = c(wa_2017_s13_county, wa_2017_sbaseline_county, wa_2017_s5_county, wa_2017_s12_county, wa_2017_s15_county, wa_2017_s16_county, wa_2017_s17_county)
df_list %>% 
  reduce(full_join, by='county')
Reduce(function(x, y) merge(x, y, all=TRUE), df_list)