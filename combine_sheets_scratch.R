# guide from: https://www.geeksforgeeks.org/combine-multiple-excel-worksheets-into-single-dataframe-in-r/

library(tidyverse)
library(readxl)
library(dplyr)

# what directory is the file in
path <- "/home/sophie/Documents/projects/judicial_foia/02-data/unprocessed/washington/"

# set the path to the directory the file is in
setwd(path)

# accessing all the sheets
sheet = excel_sheets("JLARC Public Records Full Dataset.xlsx")

# applying sheet names to df names
df = lapply(setNames(sheet, sheet), function(x) read_excel("JLARC Public Records Full Dataset.xlsx", sheet = x))

# attaching all dfs togethers
df = bind_rows(df, .id = "Sheet")

print(df)

library(rio)
data <- import_list("JLARC Public Records Full Dataset.xlsx", rbind = TRUE)
print(data)
head(data)

colnames(df)
colnames(data)
#~~~~~~~~~~~
library(readr)
library(openxlsx)
library(data.table)

sheet_list <- openxlsx::getSheetNames("JLARC Public Records Full Dataset.xlsx")

print(sheet_list[-c(1,3)])
file_list <- base::lapply(sheet_list[-c(1,3)], openxlsx::read.xlsx, xlsxFile = "JLARC Public Records Full Dataset.xlsx")

list_df <- rbindlist(file_list, idcol = "Agency Name", fill = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


length(unique(list_df$`673.agencies.reported.to.JLARC.they.did.not.have.$100,000.of.staff.and.legal.costs.associated.with.fulfilling.public.records.requests.in.the.preceding.fiscal.year.and.did.not.submit.data.`))
table(list_df$`673.agencies.reported.to.JLARC.they.did.not.have.$100,000.of.staff.and.legal.costs.associated.with.fulfilling.public.records.requests.in.the.preceding.fiscal.year.and.did.not.submit.data.`)
