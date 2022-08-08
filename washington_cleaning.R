library(tidyverse)
library(readxl)


# what directory is the file in
path <- "/home/sophie/Documents/projects/judicial_foia/02-data/unprocessed/washington/"

# set the path to the directory the file is in
setwd(path)

# name of the file
file <- "JLARC Public Records Full Dataset.xlsx"


# get list of excel file's sheets
excel_sheets(file)[-c(1, 2, 3)]
sheets <- print(excel_sheets(file)[-c(1, 2, 3, 5)])

# load all the sheets into a list that contains each sheet as a tibble
list_all <- lapply(sheets, function(x) read_excel(file, sheet = x, skip = 1))

# change the name of each tibble in list_all to the name of the sheet
names(list_all) <- sheets

# can see the head of the first sheet, looks good
head(list_all$`Baseline Data`)

# the Best Practices one is weird, I'll have to bring it in separately

# using purrr::reduce to use inner_join repeatedly
df_2017 <- list_all %>%
  reduce(left_join, by = "Agency Name")

list_all$`Baseline Data`

# So Baseline has 185 rows, and the whole dataset has 198, so looks like we
# should have all the data

best_practices_2017 <- read_excel(file, sheet = "1. Best Practices", skip = 2)

df_2017 <- left_join(df_2017, best_practices_2017, by = "Agency Name")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now I have my dataset, so lets clean this up

df_2017_new <- df_2017
colnames(df_2017_new) <- make.names(colnames(df_2017_new), unique = TRUE)
colnames(df_2017_new)

df_2017_new <- df_2017_new %>%
  select(Agency.Name, 
         Total.Open.Requests, 
         Total.Requests.Received, 
         Total.Requests.Closed.x, 
         Agency.Category.x, 
         Agency.Type.x, 
         Reporting.Status.x, 
         Total.Response.Days, 
         Total.Requests.After.Five.Day.Period, 
         Average.Response.Days, 
         Closed.Within.Five.Days, 
         Estimate.Provided.Beyond.Five.Days, 
         Partial.Fulfilled.Within.Five.Days, 
         Percent.Requests.Completed.Within.Five.Days, 
         Percent.Requests.For.Estimates.Greater.Than.Five.Days, 
         Requests.With.Final.Disposition, 
         Days.To.Final.Disposition, 
         Average.Days.To.Final.Disposition, 
         Requests.With.Estimate.Provided, 
         Average.Days.Estimate, 
         Average.Days.Actual, 
         Requests.Needing.Clarification, 
         Number.of.Requests.Denied.In.Full, 
         Number.of.Requests.Denied.Partial, 
         Most.Common.Reasons.for.Denial, 
         Requests.Abandoned.By.Requesters, 
         Total.Requests, 
         Requester.Type, 
         Requests.Fulfilled.Electronically, 
         Requests.Fulfilled.Physically, 
         Requests.Fulfilled.Combination, 
         Requests.Closed.No.Response, 
         Percent.Requests.Fulfilled.Electronically,
         Percent.Requests.Fulfilled.Physically, 
         Percent.Requests.Fulfilled.Combination, 
         Percent.Requests.Closed.No.Response,
         Requests.Scanned,
         Estimated.Staff.Hours.Spent,
         Average.Estimated.Staff.Hours.Spent...No.Rounding.,
         Estimated.Total.Cost,
         Average.Estimated.Total.Cost,
         Total.Claims,
         Claim.Type,
         Total.Litigation.Cost, 
         Staff.Cost,
         System.Cost,
         Service.Cost,
         Third.Party.Cost,
         Total.Estimated.Cost,
         Expenses.Recovered,
         Customized.Service.Charges,
         Customized.Service.Charge.Description,
         Agency.has.assigned.overall.responsibility.for.managing.and.retaining.records.to.someone..records.officer.,
         Agency.has.told.Washington.State.Archives.who.their.assigned.person.is,
         Assigned.person.has.the.ability.to.influence.the.agency.s.policies..procedures..and.compliance,
         Assigned.person.is.part.of.the.agency.s.information.governance.team,
         Agency.has.policies.or.procedures.governing.the.management.of.records,
         Policies.and.procedures.are.applicable.to.all.record.formats..including.emerging.technologies.such.as.social.media.,
         Policies.and.procedures.are.part.of.a.larger.information.governance.framework,
         Agency.has.appropriate.software.systems.to.manage.and.retain..email..social.media..Word.documents..spreadsheets..PowerPoints..text.messages..websites..etc.,
         Software.systems.include.retention.management.functionality,
         Agency.has.implemented.or.is.in.the.process.of.implementing.an.enterprise.content.management.system,
         Elected.officials.have.completed.open.government.training,
         Records.officers.have.completed.open.government.training,
         All.other.staff.have.been.trained.to.manage.the.records.they.create.or.receive,
         Records.and.information.management.training.is.part.of.new.employee.orientation,
         Agency.offers.internal.records.and.information.management.training.on.a.regular.basis,
         Key.staff.know.how.to.locate.all.records.retention.schedules.which.are.applicable.to.the.agency..how.to.apply.retention..and.what.records.can.be.considered.transitory,
         Paper.records.have.been.inventoried.at.least.once.within.the.last.10.years,
         Electronic.records.have.been.inventoried.at.least.once.within.the.last.10.years,
         Records.are.inventoried.on.a.regular..systematic.basis,
         Some.coordination.at.the.work.group.level.regarding.where.records.are.stored.and.the.naming.conventions.used,
         Records.are.organized.through.agency.wide.file.plans.and.or.file.naming.conventions,
         Electronic.records.are.retained.in.electronic.format,
         Paper.records.are.either.retained.in.paper.format.or.scanned.and.retained.in.electronic.format.according.to.Washington.State.Archives..scan...toss.requirements,
         Electronic.records.are.migrated.to.new.formats.as.needed,
         Safeguards.are.in.place.to.protect.against.accidental.or.deliberate.destruction.of.records,
         Records.are.destroyed.or.transferred.as.part.of.a.planned.and.systematic.process,
         Records.are.destroyed.or.transferred.to.the.Washington.State.Archives.at.the.end.of.their.retention.periods,
         Essential.records.are.identified,
         Agency.creates.back.ups.of.essential.records.on.a.routine..systematic.basis,
         Ability.to.restore.from.back.up.files.is.tested.checked.regularly,
         )

df_2017_tiny <- df_2017_new %>%
  select(Agency.Name,
         Agency.Category.x, 
         Agency.Type.x, 
         Reporting.Status.x,
         Total.Requests,
         Percent.Requests.Completed.Within.Five.Days,
         Average.Days.Actual,
         Requests.Fulfilled.Electronically, 
         Requests.Fulfilled.Physically, 
         Requests.Fulfilled.Combination, 
         Requests.Closed.No.Response, 
         Percent.Requests.Fulfilled.Electronically,
         Percent.Requests.Fulfilled.Physically, 
         Percent.Requests.Fulfilled.Combination, 
         Percent.Requests.Closed.No.Response,
         Requests.Scanned,
         Estimated.Staff.Hours.Spent,
         Average.Estimated.Staff.Hours.Spent...No.Rounding.,
         Estimated.Total.Cost,
         Average.Estimated.Total.Cost,
         Total.Litigation.Cost, 
         Staff.Cost,
         System.Cost,
         Service.Cost,
         Third.Party.Cost,
         Total.Estimated.Cost,
         Expenses.Recovered,
         Agency.has.assigned.overall.responsibility.for.managing.and.retaining.records.to.someone..records.officer.,
         Agency.has.policies.or.procedures.governing.the.management.of.records,
         Agency.has.appropriate.software.systems.to.manage.and.retain..email..social.media..Word.documents..spreadsheets..PowerPoints..text.messages..websites..etc.,
         Elected.officials.have.completed.open.government.training,
         Records.officers.have.completed.open.government.training,
         All.other.staff.have.been.trained.to.manage.the.records.they.create.or.receive,
         Records.and.information.management.training.is.part.of.new.employee.orientation,
         Agency.offers.internal.records.and.information.management.training.on.a.regular.basis,
         Some.coordination.at.the.work.group.level.regarding.where.records.are.stored.and.the.naming.conventions.used,
         )

df_2017_tiny_above_100k <- df_2017_tiny %>% 
  filter(Reporting.Status.x == "Above the $100k threshold. Report submittal required.")

ggplot(data  = df_2017_tiny_above_100k, mapping = aes(x = Average.Estimated.Total.Cost, y = Elected.officials.have.completed.open.government.training)) +
  geom_boxplot()


df_2017_tiny %>%
  sapply(max, na.rm = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


write_csv(df_2017_tiny_above_100k, file = "jlarc_2017_larger.csv", col_names = TRUE, append = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_2018 <- "2018 JLARC Public Records Full Dataset.xlsx"
df_2019 <- "2019 JLARC Public Records Full Dataset.xlsx"
df_2020 <- "2020 JLARC Public Records Full Dataset.xlsx"

# get list of excel file's sheets
excel_sheets(df_2020)[-c(1, 2, 3)]
sheets_2018 <- print(excel_sheets(df_2018)[-c(1, 2, 3, 10, 11, 16)])
sheets_2019 <- print(excel_sheets(df_2019)[-c(1, 2, 3, 11, 16)])
sheets_2020 <- print(excel_sheets(df_2020)[c(4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21)])

# load all the sheets into a list that contains each sheet as a tibble
list_2018 <- lapply(sheets_2018, function(x) read_excel(df_2018, sheet = x, skip = 1))
list_2019 <- lapply(sheets_2019, function(x) read_excel(df_2019, sheet = x, skip = 1))
list_2020 <- lapply(sheets_2020, function(x) read_excel(df_2020, sheet = x, skip = 1))

# change the name of each tibble in list_all to the name of the sheet
names(list_2018) <- sheets_2018
names(list_2019) <- sheets_2019
names(list_2020) <- sheets_2020

# can see the head of the first sheet, looks good
head(list_2020$Baseline)

# using purrr::reduce to use inner_join repeatedly
df_2018 <- list_2018 %>%
  reduce(left_join, by = "Agency name")

df_2019 <- list_2019 %>%
  reduce(left_join, by = "Agency name")

df_2020 <- list_2020 %>%
  reduce(left_join, by = "Agency name")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


df_2018_new <- df_2018
colnames(df_2018_new) <- make.names(colnames(df_2018_new), unique = TRUE)
colnames(df_2018_new)

df_2019_new <- df_2019
colnames(df_2019_new) <- make.names(colnames(df_2019_new), unique = TRUE)
colnames(df_2019_new)

df_2020_new <- df_2020
colnames(df_2020_new) <- make.names(colnames(df_2020_new), unique = TRUE)
colnames(df_2020_new)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_2018_tiny <- df_2018_new %>%
  select(Agency.name,
         Agency.category.x, 
         Agency.Type.x, 
         Reporting.Status.x,
         Total.open.requests,
         Requests.open.at.beginning.of.reporting.and.closed.during.reporting.period,
         Total.requests.received,
         Total.requests.closed,
         Total.requests.closed..open.at.beginning.of.reporting.and.closed.during.plus.closed.,
         Requests.closed.within.5.days,
         Number.of.requests.with.final.disposition,
         Number.of.days.to.final.disposition,
         Median.number.of.days.to.final.disposition,
         Average.number.of.days.to.final.disposition,
         Denied.in.Full,
         Denied.Partial,
         Requests.fulfilled.electronically,
         Requests.fulfilled.physically,
         Requests.fulfilled.combination,
         Requests.closed.no.responsive.records,
         Percent.requests.fulfilled.electronically,
         Percent.requests.fulfilled.physically,
         Percent.requests.fulfilled.combination,
         Percent.requests.closed.no.responsive.records,
         Requests.Scanned,
         Estimated.Staff.Hours.Spent,
         Total.Requests,
         Average.estimated.staff.hours.spent...No.rounding.,
         Average.cost.per.request,
         Total.estimated.cost,
         Agency.applied.an.overhead.rate,
         Total.litigation.cost,
         Staff.costs,
         System.costs,
         Service.costs,
         Third.party.costs,
         Total.estimated.cost,
         Agency.applied.overhead.rate,
         Expenses.recovered,
         Customized.service.charges
  )

df_2019_tiny <- df_2019_new %>%
  select(Agency.name,
         Agency.category.x,
         Agency.type.x,
         Reporting.status.x,
         Total.open.requests,
         Requests.open.at.beginning.of.reporting.and.closed.during.reporting.period,
         Total.requests.received,
         Total.requests.closed,
         Total.requests.closed..open.at.beginning.of.reporting.and.closed.during.plus.closed.,
         Requests.closed.within.5.days,
         Number.of.requests.where.an.estimated.response.time.beyond.5.days.was.provided,
         Number.of.requests.with.final.disposition,
         Number.of.days.to.final.disposition,
         Median.number.of.days.to.final.disposition,
         Average.number.of.days.to.final.disposition,
         Requests.requiring.clarification,
         Denied.in.full,
         Partially.denied,
         Requests.abandoned.by.requesters,
         Requests.fulfilled.electronically,
         Requests.fulfilled.physically,
         Requests.fulfilled.combination,
         Requests.closed...no.responsive.records,
         Percent.of.requests.fulfilled.electronically,
         Percent.requests.fulfilled.physically,
         Percent.requests.fulfilled.combination,
         Percent.requests.closed...no.responsive.records,
         Requests.scanned,
         Estimated.staff.hours.spent,
         Total.requests,
         Average.estimated.staff.hours.spent..No.rounding.,
         Average.cost.per.request,
         Total.estimated.cost,
         Agency.applied.an.overhead.rate.x,
         Total.litigation.cost,
         Staff.costs,
         System.costs,
         Service.costs,
         Third.party.costs,
         Total.estimated.costs,
         Agency.applied.an.overhead.rate.y,
         Expenses.recovered,
         Customized.service.charges
         )

df_2020_tiny <- df_2020_new %>%
  select(
    Agency.name,
    Agency.category.x,
    Agency.type.x,
    Reporting.status.x,
    Total.open.requests,
    Requests.open.at.beginning.of.reporting.and.closed.during.reporting.period,
    Total.requests.received,
    Requests.closed,
    Total.requests.closed..open.at.beginning.of.reporting.and.closed.during.plus.closed.,
    Requests.closed.within.5.days,
    Number.of.requests.where.an.estimated.response.time.beyond.5.days.was.provided,
    Number.of.requests.with.final.disposition,
    Number.of.days.to.final.disposition,
    Median.number.of.days.to.final.disposition,
    Average.number.of.days.to.final.disposition,
    Requests.requiring.clarification,
    Denied.in.full,
    Partially.denied,
    Requests.abandoned.by.requesters,
    Requests.fulfilled.electronically,
    Requests.fulfilled.physically, 
    Requests.fulfilled.combination,
    Requests.closed...no.responsive.records,
    Percent.of.requests.fulfilled.electronically,
    Percent.of.requests.fulfilled.physically,
    Percent.of.requests.fulfilled.combination,
    Percent.requests.closed...no.responsive.records,
    Requests.scanned,
    Estimated.staff.hours.spent,
    Total.requests..open...received.,
    Average.estimated.staff.hours.spent..No.rounding.,
    Average.estimated.cost.per.request,
    Total.cost.estimated,
    Agency.applied.an.overhead.rate.x,
    Total.claims.filed,
    Total.litigation.cost,
    Staff.costs,
    System.costs,
    Service.costs,
    Third.party.costs,
    Total.estimated.costs,
    Agency.applied.an.overhead.rate.y,
    Expenses.recovered,
    Customized.service.charges,
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Lets have all the colnames be the same so we can append them all together!
# Also need to add the year 

names(df_2017_tiny) <- tolower(names(df_2017_tiny))
names(df_2018_tiny) <- tolower(names(df_2018_tiny))
names(df_2019_tiny) <- tolower(names(df_2019_tiny))
names(df_2020_tiny) <- tolower(names(df_2020_tiny))

colnames(df_2017_tiny)
df_2017_tiny <- df_2017_tiny %>%
  rename(
    agency_name = agency.name,
    agency_category = agency.category.x,
    agency_type = agency.type.x,
    reporting_status = reporting.status.x,
    total_requests = total.requests,
    percent_req_completed_within_5_days = percent.requests.completed.within.five.days,
    average_days_actual = average.days.actual,
    reqs_fulfilled_electronically = requests.fulfilled.electronically,
    reqs_fulfilled_physically = requests.fulfilled.physically,
    reqs_fulfilled_combination = requests.fulfilled.combination,
    percent_reqs_fulfilled_electronically = percent.requests.fulfilled.electronically,
    percent_reqs_fulfilled_physically = percent.requests.fulfilled.physically,
    percent_reqs_fulfilled_combination = percent.requests.fulfilled.combination,
    reqs_scanned = requests.scanned,
    est_staff_hours_spent = estimated.staff.hours.spent,
    avg_est_staff_hours_spent = average.estimated.staff.hours.spent...no.rounding.,
    est_total_cost_responding_to_reqs = estimated.total.cost,
    avg_est_cost_responding_per_req = average.estimated.total.cost,
    total_litigation_cost = total.litigation.cost,
    man_staff_cost = staff.cost,
    man_system_cost = system.cost,
    man_service_cost = service.cost,
    man_third_party_cost = third.party.cost,
    man_total_cost = total.estimated.cost,
    expenses_recovered = expenses.recovered
  )

df_2017_ready_to_merge <- df_2017_tiny %>%
  select(
    agency_name,
    agency_category,
    agency_type,
    reporting_status,
    total_requests,
    percent_req_completed_within_5_days,
    average_days_actual,
    reqs_fulfilled_electronically,
    reqs_fulfilled_physically,
    reqs_fulfilled_combination,
    percent_reqs_fulfilled_electronically,
    percent_reqs_fulfilled_physically,
    percent_reqs_fulfilled_combination,
    reqs_scanned,
    est_staff_hours_spent,
    avg_est_staff_hours_spent,
    est_total_cost_responding_to_reqs,
    avg_est_cost_responding_per_req,
    total_litigation_cost,
    man_staff_cost,
    man_system_cost,
    man_service_cost,
    man_third_party_cost,
    man_total_cost,
    expenses_recovered,
  )
df_2017_ready_to_merge$year <- 2017
write_csv(df_2017_ready_to_merge, file = "jlarc_2017_ready_to_merge.csv", col_names = TRUE, append = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colnames(df_2018_tiny)
df_2018_tiny <- df_2018_tiny %>%
  rename(
    agency_name = agency.name,
    agency_category = agency.category.x,
    agency_type = agency.type.x,
    reporting_status = reporting.status.x,
    total_requests = total.requests,
    percent_req_completed_within_5_days = percent.requests.completed.within.five.days,
    average_days_actual = average.days.actual,
    reqs_fulfilled_electronically = requests.fulfilled.electronically,
    reqs_fulfilled_physically = requests.fulfilled.physically,
    reqs_fulfilled_combination = requests.fulfilled.combination,
    percent_reqs_fulfilled_electronically = percent.requests.fulfilled.electronically,
    percent_reqs_fulfilled_physically = percent.requests.fulfilled.physically,
    percent_reqs_fulfilled_combination = percent.requests.fulfilled.combination,
    reqs_scanned = requests.scanned,
    est_staff_hours_spent = estimated.staff.hours.spent,
    avg_est_staff_hours_spent = average.estimated.staff.hours.spent...no.rounding.,
    est_total_cost_responding_to_reqs = estimated.total.cost,
    avg_est_cost_responding_per_req = average.estimated.total.cost,
    total_litigation_cost = total.litigation.cost,
    man_staff_cost = staff.cost,
    man_system_cost = system.cost,
    man_service_cost = service.cost,
    man_third_party_cost = third.party.cost,
    man_total_cost = total.estimated.cost,
    expenses_recovered = expenses.recovered
  )

df_2017_ready_to_merge <- df_2017_tiny %>%
  select(
    agency_name,
    agency_category,
    agency_type,
    reporting_status,
    total_requests,
    percent_req_completed_within_5_days,
    average_days_actual,
    reqs_fulfilled_electronically,
    reqs_fulfilled_physically,
    reqs_fulfilled_combination,
    percent_reqs_fulfilled_electronically,
    percent_reqs_fulfilled_physically,
    percent_reqs_fulfilled_combination,
    reqs_scanned,
    est_staff_hours_spent,
    avg_est_staff_hours_spent,
    est_total_cost_responding_to_reqs,
    avg_est_cost_responding_per_req,
    total_litigation_cost,
    man_staff_cost,
    man_system_cost,
    man_service_cost,
    man_third_party_cost,
    man_total_cost,
    expenses_recovered,
  )
df_2017_ready_to_merge$year <- 2017
