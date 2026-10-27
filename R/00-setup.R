#setting, rules and helper functions needed at all steps

# Packages ----------------------------------------------------------------

library(here)

# Directory structure -----------------------------------------------------
datadir <- here("data")
output_tables <- here("output", "tables")
output_figures <- here("output", "figures")
output_logs <- here("output", "logs")

#if folders not present
for (d in c(output_tables, output_figures, output_logs)) 
  dir.create(d, recursive = TRUE, showWarnings = FALSE)


# Cohort settings ---------------------------------------------------------

cutoff <- as.Date("2023-12-31") #data cutoff according to brief
follow_up <- 365 #treatment and/or death window, day 0-365 both inclusive
lookback <- 365 #days of EHR and claims data required for eligibility before diagnosis
age_min <- 66 #medicare claims start usually ~age 65, so 1 year lookback adds a year


# Date checks ------------------------------------------------------------

invalid_date <- function(x)
{
  !is.na(x) & (!grepl("^\\d{4}-\\d{2}-\\d{2}$", x) | 
                 is.na(as.Date(x, format = "%Y-%m-%d")))              
}

#3 checks: blank cell, and if not blank, then wrong format date or correct format but impossible date