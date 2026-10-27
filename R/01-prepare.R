#read-verify-clean-check pipeline
#no raw data is modified, but checks and issues are recorded.

# Run to keep scripts standalone ------------------------------------------

source(here::here("R", "00-setup.R"))

# Packages ----------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)

# Verify checksums --------------------------------------------------------
cat("Verifying checksums: \n")
sums <- read.table(here("data", "CHECKSUMS.sha256"), 
                   col.names = c("expected_checksum", "filename"))
actual <- map_chr(here("data", sums$filename), 
                  digest::digest, algo = "sha256", file = TRUE)

mismatch <- sums$filename[actual != sums$expected_checksum]
if(length(mismatch) > 0)
{
  stop("Input files differ: ", paste(mismatch, collapse = ", "))
}

cat("All ", nrow(sums), "files match checksums.\n")

# Loading files -----------------------------------------------------------

#read all types as character, not assuming all valid values. Brief also instructs it.
#not trimming whitespaces as those can be a potential source of errors which I should catch
#blank cells are missing so na being set explicitly

files <- c("claims_events", "claims_patients", "code_map", "ehr_events", 
           "ehr_patients", "patient_linkage", "patients")

rawfiles <- here("data", paste0(files, ".csv")) %>%
  set_names(files) %>%
  map(read_csv, col_types = cols(.default = col_character()), 
      na = "", trim_ws = FALSE)

# Date checks - raw values -------------------------------------------------------------

index_date <- rawfiles$patients$index_date
death_date <- rawfiles$patients$death_date
claims_start <- rawfiles$claims_patients$observation_start
claims_end <- rawfiles$claims_patients$observation_end
ehr_start <- rawfiles$ehr_patients$observation_start
ehr_end <- rawfiles$ehr_patients$observation_end
event_dates <- c(rawfiles$claims_events$event_date, 
                  rawfiles$ehr_events$event_date)

#index date cannot be blank or invalid
bad_index <- is.na(index_date) | invalid_date(index_date)

#death date can be blank but not invalid
bad_death <- invalid_date(death_date)

#spans need valid start, end and no blanks, any one will cause trouble
bad_claims_interval <- is.na(claims_start)|is.na(claims_end)|invalid_date(claims_start)|invalid_date(claims_end)
bad_ehr_interval <- is.na(ehr_start)|is.na(ehr_end)|invalid_date(ehr_start)|invalid_date(ehr_end)

bad_event <- is.na(event_dates) | invalid_date(event_dates)

date_checks <- c(
  "Index date blank or invalid" = sum(bad_index),
  "Death date invalid (blank allowed, no death)" = sum(bad_death),
  "Claims span blank or invalid" = sum(bad_claims_interval),
  "EHR span blank or invalid" = sum(bad_ehr_interval),
  "Event date (Claims/EHR) blank or invalid" = sum(bad_event)
)

# Non-date value checks - raw values -----------------------------------------------------------

age <- rawfiles$patients$age_at_index
psa <- rawfiles$ehr_patients$baseline_psa_ng_ml
psa_number <- as.numeric(psa) #needed to check if PSA values were correct
grade <- rawfiles$ehr_patients$grade_group

#any spaces
all_cells = unlist(rawfiles)
n_spaces <- sum(all_cells != trimws(all_cells), na.rm = TRUE)

#age must be a whole number
bad_age <- !grepl("^\\d+$", age)

# PSA if present must be a number
psa_not_numeric <- !is.na(psa) & is.na(psa_number)
# PSA should be more than 0
psa_not_positive <- !is.na(psa_number) & psa_number <= 0
bad_psa <- psa_not_numeric | psa_not_positive

#grade 1-5 only if present
bad_grade <- !is.na(grade) & !grade %in% 1:5

#part D 0/1 only if present
bad_part_d <- !rawfiles$claims_patients$part_d %in% c("0", "1")

value_checks <- c(
  "Cells with leading/trailing spaces" = n_spaces,
  "age_at_index not a whole number" = sum(bad_age),
  "part_d not 0 or 1" = sum(bad_part_d),
  "PSA present but not a number" = sum(psa_not_numeric),
  "PSA zero or negative" = sum(psa_not_positive),
  "PSA blank (reporting but expected)" = sum(is.na(psa)),
  "Grade group present but not 1-5" = sum(bad_grade),
  "Grade blank (reporting but expected)" = sum(is.na(grade))
)

# Convert columns ---------------------------------------------------------

#since the value checks are done, type conversion can happen now

patients <- rawfiles$patients %>%
  mutate(index_date = as.Date(index_date, format = "%Y-%m-%d"),
         death_date = as.Date(death_date, format = "%Y-%m-%d"),
         age_at_index = as.integer(age_at_index))

patient_linkage = rawfiles$patient_linkage

claims_patients <- rawfiles$claims_patients %>%
  mutate(observation_start = as.Date(observation_start, format = "%Y-%m-%d"),
         observation_end = as.Date(observation_end, format = "%Y-%m-%d"),
         part_d = as.integer(part_d))

ehr_patients <- rawfiles$ehr_patients %>%
  mutate(observation_start = as.Date(observation_start, format = "%Y-%m-%d"),
         observation_end = as.Date(observation_end, format = "%Y-%m-%d"),
         baseline_psa_ng_ml = if_else(bad_psa, NA, psa_number),
         grade_group = if_else(bad_grade, NA, as.integer(grade_group)))


# Range checks ------------------------------------------------------------

#first diagnosis between 2018-01-01 and 2021-12-31
bad_index_range <- patients$index_date < as.Date("2018-01-01") |
  patients$index_date > as.Date("2021-12-31")

#age must be plausible for adults
age_implausible <- patients$age_at_index < 18 | patients$age_at_index > 120

#death cannot occur before index date or after the cutoff to be eligible
death_before_index <- patients$death_date < patients$index_date
death_after_cutoff <- patients$death_date > cutoff

#observation end date cannot be before start date or after cutoff
claims_start_after_end <- claims_patients$observation_start > claims_patients$observation_end
claims_end_after_cutoff <- claims_patients$observation_end > cutoff
ehr_start_after_end <- ehr_patients$observation_start > ehr_patients$observation_end
ehr_end_after_cutoff <- ehr_patients$observation_end > cutoff

# No event can come after the data cutoff
event_after_cutoff <- as.Date(event_dates, format = "%Y-%m-%d") > cutoff

range_checks <- c(
  "index_date outside 2018-2021" = sum(bad_index_range, na.rm = TRUE),
  "age_at_index outside 18-120" = sum(age_implausible, na.rm = TRUE),
  "Death before index date" = sum(death_before_index, na.rm = TRUE),
  "Death after cutoff date" = sum(death_after_cutoff, na.rm = TRUE),
  "Claims observation start after end" = sum(claims_start_after_end, na.rm = TRUE),
  "Claims observation end after cutoff" = sum(claims_end_after_cutoff, na.rm = TRUE),
  "EHR observation start after end" = sum(ehr_start_after_end, na.rm = TRUE),
  "EHR observation end after cutoff" = sum(ehr_end_after_cutoff, na.rm = TRUE),
  "Event after cutoff" = sum(event_after_cutoff, na.rm = TRUE)
)


# ID and link test --------------------------------------------------------

#each roster patient appears once
dup_patient <- duplicated(patients$patient_id)

#no blank or repeated ID in any column
bad_link_id <- sum(is.na(patient_linkage), sapply(patient_linkage, duplicated))

#roster and crosswalk contain the same patients
roster_not_linked <- setdiff(patients$patient_id, patient_linkage$patient_id)

#every crosswalk ID has exactly one row in its source patient table
claims_unmatched <- setdiff(patient_linkage$claims_id, claims_patients$claims_id)
ehr_unmatched <- setdiff(patient_linkage$ehr_id, ehr_patients$ehr_id)
dup_claims_pt <- duplicated(claims_patients$claims_id)
dup_ehr_pt <- duplicated(ehr_patients$ehr_id)

key_checks <- c(
  "Duplicate patient_id in roster" = sum(dup_patient),
  "Crosswalk: blank or repeated ID (any column)" = sum(bad_link_id),
  "Roster patients missing from crosswalk" = length(roster_not_linked),
  "Crosswalk claims_id not in claims_patients" = length(claims_unmatched),
  "Crosswalk ehr_id not in ehr_patients" = length(ehr_unmatched),
  "Duplicate claims_id in claims_patients" = sum(dup_claims_pt),
  "Duplicate ehr_id in ehr_patients" = sum(dup_ehr_pt)
)

#every later step depends on the linkage, so stop here if it is broken
if (sum(bad_link_id) > 0) stop("Crosswalk is not complete and one-to-one")

#values could have been checked independently so not a stop condition, this is present here only.

# events: attach patient_id and code meaning ----------------------------

# add source, attach the shared patient_id through the crosswalk, then the code meaning from code_map 
#code map needs to be matched on source + code

claims_events <- rawfiles$claims_events %>%
  mutate(source = "claims") %>%
  inner_join(select(patient_linkage, patient_id, claims_id), by = "claims_id")

ehr_events <- rawfiles$ehr_events %>%
  mutate(source = "ehr") %>%
  inner_join(select(patient_linkage, patient_id, ehr_id), by = "ehr_id")

events <- bind_rows(claims_events, ehr_events) %>%
  mutate(event_date = as.Date(event_date, format = "%Y-%m-%d")) %>%
  left_join(rawfiles$code_map, by = c("source", "code")) %>%
  select(patient_id, source, event_date, code, event_type, modality)

#event IDs must exist in the crosswalk (unmatched rows would be dropped by the joins)
claims_event_unlinked <- !rawfiles$claims_events$claims_id %in% patient_linkage$claims_id
ehr_event_unlinked <- !rawfiles$ehr_events$ehr_id %in% patient_linkage$ehr_id

#every code must have exactly one meaning per source
dup_code <- duplicated(select(rawfiles$code_map, source, code))
unmapped_code <- is.na(events$event_type)

#identical repeated rows are expected (unique event ID); patients are counted once later
dup_claims_rows <- sum(duplicated(rawfiles$claims_events))
dup_ehr_rows    <- sum(duplicated(rawfiles$ehr_events))

event_checks <- c(
  "Claims event claims_id not in crosswalk" = sum(claims_event_unlinked),
  "EHR event ehr_id not in crosswalk" = sum(ehr_event_unlinked),
  "source + code not unique in code_map" = sum(dup_code),
  "Event code not in code_map" = sum(unmapped_code),
  "Exact duplicate rows in claims_events (expected; kept but reported)" = dup_claims_rows,
  "Exact duplicate rows in ehr_events (expected; kept but reported)" = dup_ehr_rows
)


# events and spans against observation and death ------------

#each event must fall inside its OWN source's observation span

claims_event_spans <- claims_events %>%
  mutate(event_date = as.Date(event_date, format = "%Y-%m-%d")) %>%
  inner_join(claims_patients, by = "claims_id")

ehr_event_spans <- ehr_events %>%
  mutate(event_date = as.Date(event_date, format = "%Y-%m-%d")) %>%
  inner_join(ehr_patients, by = "ehr_id")

claims_event_outside_span <- claims_event_spans$event_date < claims_event_spans$observation_start |
  claims_event_spans$event_date > claims_event_spans$observation_end
ehr_event_outside_span    <- ehr_event_spans$event_date < ehr_event_spans$observation_start |
  ehr_event_spans$event_date > ehr_event_spans$observation_end

#no event can come after the patient's death
event_deaths <- left_join(events, select(patients, patient_id, death_date), by = "patient_id")
event_after_death <- event_deaths$event_date > event_deaths$death_date

#recorded span running past death is an expected artifact (truncated at death for analysis)
claims_deaths <- claims_patients %>%
  inner_join(patient_linkage, by = "claims_id") %>%
  inner_join(select(patients, patient_id, death_date), by = "patient_id")
ehr_deaths <- ehr_patients %>%
  inner_join(patient_linkage, by = "ehr_id") %>%
  inner_join(select(patients, patient_id, death_date), by = "patient_id")

claims_end_after_death <- claims_deaths$observation_end > claims_deaths$death_date
ehr_end_after_death <- ehr_deaths$observation_end > ehr_deaths$death_date

plausibility_checks <- c(
  "Claims event outside claims observation span" = sum(claims_event_outside_span),
  "EHR event outside EHR observation span" = sum(ehr_event_outside_span),
  "Event after death" = sum(event_after_death, na.rm = TRUE),
  "Claims recorded end after death (artefact; truncated)" = sum(claims_end_after_death, na.rm = TRUE),
  "EHR recorded end after death (artefact; truncated)" = sum(ehr_end_after_death, na.rm = TRUE)
)


# write the issue log ----------------------------------

all_checks <- c(date_checks, value_checks, range_checks, key_checks, event_checks, plausibility_checks)
checks <- tibble(check = names(all_checks), n_flagged = all_checks)

# issue log: problems that changed or could affect the data
grade_rows <- rawfiles$ehr_patients[bad_grade, ]
psa_rows   <- rawfiles$ehr_patients[bad_psa, ]

issues <- bind_rows(
  tibble(file = "ehr_patients.csv", record_id = grade_rows$ehr_id, column = "grade_group",
         original_value = grade_rows$grade_group, issue = "Grade group outside 1-5",
         action = "Set to missing", reason = "Grade group outside 1-5",
         likely_impact = "Grade missingness up by this count"),
  tibble(file = "ehr_patients.csv", record_id = psa_rows$ehr_id, column = "baseline_psa_ng_ml",
         original_value = psa_rows$baseline_psa_ng_ml, issue = "PSA not a positive number",
         action = "Set to missing", reason = "Valid PSA is positive",
         likely_impact = "PSA missingness up by this count"),
  tibble(file = c("claims_events.csv", "ehr_events.csv"), record_id = "(summary)",
         column = "(whole row)", original_value = paste(c(dup_claims_rows, dup_ehr_rows), "identical rows"),
         issue = "Repeated identical rows", action = "Kept; each patient counted once per source",
         reason = "No event ID; repeats are expected", likely_impact = "None on yes/no treatment flags")
)

write_csv(checks, here("output", "logs", "data_checks_summary.csv"))
write_csv(issues, here("output", "logs", "data_issues.csv"))

cat("Checks run:", nrow(checks), ", Flagged:", sum(checks$n_flagged > 0),
    ", Issues logged:", nrow(issues), "\n")






