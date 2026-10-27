#Automated checks on the real data, stops at first failure. 


# Source previous script --------------------------------------------------

source(here::here("R", "02-analysis.R"))

# Packages ----------------------------------------------------------------

library(dplyr)
library(purrr)


# Check flagging function -------------------------------------------------

check <- function(name, ok) {
  cat(sprintf("[%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", name))
  if (!isTRUE(ok)) stop("Check failed: ", name, call. = FALSE)
}


# Start logging -----------------------------------------------------------

test_log <- capture.output({
  
  # One-to-one linkage and unmatched IDs (most important, without linkage the full system breaks)
  check("Linkage: no ID is missing or repeated in the crosswalk",
        !anyNA(patient_linkage) && !any(map_lgl(patient_linkage, anyDuplicated)))
  check("Linkage: roster and crosswalk contain the same patients",
        setequal(patients$patient_id, patient_linkage$patient_id))
  check("Linkage: every event row matched the crosswalk (none lost in the join)",
        nrow(events) == nrow(rawfiles$claims_events) + nrow(rawfiles$ehr_events))
  check("Linkage: the joined patient table has one row per roster patient",
        nrow(pt) == nrow(patients) && !anyDuplicated(pt$patient_id))
  
  # Treatment on day 365 counts, day 366 does not (real boundary events)
  boundary <- events %>%
    filter(event_type == "treatment") %>%
    inner_join(select(pt, patient_id, index_date), by = "patient_id") %>%
    mutate(day = as.integer(event_date - index_date))
  day365 <- filter(boundary, day == 365)
  day366 <- filter(boundary, day == 366)
  check(sprintf("Window: all %d real treatment events on day 365 are counted", nrow(day365)),
        nrow(day365) > 0 && nrow(anti_join(day365, tx, by = c("patient_id", "source", "event_date", "code"))) == 0)
  check(sprintf("Window: none of the %d real treatment events on day 366 is counted", nrow(day366)),
        nrow(day366) > 0 && nrow(semi_join(day366, tx, by = c("patient_id", "source", "event_date", "code"))) == 0)
  
  # Each patient counted once (repeated rows, or treatment in both sources)
  repeated <- events %>%
    filter(event_type == "treatment", patient_id %in% capture_cohort$patient_id) %>%
    count(patient_id, source, event_date, code) %>%
    filter(n > 1)
  check(sprintf("Once per patient: %d capture patients have repeated treatment rows, flags still have one row per patient",
                n_distinct(repeated$patient_id)),
        nrow(repeated) > 0 && nrow(flags) == nrow(capture_cohort) && !anyDuplicated(flags$patient_id))
  check("Once per patient: patients treated in both sources count once in the linked total",
        capture_tbl$n_treated[3] == nrow(flags) - sum(!flags$tx_linked))
  check("PSA tests are never counted as treatment", all(tx$event_type == "treatment"))
  
  # Other rules and totals - effective end, capture cohort, linked - claims = EHR
  past_death <- filter(pt, c_end > death_date)
  check(sprintf("Effective end = death date for all %d patients whose claims span runs past death", nrow(past_death)),
        nrow(past_death) > 0 && all(past_death$c_eff_end == past_death$death_date))
  check("Agreement and pattern groups each add up to the capture cohort",
        sum(agreement$n) == nrow(capture_cohort) && sum(patterns$n) == nrow(capture_cohort))
  check("Linked minus claims equals the EHR-only count",
        capture_tbl$n_treated[3] - capture_tbl$n_treated[1] == n_ehr_only)
  check("Mortality uses the baseline cohort denominator",
        mortality$denominator == nrow(baseline_cohort))
  
  # Extension: each part covers its cohort exactly once
  by_age   <- filter(extension, part == "Grade missing, by age (baseline cohort)")
  by_grade <- filter(extension, part == "Treated, by grade (capture cohort)")
  check("Extension: age groups add up to the baseline cohort", sum(by_age$n) == nrow(baseline_cohort))
  check("Extension: grade categories add up to the capture cohort", sum(by_grade$n) == nrow(capture_cohort))
  check("Extension: missing-grade count matches the data-fitness table",
        by_grade$n[by_grade$group == "Missing"] == fitness$grade_missing_n)
  
  cat("All checks passed\n")
})

writeLines(test_log, here("output", "logs", "automated_checks.txt"))
cat(test_log, sep = "\n")



