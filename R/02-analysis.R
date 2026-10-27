# main analysis: cohorts, treatment capture, patterns, mortality, data fitness, extension

# Source previous script --------------------------------------------------

source(here::here("R", "01-prepare.R"))


# Packages ----------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)


# One row per patient with EHR and claim spans ----------------------------

pt <- patients %>%
  inner_join(patient_linkage, by = "patient_id") %>%
  inner_join(rename(claims_patients, c_start = observation_start, c_end = observation_end), by = "claims_id") %>%
  inner_join(rename(ehr_patients, e_start = observation_start, e_end = observation_end), by = "ehr_id") %>%
  mutate(
    day365 = index_date + follow_up,
    #a source cannot observe after death or after the data cutoff
    c_eff_end = pmin(c_end, death_date, cutoff, na.rm = TRUE),
    e_eff_end = pmin(e_end, death_date, cutoff, na.rm = TRUE)
  )



# Defining the baseline and capture cohorts -------------------------------

pt <- pt %>%
  mutate(
    #age 66 or older
    step1_age = age_at_index >= age_min,
    #both sources observing from 365 days before diagnosis through diagnosis (failing removes patient from baseline cohort)
    step2_lookback = c_start <= index_date - lookback & e_start <= index_date - lookback &
      c_eff_end >= index_date & e_eff_end >= index_date,
    #both sources still observing on day 365 (not dead/left the system)
    step3_followup = c_eff_end >= day365 & e_eff_end >= day365,
    
    in_baseline = step1_age & step2_lookback, # used for mortality
    in_capture  = in_baseline & step3_followup # used for treatment
  )

baseline_cohort <- filter(pt, in_baseline)
capture_cohort  <- filter(pt, in_capture)


flow <- tibble(
  step = c("Roster", "Age >= 66", "Lookback in both claims+EHR", "Follow-up to day 365 in both claims+EHR"),
  cohort = c("", "", "Baseline cohort (mortality)", "Capture cohort (treatment)"),
  remaining = c(nrow(pt), sum(pt$step1_age), nrow(baseline_cohort), nrow(capture_cohort)),
  excluded = c(NA, -diff(remaining))
)

#how many left the cohort, and how many died, so excluded from capture cohort
step3 <- tibble(
  excluded           = nrow(baseline_cohort) - nrow(capture_cohort),
  died_before_day365 = sum(baseline_cohort$death_date < baseline_cohort$day365, na.rm = TRUE)
)

# Treatment -----------------------------------------------------------

# qualifying events: code_map "treatment" (not PSA tests), days 0-365 inclusive,
# and only while that source is still observing the patient

tx <- events %>%
  filter(event_type == "treatment") %>%
  inner_join(select(pt, patient_id, index_date, c_eff_end, e_eff_end), by = "patient_id") %>%
  mutate(day = as.integer(event_date - index_date)) %>%
  filter(day >= 0, day <= follow_up,
         event_date <= if_else(source == "claims", c_eff_end, e_eff_end))

claims_treated <- unique(tx$patient_id[tx$source == "claims"])
ehr_treated <- unique(tx$patient_id[tx$source == "ehr"])

#number of different modalities per patient, and the modality if only one
modalities <- tx %>%
  distinct(patient_id, modality) %>%
  group_by(patient_id) %>%
  summarise(n_modalities = n(), modality = first(modality))

#one row per capture patient: yes/no per source, linked = either claims or EHR is linked + pattern
flags <- capture_cohort %>%
  select(patient_id, part_d) %>%
  left_join(modalities, by = "patient_id") %>%
  mutate(
    tx_claims = patient_id %in% claims_treated,
    tx_ehr    = patient_id %in% ehr_treated,
    tx_linked = tx_claims | tx_ehr,
    pattern = case_when(
      is.na(n_modalities)         ~ "No recorded treatment",
      n_modalities > 1            ~ "Multiple modalities",
      modality == "prostatectomy" ~ "Prostatectomy only",
      modality == "radiation"     ~ "Radiation only",
      modality == "systemic"      ~ "Systemic therapy only"
    )
  )

n_cap <- nrow(flags)


# Treatment capture with Wilson CIs ---------------------------------------
# prop.test handles one proportion at a time, hence rowwise();
# correct = FALSE gives the plain Wilson score interval

capture_tbl <- tibble(
  view       = c("Claims", "EHR", "Linked (either)"),
  n_treated  = c(sum(flags$tx_claims), sum(flags$tx_ehr), sum(flags$tx_linked)),
  n_patients = n_cap
) %>%
  rowwise() %>%
  mutate(pct      = 100 * n_treated / n_patients,
         ci_lower = 100 * prop.test(n_treated, n_patients, correct = FALSE)$conf.int[1],
         ci_upper = 100 * prop.test(n_treated, n_patients, correct = FALSE)$conf.int[2]) %>%
  ungroup()

#agreement between sources: every patient in exactly one group
agreement <- tibble(
  group = c("Both sources", "Claims only", "EHR only", "Neither"),
  n     = c(sum(flags$tx_claims & flags$tx_ehr),
            sum(flags$tx_claims & !flags$tx_ehr),
            sum(!flags$tx_claims & flags$tx_ehr),
            sum(!flags$tx_claims & !flags$tx_ehr))
)

#linked minus claims equals the EHR-only share, so its Wilson CI is the CI of the gain
n_ehr_only <- agreement$n[agreement$group == "EHR only"]
gain_ci    <- prop.test(n_ehr_only, n_cap, correct = FALSE)$conf.int
gain <- tibble(comparison = "Linked minus claims (= EHR-only share)",
               n = n_cap, n_ehr_only = n_ehr_only, pct = 100 * n_ehr_only / n_cap,
               ci_lower = 100 * gain_ci[1], ci_upper = 100 * gain_ci[2])

#treatment patterns in five mutually exclusive groups
pattern_levels <- c("No recorded treatment", "Prostatectomy only", "Radiation only",
                    "Systemic therapy only", "Multiple modalities")
patterns <- tibble(pattern = pattern_levels,
                   n = sapply(pattern_levels, \(p) sum(flags$pattern == p)),
                   pct = 100 * n / n_cap)


# 365-day all-cause mortality (baseline cohort) ---------------------------

n_base   <- nrow(baseline_cohort)
n_deaths <- sum(baseline_cohort$death_date <= baseline_cohort$day365, na.rm = TRUE)
mort_ci  <- prop.test(n_deaths, n_base, correct = FALSE)$conf.int

mortality <- tibble(endpoint = "All-cause death, days 0-365 after diagnosis (registry)",
                    cohort = "Baseline cohort", denominator = n_base, deaths = n_deaths,
                    pct = 100 * n_deaths / n_base,
                    ci_lower = 100 * mort_ci[1], ci_upper = 100 * mort_ci[2])


# Data fitness in the capture cohort -------------------------------------------

fitness <- tibble(
  n_capture         = n_cap,
  median_psa_valid  = median(capture_cohort$baseline_psa_ng_ml, na.rm = TRUE),
  psa_missing_n     = sum(is.na(capture_cohort$baseline_psa_ng_ml)),
  psa_missing_pct   = 100 * psa_missing_n / n_cap,
  grade_missing_n   = sum(is.na(capture_cohort$grade_group)),
  grade_missing_pct = 100 * grade_missing_n / n_cap,
  part_d_0_n        = sum(capture_cohort$part_d == 0)
)



# Extension: Closer look at patients with a missing Grade Group --------

# (a) Who lacks a recorded grade: uses the BASELINE cohort
# (b) How are these individuals treated: uses the CAPTURE cohort


add_grade_vars <- function(d) {
  d %>% mutate(
    grade_missing = is.na(grade_group),
    grade_cat = if_else(is.na(grade_group), "Missing", paste("Grade", grade_group)),
    age_group = case_when(age_at_index <= 70 ~ "66-70",
                          age_at_index <= 75 ~ "71-75",
                          age_at_index <= 80 ~ "76-80",
                          TRUE               ~ "81+"))
}

baseline_grade <- add_grade_vars(baseline_cohort)
capture_grade  <- add_grade_vars(capture_cohort) %>%
  left_join(select(flags, patient_id, tx_linked, pattern), by = "patient_id")

# (a) How common is missing grade in each age group? (baseline cohort)
missing_by_age <- baseline_grade %>%
  group_by(group = age_group) %>%
  summarise(n = n(), count = sum(grade_missing)) %>%
  mutate(part = "Grade missing, by age (baseline cohort)")

# (b) How often is each grade treated, linked days 0-365? (capture cohort)
treated_by_grade <- capture_grade %>%
  group_by(group = grade_cat) %>%
  summarise(n = n(), count = sum(tx_linked), systemic_only = sum(pattern == "Systemic therapy only")) %>%
  mutate(part = "Treated, by grade (capture cohort)")

extension <- bind_rows(missing_by_age, treated_by_grade) %>%
  rowwise() %>%
  mutate(pct      = 100 * count / n,
         ci_lower = 100 * prop.test(count, n, correct = FALSE)$conf.int[1],
         ci_upper = 100 * prop.test(count, n, correct = FALSE)$conf.int[2]) %>%
  ungroup() %>%
  select(part, group, n, count, pct, ci_lower, ci_upper, systemic_only)


# Plots -----------------------------------------------------------------

#Plot required 1: Treatment capture, how much does linking add?
plot_a <- ggplot(capture_tbl, aes(factor(view, levels = view), pct)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_text(aes(label = sprintf("%.1f%% (%d/%d)", pct, n_treated, n_patients)),
            nudge_x = 0.12, hjust = 0, size = 3.5) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = NULL, y = "Patients with a recorded treatment, days 0-365 (%)",
       title = sprintf("Plot A. Linking adds %.1f points to claims alone (%.1f%% to %.1f%%)",
                       capture_tbl$pct[3] - capture_tbl$pct[1], capture_tbl$pct[1], capture_tbl$pct[3]),
       subtitle = sprintf("Capture cohort, n = %d patients (same patients in every column)", n_cap),
       caption = "Error bars: Wilson score 95% confidence intervals") +
  theme_minimal(base_size = 12)

#Plot required 2: Initial treatment patterns in year 1
plot_b <- ggplot(patterns, aes(n, factor(pattern, levels = rev(pattern_levels)))) +
  geom_col(fill = "grey60") +
  geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Patients", y = NULL,
       title = sprintf("Plot B. %.0f%% have no treatment recorded in either source by day 365",
                       patterns$pct[1]),
       subtitle = sprintf("Linked records, capture cohort, n = %d; groups are mutually exclusive", n_cap),
       caption = "Combinations of modalities observed in days 0-365, not sequence or intent") +
  theme_minimal(base_size = 12)

#Plot 3: extension
plot_x <- ggplot(filter(extension, part == "Treated, by grade (capture cohort)"),
                 aes(factor(group, levels = group), pct, colour = group == "Missing")) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_text(aes(label = sprintf("%.0f%% (%d/%d)", pct, count, n)),
            nudge_x = 0.15, hjust = 0, size = 3.4, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "grey35", `TRUE` = "#b7791f"), guide = "none") +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Baseline grade group", y = "Treated in days 0-365, linked records (%)",
       title = "Extension. Missing grade: treated more than grades 1-2, less than 4-5",
       subtitle = sprintf("Capture cohort, n = %d; grade missing for %d", n_cap, sum(capture_grade$grade_missing)),
       caption = "Wilson score 95% CI. Descriptive; not adjusted for age or other factors.") +
  theme_minimal(base_size = 12)


# Save files --------------------------------------------------------------------

tables_dir <- here("output", "tables")
write_csv(flow,           file.path(tables_dir, "table1_cohort_flow.csv"))
write_csv(capture_tbl,    file.path(tables_dir, "table2_treatment_capture.csv"))
write_csv(mortality,      file.path(tables_dir, "table3_mortality.csv"))
write_csv(agreement,      file.path(tables_dir, "agreement_counts.csv"))
write_csv(gain,           file.path(tables_dir, "linked_minus_claims.csv"))
write_csv(patterns,       file.path(tables_dir, "treatment_patterns.csv"))
write_csv(fitness,        file.path(tables_dir, "data_fitness.csv"))
write_csv(extension,      file.path(tables_dir, "extension_missing_grade.csv"))
write_csv(step3,          file.path(tables_dir, "step3_exclusions.csv"))
write_csv(capture_deaths, file.path(tables_dir, "mortality_in_capture_cohort_for_comparison.csv"))

ggsave(here("output", "figures", "plotA_capture.png"),   plot_a, width = 7, height = 5)
ggsave(here("output", "figures", "plotB_patterns.png"),  plot_b, width = 8, height = 4.5)
ggsave(here("output", "figures", "plotX_extension.png"), plot_x, width = 8, height = 4.5)
