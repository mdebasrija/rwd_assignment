# Claims + EHR linkage: treatment capture and 365-day mortality

## Run

1. Open `rwd_assignment.Rproj` in RStudio (or start R in this folder). renv starts
   automatically via `.Rprofile`.
2. Install the exact package versions: `renv::restore()`
3. Run everything: `source("run_all.R")` in RStudio, or `Rscript run_all.R` from a
   terminal in this folder.

One command reproduces every result from the unmodified files in `data/`. Results are
written to `output/`; the report is `output/report.html`.

Requirements: R >= 4.1 (built with R 4.6.0) with `here`, `readr`, `dplyr`, `purrr`,
`ggplot2`, `digest`, `rmarkdown`, `knitr`. Exact versions are pinned in `renv.lock`;
each run also writes `output/logs/sessionInfo.txt`. No random numbers are used, so
no seed is needed. Also requires `pandoc` >= 2.8 (comes bundled with RStudio but needs to be installed if opening on CLI).


**Data disclaimer.** The files in `data/` are **synthetic**. They were generated for a 
take-home assessment and describe no real people. They are included only so the 
pipeline can be run end to end. Real patient-level claims or EHR data must never be 
committed to a repository, public or private: in real work the data stay in an 
approved, access-controlled environment, and only code and aggregate, de-identified 
outputs are shared. The assignment brief (`Assignment_and_Data_Guide.pdf`) is 
included for context and belongs to its authors.

## Structure

```
run_all.R            sources 03-tests.R (which runs the whole chain), then renders the report
R/00-setup.R         settings (cutoff, window, look-back, age), one helper: invalid_date()
R/01-prepare.R       sources 00; verify checksums, read as text, convert types, data checks, issue log
R/02-analysis.R      sources 01; cohorts, treatment capture, agreement, patterns, mortality,
                     data fitness, extension, figures; saves everything to output/
R/03-tests.R         sources 02; automated checks on the real data; stops on failure
report.Rmd           the report; reads results from output/ (only re-verifies checksums)
plan_analysis.txt    dated plan + change log
data/                supplied synthetic CSVs, never modified (checked against CHECKSUMS.sha256)
output/
  tables/            result tables (CSV)
  figures/           Plot A, Plot B, extension plot (PNG)
  logs/              data_issues.csv, data_checks_summary.csv, automated_checks.txt, sessionInfo.txt
  report.html
```

Each script sources the one before it and loads only the packages it uses, so any
script runs on its own (e.g. open `02-analysis.R` and click Source). `run_all.R`
sources only the last script, which runs the whole chain once, in order.

## Key decisions

| Decision | Choice | Why |
|---|---|---|
| Reading | All columns as text, `na = ""`, spaces kept | Convert deliberately; detect problems |
| Effective end | min(recorded end, death, 2023-12-31) per source | No observation after death or cutoff |
| Window | Days 0-365 inclusive, elapsed days | As specified in the brief |
| Duplicate event rows | Kept; each patient flagged once per source | No event ID; repeats are expected |
| Grade group = 7 | Set to missing, logged | Valid values are 1-5 |
| Confidence intervals | Wilson: `prop.test(x, n, correct = FALSE)` | Good coverage for proportions and small counts; no continuity correction |
| Gain from linkage CI | Wilson CI of the EHR-only share | Linked minus claims equals that single proportion |
| Mortality cohort | Baseline | The capture cohort excludes early deaths by design |
| Extension | Is missing grade random? Missing by age; treatment by grade | Shows what grade missingness means, not just how much |

## Time spent and unfinished work

- Time spent: about 4 hours excluding breaks, including setup and writing.
- Unfinished: robustness checks for the extension. Next I would run sensitivity
  analyses (each source alone, Part D patients only, age 80 or under vs 81+, a
  shorter window such as 180 days) and adjust the grade comparison for age.
- More hand-built tests that break specifically on edge cases such as a death on day 365/leap years. 

## AI usage disclosure

- **Claude**: co-wrote code with me; I reviewed, ran and checked every step it
  generated. It also helped draft the report text, which I verified and edited, and
  it reviewed the finished project against the brief, catching run-breaking bugs
  (a wrong filename, a missing object, invalid YAML in the report header).
- My own work: cohort and window rules, data checks and issue log, choice of Wilson
  intervals, the extension question, and the final interpretation and conclusions.