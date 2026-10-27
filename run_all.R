# One command reproduces every result from the unmodified CSVs in data/:
#   Rscript run_all.R
# Each step sources the step before it, so sourcing the last step runs
# 00-setup -> 01-prepare -> 02-analysis -> 03-tests exactly once, in order.

source(here::here("R", "03-tests.R"))

writeLines(c(paste("Run at:", format(Sys.time())), capture.output(sessionInfo())),
           here::here("output", "logs", "sessionInfo.txt"))
rmarkdown::render(here::here("report.Rmd"), output_dir = here::here("output"), quiet = TRUE)
cat("\nDone: see output/report.html\n")