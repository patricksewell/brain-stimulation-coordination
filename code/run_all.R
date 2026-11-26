# Reproduces every table, figure and statistic in the paper and SI Appendix.
# Run from the repository root:   Rscript code/run_all.R
# Outputs go to output/tables, output/figures and output/numbers.
# Each script runs in a fresh R session. The bootstrap tests with 999,999 draws
# (07 and 09) take several minutes each.

# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}

scripts <- c("01_balance.R", "02_treatment_effects.R", "03_robustness.R", "04_experiment1.R",
             "05_beliefs.R", "06_structural.R", "07_belief_updating.R", "08_dynamics.R",
             "09_risk_measures.R", "10_figures.R", "11_blinding.R")
rscript <- file.path(R.home("bin"), "Rscript")
for (s in scripts) {
  message("==== ", s)
  t0 <- Sys.time()
  status <- system2(rscript, file.path("code", s))
  if (status != 0) stop(s, " failed")
  message("     done in ", format(round(difftime(Sys.time(), t0, units = "mins"), 1)))
}
writeLines(capture.output(sessionInfo()), file.path("output", "sessionInfo.txt"))
