# Table S1: pre-stimulation characteristics by treatment group (Experiment 2).
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

subjects <- build_frames()$subjects
G <- length(unique(subjects$session))
vars <- c(risk_will_survey = "Willingness to take risks", patience_pre = "Patience",
          reciprocity_pre = "Reciprocity", math = "Self-assessed mathematical ability",
          education = "Education (postgraduate)", age = "Age", gender = "Female")
stopifnot(nrow(subjects) == 52, !anyNA(subjects[, names(vars)]))

msd <- function(x) sprintf("%.2f (%.2f)", mean(x), sd(x))
rows <- bind_rows(lapply(names(vars), function(v) {
  m <- lm(as.formula(paste(v, "~ stim")), data = subjects)
  b <- coef(m)[["stim"]]
  se <- sqrt(vcovCL(m, cluster = subjects$session, type = "HC1")["stim", "stim"])
  data.frame(a = vars[[v]], b = msd(subjects[[v]][subjects$stim == 0]),
             c = msd(subjects[[v]][subjects$stim == 1]),
             d = fmt_p(2 * pt(-abs(b / se), df = G - 1)))
}))
write_tabular(rows, file.path(OUT_TAB, "table_S1_balance.tex"), align = "lccr", size = "small",
              header_lines = "& Sham & Active stimulation & $P$ \\\\")
