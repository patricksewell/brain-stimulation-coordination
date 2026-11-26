# Participant blinding (SI section 2.3, Treatment Assignment and Blinding):
#   the end-of-session guess about the condition received, by actual condition,
#   and Pearson's chi-squared test among participants who gave a definite guess.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

b <- read.csv("data/blinding_check.csv")
stopifnot(nrow(b) == 112, all(b$guess %in% c("active", "sham", "unsure")))
b$condition <- ifelse(b$stim == 1, "active", "sham")
b$guess <- factor(b$guess, levels = c("active", "sham", "unsure"))

# Actual condition by guess, for each experiment and pooled
crosstab <- function(d, label) {
  t <- table(d$condition, d$guess)
  data.frame(sample = label, condition = rownames(t), guessed_active = t[, "active"],
             guessed_sham = t[, "sham"], unsure = t[, "unsure"], total = rowSums(t), row.names = NULL)
}
tab <- bind_rows(crosstab(b[b$experiment == 1, ], "Experiment 1"),
                 crosstab(b[b$experiment == 2, ], "Experiment 2"),
                 crosstab(b, "Both experiments"))
write.csv(tab, file.path(OUT_TAB, "blinding_check.csv"), row.names = FALSE)
message("wrote ", file.path(OUT_TAB, "blinding_check.csv"))

where <- "SI section 2.3 (Participant blinding)"
unsure <- b$guess == "unsure"
record(where, "Unsure, both experiments", sprintf("%d (%s%%)", sum(unsure), fmt_num(100 * mean(unsure), 1)))
record(where, "Unsure, active / sham", paste(sum(unsure & b$stim == 1), "/", sum(unsure & b$stim == 0)))
definite <- droplevels(b[!unsure, ])
record(where, "Definite guesses", nrow(definite))
record(where, "Correct guesses among definite (%)",
       fmt_num(100 * mean(as.character(definite$guess) == definite$condition), 0))
# Pearson's test without continuity correction
test <- chisq.test(table(definite$condition, definite$guess), correct = FALSE)
record(where, "Pearson chi-squared (1 df)", fmt_num(unname(test$statistic), 2))
record(where, "Pearson chi-squared, P", fmt_p(test$p.value))
write_numbers("11_blinding")
