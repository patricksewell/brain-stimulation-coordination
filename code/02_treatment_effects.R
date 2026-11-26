# Main treatment effects in Experiment 2 and Table S3.
#   Stag choice and Stag-Stag coordination, without and with pre-stimulation
#   controls (OLS with round fixed effects, session-clustered CR1 standard
#   errors, t(12) and wild cluster bootstrap P values), and the lottery score.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

set.seed(20260916)
fr <- build_frames()
rounds <- fr$rounds; pairs <- fr$pairs; subjects <- fr$subjects

# The five tests run in this order with this seed reproduce the published
# bootstrap P values exactly.
specs <- bind_rows(
  session_test("Stag choice", PlayStag ~ stim + Round, rounds, "stim"),
  session_test("Stag choice + controls", f_ind, rounds, "stim"),
  session_test("Stag--Stag", stag_stag ~ stim + Round, pairs, "stim"),
  session_test("Stag--Stag + controls", f_pair, pairs, "stim"),
  session_test("Lottery score", risk_classification ~ stim, subjects, "stim")
)
write.csv(specs, file.path(OUT_NUM, "treatment_effects_detail.csv"), row.names = FALSE)

#### Table S3: the four specifications with all coefficients ####
reg_fits <- list(lm(PlayStag ~ stim + Round, rounds), lm(f_ind, rounds),
                 lm(stag_stag ~ stim + Round, pairs), lm(f_pair, pairs))
reg_cl <- list(rounds$session, rounds$session, pairs$session, pairs$session)
reg_V <- lapply(1:4, function(k) vcovCL(reg_fits[[k]], cluster = reg_cl[[k]], type = "HC1"))
reg_terms <- c(stim = "Stimulated", risk_will_survey = "Baseline risk", education = "Education",
               math = "Math", reciprocity_pre = "Baseline reciprocity",
               patience_pre = "Baseline patience", `(Intercept)` = "Constant")
signed <- function(x) sub("^-", "$-$", sprintf("%.3f", x))
reg_rows <- do.call(rbind, lapply(names(reg_terms), function(tm) {
  est <- sapply(reg_fits, function(m) if (tm %in% names(coef(m))) signed(coef(m)[[tm]]) else "")
  se <- sapply(1:4, function(k) if (tm %in% names(coef(reg_fits[[k]])))
    sprintf("(%.3f)", sqrt(reg_V[[k]][tm, tm])) else "")
  rbind(c(reg_terms[[tm]], est), c("", se))
}))
foot <- rbind(c("Round fixed effects", rep("Yes", 4)),
              c("Observations", sapply(reg_fits, function(m) format(nobs(m), big.mark = ","))),
              c("Sessions", rep("13", 4)),
              c("Session-clustered $P$, stimulation", fmt_p(specs$p_session[1:4])),
              c("Wild bootstrap $P$, stimulation", fmt_p(specs$p_wild[1:4])))
writeLines(c("\\small", "\\begin{tabular}{lrrrr}", "\\toprule",
             " & \\multicolumn{2}{c}{Stag choice} & \\multicolumn{2}{c}{Stag--Stag} \\\\",
             "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
             " & (1) & (2) & (3) & (4) \\\\", "\\midrule",
             apply(reg_rows, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
             "\\midrule",
             apply(foot, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
             "\\bottomrule", "\\end{tabular}"),
           file.path(OUT_TAB, "table_S3_treatment_effects.tex"))

#### Numbers in the text ####
where <- "Main text, Results; SI Table S3"
for (i in 1:4) {
  s <- specs[i, ]
  record(where, paste(s$model, "- estimate (pp)"), fmt_pp(s$estimate))
  record(where, paste(s$model, "- SE (pp)"), fmt_pp(s$se))
  record(where, paste(s$model, "- session-clustered P"), fmt_p(s$p_session))
  record(where, paste(s$model, "- wild bootstrap P"), fmt_p(s$p_wild))
}
lot <- specs[5, ]
where <- "Main text, Risk-taking (direct comparison)"
record(where, "Lottery score, sham mean", fmt_num(mean(subjects$risk_classification[subjects$stim == 0])))
record(where, "Lottery score, active mean", fmt_num(mean(subjects$risk_classification[subjects$stim == 1])))
record(where, "Lottery score, difference", fmt_num(lot$estimate))
record(where, "Lottery score, SE", fmt_num(lot$se))
record(where, "Lottery score, session-clustered P", fmt_p(lot$p_session))
write_numbers("02_treatment_effects")
