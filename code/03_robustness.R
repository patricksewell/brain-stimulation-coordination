# Robustness of the main estimates (SI section 3.2):
#   omitting one session or one pair at a time, adding a collection-wave
#   indicator, modeling serial correlation (AR(1)), and the treatment
#   difference over rounds.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

set.seed(20260928)
fr <- build_frames()
rounds <- fr$rounds; pairs <- fr$pairs
where <- "SI section 3.2 (Sensitivity of the main estimates); main text, Results"

#### Omitting one session or one pair at a time ####
loo_est <- function(d, keep, formula) coef(lm(formula, data = d[keep, ]))[["stim"]]
sessions <- sort(unique(rounds$session))
pair_ids <- unique(rounds$pair_id_unique)
loo <- list(
  stag_session = sapply(sessions, function(s) loo_est(rounds, rounds$session != s, f_ind)),
  ss_session   = sapply(sessions, function(s) loo_est(pairs, pairs$session != s, f_pair)),
  stag_pair    = sapply(pair_ids, function(p) loo_est(rounds, rounds$pair_id_unique != p, f_ind)),
  ss_pair      = sapply(pair_ids, function(p) loo_est(pairs, pairs$pair_id_unique != p, f_pair)))
record(where, "Stag choice, full sample (pp)", fmt_pp(coef(lm(f_ind, rounds))[["stim"]]))
record(where, "Stag choice, omitting one session: range (pp)",
       paste(fmt_pp(min(loo$stag_session)), "to", fmt_pp(max(loo$stag_session))))
record(where, "Stag choice, omitting one pair: range (pp)",
       paste(fmt_pp(min(loo$stag_pair)), "to", fmt_pp(max(loo$stag_pair))))
record(where, "Stag-Stag, full sample (pp)", fmt_pp(coef(lm(f_pair, pairs))[["stim"]]))
record(where, "Stag-Stag, omitting one session: range (pp)",
       paste(fmt_pp(min(loo$ss_session)), "to", fmt_pp(max(loo$ss_session))))
record(where, "Stag-Stag, omitting one pair: range (pp)",
       paste(fmt_pp(min(loo$ss_pair)), "to", fmt_pp(max(loo$ss_pair))))

#### Collection wave ####
# Wave 2 (January 2025) used the session configurations Z1, Z2, D1 and D2.
rounds$wave <- ifelse(sub("toledo_experiment_", "", rounds$session.config.name) %in%
                        c("Z1", "Z2", "D1", "D2"), "Wave 2", "Wave 1")
stopifnot(n_distinct(rounds$session[rounds$wave == "Wave 2"]) == 4)
wave_fit <- lm(update(f_ind, . ~ . + wave), data = rounds)
record(where, "Stag choice with a wave indicator (pp)", fmt_pp(coef(wave_fit)[["stim"]]))

#### Serial correlation: AR(1) errors within each participant and game ####
m_ar1 <- nlme::lme(f_ind, random = ~ 1 | participant.code/Game, data = rounds, method = "ML",
                   correlation = nlme::corAR1(form = ~ round_num | participant.code/Game))
record(where, "Stag choice with AR(1) errors (pp)",
       fmt_pp(summary(m_ar1)$tTable["stim", "Value"]))
record(where, "Estimated AR(1) autocorrelation",
       fmt_num(coef(m_ar1$modelStruct$corStruct, unconstrained = FALSE), 2))

#### Treatment difference over rounds ####
d <- rounds %>%
  mutate(T = stim, Gm = as.integer(Game == "stag_risk_dom"),
         rc = round_num - 5.5,
         q  = (as.numeric(as.character(game_order)) - 1) * 10 + round_num,   # order played, 1-20
         qc = q - 10.5,
         Rf = factor(round_num), Qf = factor(q),
         TG = T * Gm, Trc = T * rc, TrcG = T * rc * Gm, Tqc = T * qc) %>%
  as.data.frame()
stopifnot(all(range(d$q) == c(1, 20)))
trend_test <- function(label, f, data, term) {
  X <- model.matrix(f, data)
  j <- which(colnames(X) == term)
  ct <- cr1_t(X, data$PlayStag, data$session, j)
  G <- length(unique(data$session))
  set.seed(20261120)
  p_wild <- wild_cluster_boot_fast(X, data$PlayStag, data$session, j, B = 99999)
  data.frame(test = label, estimate = ct[["b"]], se = ct[["se"]],
             lo = ct[["b"]] - qt(0.975, G - 1) * ct[["se"]],
             hi = ct[["b"]] + qt(0.975, G - 1) * ct[["se"]],
             p = 2 * pt(-abs(ct[["t"]]), df = G - 1), p_wild = p_wild)
}
trends <- bind_rows(
  trend_test("Within-game rounds, both games", PlayStag ~ Rf + Gm + T + TG + Trc, d, "Trc"),
  trend_test("Within-game rounds, SH", PlayStag ~ Rf + T + Trc, d[d$Gm == 0, ], "Trc"),
  trend_test("Within-game rounds, SH RD", PlayStag ~ Rf + T + Trc, d[d$Gm == 1, ], "Trc"),
  trend_test("SH RD minus SH", PlayStag ~ Rf * Gm + T + TG + Trc + TrcG, d, "TrcG"),
  trend_test("Decisions 1-20 in the order played", PlayStag ~ Qf + Gm + T + TG + Tqc, d, "Tqc"))
where <- "SI section 3.2 (Treatment difference over rounds)"
for (i in seq_len(nrow(trends))) {
  record(where, paste(trends$test[i], "- change per round (pp)"), fmt_pp(trends$estimate[i], 2))
  record(where, paste(trends$test[i], "- 95% CI (pp)"),
         fmt_ci(100 * trends$lo[i], 100 * trends$hi[i], 2))
  record(where, paste(trends$test[i], "- session-clustered P"), fmt_p(trends$p[i]))
  record(where, paste(trends$test[i], "- wild bootstrap P"), fmt_p(trends$p_wild[i]))
}
write_numbers("03_robustness")
