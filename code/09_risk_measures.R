# Risk and other elicited measures (main text; SI section 3.6):
#   lottery scores relative to baseline risk attitudes, risk types, the five
#   post-stimulation measures with Holm's correction, correlations between the
#   risk measures and Stag choice, and the attenuation analysis.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

fr <- build_frames()
subjects <- fr$subjects
G <- length(unique(subjects$session))

#### Lottery score relative to baseline risk attitudes ####
# Calibrate lottery scores on baseline willingness to take risks in the sham
# group, then compare observed minus predicted scores. The difference and its
# standard error come from one regression with the interaction, so the SE
# accounts for the uncertainty in the sham calibration.
cal <- lm(risk_classification ~ risk_will_survey, data = subset(subjects, stim == 0))
subjects$resid <- subjects$risk_classification - predict(cal, subjects)
m_int <- lm(risk_classification ~ risk_will_survey * stim, data = subjects)
g <- setNames(rep(0, length(coef(m_int))), names(coef(m_int)))
g[["stim"]] <- 1
g[["risk_will_survey:stim"]] <- mean(subjects$risk_will_survey[subjects$stim == 1])
resid_est <- sum(g * coef(m_int))
stopifnot(abs(resid_est - mean(subjects$resid[subjects$stim == 1])) < 1e-8)
resid_se <- sqrt(drop(t(g) %*% vcovCL(m_int, cluster = subjects$session, type = "HC1") %*% g))
where <- "Main text, Risk-taking (relative to baseline); SI section 3.6"
record(where, "Residualized lottery score, difference", fmt_num(resid_est))
record(where, "Residualized lottery score, SE", fmt_num(resid_se))
record(where, "Residualized lottery score, 95% CI",
       fmt_ci(resid_est - qt(0.975, G - 1) * resid_se, resid_est + qt(0.975, G - 1) * resid_se))
record(where, "Residualized lottery score, P",
       fmt_p(2 * pt(-abs(resid_est / resid_se), df = G - 1)))

anc <- lm(risk_classification ~ stim + risk_will_survey, data = subjects)
anc_se <- sqrt(vcovCL(anc, cluster = subjects$session, type = "HC1")["stim", "stim"])
anc_b <- coef(anc)[["stim"]]
record(where, "ANCOVA, difference", fmt_num(anc_b))
record(where, "ANCOVA, SE", fmt_num(anc_se))
record(where, "ANCOVA, 95% CI", fmt_ci(anc_b - qt(0.975, G - 1) * anc_se, anc_b + qt(0.975, G - 1) * anc_se))
record(where, "ANCOVA, P", fmt_p(2 * pt(-abs(anc_b / anc_se), df = G - 1)))

# Risk types from the certainty equivalent (risk neutral: 145-155 euros)
types <- subjects %>%
  mutate(type = case_when(certainty_equiv < 145 ~ "risk averse", certainty_equiv <= 155 ~ "near risk neutral",
                          TRUE ~ "risk seeking")) %>%
  count(stim, type) %>% group_by(stim) %>% mutate(share = n / sum(n)) %>% ungroup()
for (i in seq_len(nrow(types)))
  record("Main text, Risk-taking (risk types); Fig. 4B",
         paste("Share", types$type[i], c("(sham, %)", "(active, %)")[types$stim[i] + 1]),
         fmt_num(100 * types$share[i], 1))

#### Five post-stimulation measures, Holm's correction ####
elicited <- c(Lottery = "risk_classification", BRET = "BRET_Avg_Boxes", CRT = "crt",
              Patience = "patience_post", Reciprocity = "reciprocity_post")
measures <- bind_rows(lapply(names(elicited), function(nm) {
  m <- lm(as.formula(paste(elicited[[nm]], "~ stim")), data = subjects)
  X <- model.matrix(m)
  set.seed(20261023)
  data.frame(measure = nm, estimate = coef(m)[["stim"]],
             p_wild = wild_cluster_boot_fast(X, model.response(model.frame(m)), subjects$session,
                                             which(colnames(X) == "stim"), B = 99999))
})) %>% mutate(p_holm = p.adjust(p_wild, "holm"))
where <- "Main text, Risk-taking; SI section 3.6 (Risk measures, Other elicited measures)"
for (i in seq_len(nrow(measures))) {
  record(where, paste(measures$measure[i], "- difference"), fmt_num(measures$estimate[i]))
  record(where, paste(measures$measure[i], "- wild bootstrap P"), fmt_p(measures$p_wild[i]))
  record(where, paste(measures$measure[i], "- Holm P (five measures)"), fmt_p(measures$p_holm[i]))
}

#### Risk measures and Stag choice ####
# Correlation between each participant's share of Stag choices (20 rounds) and
# each risk measure, standardizing both within treatment groups.
rates <- fr$rounds %>% group_by(participant.code) %>% summarise(stag = mean(PlayStag), .groups = "drop")
subj <- subjects %>% inner_join(rates, by = "participant.code") %>% as.data.frame()
link <- function(d, x) {
  d <- d %>% group_by(stim) %>%
    mutate(zx = as.numeric(scale(.data[[x]])), zy = as.numeric(scale(stag))) %>%
    ungroup() %>% as.data.frame()
  f <- if (length(unique(d$stim)) > 1) zy ~ zx + stim else zy ~ zx
  m <- lm(f, data = d)
  se <- sqrt(vcovCL(m, cluster = d$session, type = "HC1")["zx", "zx"])
  c(r = coef(m)[["zx"]], p = 2 * pt(-abs(coef(m)[["zx"]] / se), df = length(unique(d$session)) - 1))
}
risk <- c(risk_classification = "Lottery", BRET_Avg_Boxes = "BRET", risk_will_survey = "Baseline risk willingness")
cors <- bind_rows(lapply(names(risk), function(x) {
  s <- link(subj[subj$stim == 0, ], x); a <- link(subj[subj$stim == 1, ], x); b <- link(subj, x)
  data.frame(measure = risk[[x]], r_sham = s[["r"]], p_sham = s[["p"]], r_stim = a[["r"]],
             p_stim = a[["p"]], r_both = b[["r"]], p_both = b[["p"]])
}))
where <- "SI section 3.6 (Risk measures and Stag choice)"
for (i in seq_len(nrow(cors))) {
  record(where, paste(cors$measure[i], "- correlation, both groups (average within group)"), fmt_num(cors$r_both[i]))
  record(where, paste(cors$measure[i], "- correlation, sham / active"),
         paste(fmt_num(cors$r_sham[i]), "/", fmt_num(cors$r_stim[i])))
}
record(where, "Smallest Holm-adjusted P across the nine correlations",
       fmt_p(min(p.adjust(c(cors$p_sham, cors$p_stim, cors$p_both), "holm"))))

#### Attenuation: adding the risk measures to the Stag-choice regression ####
d <- fr$rounds %>%
  left_join(subjects %>% transmute(participant.code,
                                   lottery = as.numeric(scale(risk_classification)),
                                   bret = as.numeric(scale(BRET_Avg_Boxes))),
            by = "participant.code")
stopifnot(nrow(d) == 1040, !anyNA(d[, c("lottery", "bret")]))
mediators <- list(none = character(0), lottery = "lottery", bret = "bret", both = c("lottery", "bret"))
rhs <- function(m) paste(c("stim", m, "Round", controls), collapse = " + ")
X_of <- lapply(mediators, function(m) model.matrix(as.formula(paste("~", rhs(m))), data = d))
y <- d$PlayStag
stim_coef <- function(X, rows) lm.fit(X[rows, , drop = FALSE], y[rows])$coefficients[["stim"]]

# Reduction in the stimulation coefficient, with 95% intervals from resampling
# sessions within each treatment group.
set.seed(20261022)
rows_by_session <- split(seq_len(nrow(d)), d$session)
arm_sessions <- split(names(rows_by_session), tapply(d$stim, d$session, first)[names(rows_by_session)])
boot <- replicate(9999, {
  s <- unlist(lapply(arm_sessions, function(x) sample(x, length(x), replace = TRUE)))
  rows <- unlist(rows_by_session[s], use.names = FALSE)
  c0 <- stim_coef(X_of$none, rows)
  sapply(X_of[-1], function(X) c0 - stim_coef(X, rows))
})
full <- sapply(X_of, function(X) stim_coef(X, seq_len(nrow(d))))
where <- "SI section 3.6 (Risk measures and Stag choice)"
for (m in c("lottery", "bret", "both")) {
  record(where, paste("Stimulation coefficient with", m, "(pp)"), fmt_num(100 * full[[m]], 1))
  ci <- quantile(boot[m, ], c(0.025, 0.975))
  record(where, paste("Reduction in the stimulation coefficient,", m, "(pp)"), fmt_num(100 * (full[["none"]] - full[[m]]), 1))
  record(where, paste("95% interval for the reduction,", m), fmt_ci(100 * ci[1], 100 * ci[2], 1))
}
# BRET coefficient per SD (wild bootstrap, 999,999 draws)
for (m in c("bret", "both")) {
  X <- X_of[[m]]
  b <- lm.fit(X, y)$coefficients[["bret"]]
  set.seed(20261022)
  p <- wild_cluster_boot_fast(X, y, d$session, which(colnames(X) == "bret"), B = 999999)
  record(where, paste("BRET coefficient per SD (pp), model with", m), fmt_num(100 * b, 1))
  record(where, paste("BRET coefficient, wild bootstrap P, model with", m), fmt_p(p))
}
write_numbers("09_risk_measures")
