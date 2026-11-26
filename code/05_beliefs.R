# Beliefs and strategic reasoning (SI section 3.3):
#   Table S4, logistic belief-choice curves (slopes and 50% locations);
#   Table S5, first-round beliefs and Stag choice; the mixed-strategy benchmark.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

rounds <- build_frames()$rounds
G <- length(unique(rounds$session))

#### Table S4: belief-choice curves ####
# Each group has its own slope on the reported belief (shared across games) and
# its own intercept for each game. The 50% location is -intercept / slope.
d <- rounds %>% mutate(rd = as.integer(Game == "stag_risk_dom"))
f_belief <- PlayStag ~ 0 + I(1 - stim) + I((1 - stim) * belief01) + I((1 - stim) * rd) +
  stim + I(stim * belief01) + I(stim * rd)
m <- glm(f_belief, family = binomial, data = d)
V <- vcovCL(m, cluster = d$session, type = "HC1")
delta <- function(g_sham, g_stim) {
  se_of <- function(g) { gr <- numDeriv::grad(g, coef(m)); sqrt(drop(t(gr) %*% V %*% gr)) }
  g_diff <- function(b) g_stim(b) - g_sham(b)
  data.frame(sham = g_sham(coef(m)), se_sham = se_of(g_sham),
             stim = g_stim(coef(m)), se_stim = se_of(g_stim),
             diff = g_diff(coef(m)), p = 2 * pt(-abs(g_diff(coef(m)) / se_of(g_diff)), df = G - 1))
}
curves <- bind_rows(
  cbind(term = "slope", delta(function(b) b[2], function(b) b[5])),
  cbind(term = "loc_sh", delta(function(b) -b[1] / b[2], function(b) -b[4] / b[5])),
  cbind(term = "loc_rd", delta(function(b) -(b[1] + b[3]) / b[2], function(b) -(b[4] + b[6]) / b[5])))
curves$p_holm <- p.adjust(curves$p, "holm")

scale_of <- function(term) ifelse(term == "slope", 0.1, 100)   # slope per 10 belief points
dig_of <- function(term) ifelse(term == "slope", 2, 1)
lab <- c(slope = "Slope (per 10 belief points)", loc_sh = "Location, SH", loc_rd = "Location, SH RD")
rows <- bind_rows(lapply(seq_len(nrow(curves)), function(i) {
  x <- curves[i, ]; k <- scale_of(x$term); g <- dig_of(x$term)
  data.frame(a = lab[[x$term]],
             b = paste0(fmt_num(k * x$sham, g), " (", fmt_num(k * x$se_sham, g), ")"),
             c = paste0(fmt_num(k * x$stim, g), " (", fmt_num(k * x$se_stim, g), ")"),
             e = fmt_num(k * x$diff, g), f = fmt_p(x$p), h = fmt_p(x$p_holm))
}))
rows[] <- lapply(rows, function(v) gsub("(^|\\()-", "\\1$-$", v))
write_tabular(rows, file.path(OUT_TAB, "table_S4_belief_curves.tex"), align = "lrrrrr",
              size = "small", header_lines = "& Sham & Stimulated & Difference & $P$ & Holm $P$ \\\\")
where <- "Main text, Results (belief thresholds); SI Table S4"
for (i in seq_len(nrow(curves))) {
  x <- curves[i, ]; k <- scale_of(x$term); g <- dig_of(x$term)
  record(where, paste(lab[[x$term]], "- sham"), fmt_num(k * x$sham, g))
  record(where, paste(lab[[x$term]], "- active"), fmt_num(k * x$stim, g))
  record(where, paste(lab[[x$term]], "- P"), fmt_p(x$p))
  record(where, paste(lab[[x$term]], "- Holm P"), fmt_p(x$p_holm))
}

#### Table S5: first round of each game ####
r1 <- rounds %>% filter(round_num == 1) %>% mutate(Beliefs = as.numeric(Beliefs))
stopifnot(nrow(r1) == 104)
infer <- function(dd, f) {
  X <- model.matrix(f, dd); y <- model.response(model.frame(f, dd))
  j <- which(colnames(X) == "stim")
  ct <- cr1_t(X, y, dd$session, j)
  set.seed(20261014)
  data.frame(estimate = ct[["b"]],
             lo = ct[["b"]] - qt(0.975, G - 1) * ct[["se"]],
             hi = ct[["b"]] + qt(0.975, G - 1) * ct[["se"]],
             p = 2 * pt(-abs(ct[["t"]]), df = G - 1),
             p_wild = wild_cluster_boot(X, y, dd$session, j, B = 9999))
}
games <- c(SH = "stag", `SH RD` = "stag_risk_dom")
r1_res <- bind_rows(lapply(c("PlayStag", "Beliefs"), function(v)
  bind_rows(lapply(names(games), function(g) {
    dd <- r1[r1$Game == games[[g]], ]
    bind_rows(lapply(c(FALSE, TRUE), function(ctrl) {
      f <- as.formula(paste(v, "~ stim", if (ctrl) paste("+", ctrl_rhs) else ""))
      cbind(outcome = v, game = g, ctrl = ctrl,
            sham = mean(dd[[v]][dd$stim == 0]), stim = mean(dd[[v]][dd$stim == 1]), infer(dd, f))
    }))
  }))))
k_of <- function(v) if (v == "PlayStag") 100 else 1
fmt_rows <- function(v) {
  x <- r1_res[r1_res$outcome == v, ]; k <- k_of(v)
  data.frame(a = ifelse(x$ctrl, "\\quad\\quad + controls", paste0("\\quad ", x$game)),
             b = ifelse(x$ctrl, "", fmt_num(k * x$sham, 1)),
             c = ifelse(x$ctrl, "", fmt_num(k * x$stim, 1)),
             d = fmt_num(k * x$estimate, 1), e = fmt_ci(k * x$lo, k * x$hi, 1),
             g = fmt_p(x$p), h = fmt_p(x$p_wild))
}
blank <- function(label) data.frame(a = label, b = "", c = "", d = "", e = "", g = "", h = "")
tab <- bind_rows(blank("\\textit{Belief (0--100)}"), fmt_rows("Beliefs"),
                 blank("\\textit{Stag choice (\\%)}"), fmt_rows("PlayStag"))
tab[] <- lapply(tab, function(v) gsub("(^|\\[)-", "\\1$-$", v))
write_tabular(tab, file.path(OUT_TAB, "table_S5_round_one.tex"), align = "lrrrrrr", size = "small",
  header_lines = c("& & & & & \\multicolumn{2}{c}{$P$} \\\\", "\\cmidrule(lr){6-7}",
                   "Round 1 & Sham & Stimulated & Difference & 95\\% CI & Clustered & Bootstrap \\\\"))
where <- "SI section 3.3 (Beliefs and choices); SI Table S5"
for (i in which(!r1_res$ctrl)) {
  x <- r1_res[i, ]; k <- k_of(x$outcome)
  nm <- paste(ifelse(x$outcome == "PlayStag", "First-round Stag choice", "First-round belief"), x$game)
  record(where, paste(nm, "- difference"), fmt_num(k * x$estimate, 1))
  record(where, paste(nm, "- session-clustered P"), fmt_p(x$p))
}

#### Mixed-strategy benchmark ####
# Stag and Hare have equal expected payoffs when the partner chooses Stag with
# probability 25/35 (SH) or 27/56 (SH RD).
where <- "SI section 3.3 (Mixed-strategy benchmark)"
record(where, "Equal-EV belief, SH (%)", fmt_num(100 * 25 / 35, 1))
record(where, "Equal-EV belief, SH RD (%)", fmt_num(100 * 27 / 56, 1))
for (g in names(games)) for (a in 0:1)
  record(where, paste("Stag rate,", g, c("sham", "active")[a + 1], "(%)"),
         fmt_pp(mean(rounds$PlayStag[rounds$Game == games[[g]] & rounds$stim == a])))
write_numbers("05_beliefs")
