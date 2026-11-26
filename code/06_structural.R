# Table S6: random parameter model (Apesteguia and Ballester 2018) with CRRA
# utility, estimated separately for each group on the Stag Hunt choices and on
# the lottery choices (SI section 3.4).
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

OMEGA <- 1e-8   # euros added to every payoff, to accommodate zero outcomes
EUR   <- 0.02   # euros per point
G_ALL <- 13     # sessions
fr <- build_frames()

# Stag Hunt: choosing Stag is a lottery paying x_win with the reported
# probability p and zero otherwise; Hare pays x_hare for sure.
dat <- fr$rounds %>%
  transmute(session, stim, p = as.numeric(Beliefs) / 100, y = PlayStag,
            x_win = EUR * ifelse(Game == "stag", 35, 56),
            x_hare = EUR * ifelse(Game == "stag", 25, 27))
stopifnot(nrow(dat) == 1040, !anyNA(dat))

# Lottery task: five choices between a 50:50 lottery (300 or 0) and a sure amount
# that starts at 160 and moves by 80, 40, 20 and 10. The certainty equivalent
# (score x 10 - 5) determines the whole sequence of choices, which we rebuild:
# at each step the lottery (A) is chosen if the certainty equivalent exceeds the
# sure amount.
lot <- bind_rows(lapply(seq_len(nrow(fr$subjects)), function(i) {
  ce <- fr$subjects$certainty_equiv[i]; sure <- 160; out <- vector("list", 5)
  for (k in 1:5) {
    y <- as.integer(ce > sure)
    out[[k]] <- data.frame(participant.code = fr$subjects$participant.code[i], step = k,
                           sure = sure, y = y)
    sure <- sure + ifelse(y == 1, 1, -1) * 80 / 2^(k - 1)
  }
  bind_rows(out)
})) %>%
  left_join(select(fr$subjects, participant.code, session, stim, certainty_equiv),
            by = "participant.code")
stopifnot(nrow(lot) == 260,
          with(filter(lot, step == 5), all(certainty_equiv == sure + ifelse(y == 1, 5, -5))))
lot <- lot %>% mutate(p = 0.5, x_win = 300, x_hare = sure)

#### CRRA utility, computed stably relative to the perturbation ####
log_u_rel <- function(x, r, omega) {
  L <- log1p(x / omega); a <- 1 - r
  if (abs(a) < 1e-8) log(L)
  else if (a > 0) a * L + log(-expm1(-a * L)) - log(a)
  else log(-expm1(a * L)) - log(-a)
}
# Risk coefficient at which the risky and the sure option have equal expected utility
K_threshold <- function(p, x_win, x_hare, omega, lo = -30, hi = 30) {
  f <- function(r) log(p) + log_u_rel(x_win, r, omega) - log_u_rel(x_hare, r, omega)
  if (f(lo) <= 0) return(-Inf)
  if (f(hi) >= 0) return(Inf)
  uniroot(f, c(lo, hi), tol = 1e-10)$root
}
add_cells <- function(d) {
  cells <- d %>% distinct(p, x_win, x_hare) %>% mutate(cell = row_number())
  list(d = left_join(d, cells, by = c("p", "x_win", "x_hare")), cells = cells)
}
game <- add_cells(dat); lottery <- add_cells(lot)

# P(risky) = kappa + (1 - 2 kappa) Lambda((K - theta) / sigma)
tremble <- function(P, eps) pmin(pmax(eps + (1 - 2 * eps) * P, 1e-12), 1 - 1e-12)
rpm <- function(cells) {
  K <- with(cells, mapply(K_threshold, p, x_win, x_hare, OMEGA))
  list(par = c("mu_r", "log_sigma_r", "logit_eps"), start = c(0, 0, -4),
       prob = function(th) tremble(plogis((K - th[1]) / exp(th[2])), plogis(th[3])))
}
models <- list(rpm_crra = c(rpm(game$cells), list(d = game$d)),
               rpm_lottery = c(rpm(lottery$cells), list(d = lottery$d)))
loglik_obs <- function(th, m, d) {
  P <- pmin(pmax(m$prob(th)[d$cell], 1e-12), 1 - 1e-12)
  ifelse(d$y == 1, log(P), log(1 - P))
}
bounds <- list(mu_r = c(-10, 10), log_sigma_r = c(-4, 3), logit_eps = c(-9, 0))

#### Maximum likelihood with session-clustered sandwich standard errors ####
optimize_arm <- function(m, d) {
  nll <- function(th) -sum(loglik_obs(th, m, d))
  lo <- sapply(m$par, function(x) bounds[[x]][1]); hi <- sapply(m$par, function(x) bounds[[x]][2])
  starts <- list(m$start, m$start + 0.5, m$start - 0.5)
  fits <- lapply(starts, function(s) tryCatch(
    optim(pmin(pmax(s, lo + 1e-3), hi - 1e-3), nll, method = "L-BFGS-B", lower = lo, upper = hi,
          control = list(maxit = 3000, factr = 1e5)), error = function(e) NULL))
  c(fits[[which.min(sapply(fits, function(f) if (is.null(f)) Inf else f$value))]],
    list(nll = nll, lo = lo, hi = hi))
}
fit_arm <- function(m, d) {
  fit <- optimize_arm(m, d)
  at_bound <- abs(fit$par - fit$lo) < 1e-3 | abs(fit$par - fit$hi) < 1e-3
  free <- which(!at_bound)
  V <- matrix(NA_real_, length(fit$par), length(fit$par))
  f_free <- function(tf) { th <- fit$par; th[free] <- tf; th }
  H <- numDeriv::hessian(function(tf) fit$nll(f_free(tf)), fit$par[free])
  meat <- Reduce(`+`, lapply(unique(d$session), function(s) {
    ds <- d[d$session == s, ]
    tcrossprod(numDeriv::grad(function(tf) sum(loglik_obs(f_free(tf), m, ds)), fit$par[free]))
  }))
  V[free, free] <- solve(H) %*% meat %*% solve(H)
  list(par = setNames(fit$par, m$par), V = V, ll = -fit$value, at_bound = at_bound,
       n = nrow(d), k = length(free))
}
fits <- lapply(models, function(m)
  lapply(c(sham = 0, stim = 1), function(a) fit_arm(m, m$d[m$d$stim == a, ])))

# Wald tests of the group differences (t(12)); the tremble is compared on the
# logit(2 kappa) scale.
compare_arms <- function(nm, sh, st) {
  N <- sh$n + st$n; K <- sh$k + st$k
  cr1 <- G_ALL / (G_ALL - 1) * (N - 1) / (N - K)
  to_test <- function(par) ifelse(names(par) == "logit_eps", qlogis(2 * plogis(par)), par)
  slope <- function(par) ifelse(names(par) == "logit_eps", (1 - plogis(par)) / (1 - 2 * plogis(par)), 1)
  diff <- to_test(st$par) - to_test(sh$par)
  se_d <- sqrt(cr1 * (slope(sh$par)^2 * diag(sh$V) + slope(st$par)^2 * diag(st$V)))
  data.frame(model = nm, term = names(sh$par), sham = sh$par, bound_sham = sh$at_bound,
             stim = st$par, bound_stim = st$at_bound, diff = diff,
             p = 2 * pt(-abs(diff / se_d), df = G_ALL - 1),
             ll = sh$ll + st$ll, n_obs = N, row.names = NULL)
}
res <- bind_rows(lapply(names(fits), function(nm) compare_arms(nm, fits[[nm]]$sham, fits[[nm]]$stim)))

# When the sham tremble sits at its bound of zero (lottery), test the difference
# with a session-clustered score test from the model with a common tremble.
score_test_tremble <- function(m, start) {
  d <- m$d
  ll <- function(th, dd) sum(sapply(0:1, function(a) {
    da <- dd[dd$stim == a, ]
    if (nrow(da) == 0) return(0)
    sum(loglik_obs(c(th[1 + 2 * a], th[2 + 2 * a], th[5] + th[6] * a), m, da))
  }))
  lo <- c(bounds$mu_r[1], bounds$log_sigma_r[1], bounds$mu_r[1], bounds$log_sigma_r[1], bounds$logit_eps[1])
  hi <- c(bounds$mu_r[2], bounds$log_sigma_r[2], bounds$mu_r[2], bounds$log_sigma_r[2], bounds$logit_eps[2])
  r <- optim(start, function(t) -ll(c(t, 0), d), method = "L-BFGS-B", lower = lo, upper = hi)
  th0 <- c(r$par, 0)
  A <- -numDeriv::hessian(function(t) ll(t, d), th0)
  sc <- t(sapply(unique(d$session), function(s) numDeriv::grad(function(t) ll(t, d[d$session == s, ]), th0)))
  B <- G_ALL / (G_ALL - 1) * (nrow(d) - 1) / (nrow(d) - length(th0)) * crossprod(sc)
  Ai <- solve(A)
  t_score <- (Ai %*% colSums(sc))[6] / sqrt((Ai %*% B %*% Ai)[6, 6])
  2 * pt(-abs(t_score), df = G_ALL - 1)
}
res$p_score <- FALSE
for (i in which(is.na(res$p) & res$term == "logit_eps")) {
  nm <- res$model[i]; f <- fits[[nm]]
  res$p[i] <- score_test_tremble(models[[nm]], c(f$sham$par[1:2], f$stim$par[1:2], -3))
  res$p_score[i] <- TRUE
}

#### Table S6 ####
labels <- c(mu_r = "Location $\\theta$ (CRRA $r$)", log_sigma_r = "Dispersion $\\sigma$",
            logit_eps = "Tremble $\\kappa$ (\\%)")
natural <- function(term, x) switch(term, log_sigma_r = exp(x), logit_eps = 100 * plogis(x), x)
model_label <- c(rpm_crra = "Stag Hunt choices", rpm_lottery = "Lottery choices")
fmt_cell <- function(term, x, bound) {
  v <- fmt_num(natural(term, x), ifelse(term == "logit_eps", 1, 2))
  ifelse(bound, paste0(v, "$^\\dagger$"), v)
}
rows <- bind_rows(lapply(names(model_label), function(m) {
  xm <- res[res$model == m, ]
  bind_rows(
    data.frame(a = paste0("\\textit{", model_label[[m]], "}"), b = "", c = "", d = ""),
    data.frame(a = paste0("\\quad ", labels[xm$term]),
               b = mapply(fmt_cell, xm$term, xm$sham, xm$bound_sham),
               c = mapply(fmt_cell, xm$term, xm$stim, xm$bound_stim),
               d = paste0(ifelse(is.na(xm$p), "--", fmt_p(xm$p)), ifelse(xm$p_score, "$^\\ddagger$", ""))),
    data.frame(a = "\\quad Log-likelihood", b = "", c = "", d = fmt_num(xm$ll[1], 1)),
    data.frame(a = "\\quad Choices", b = "", c = "", d = format(xm$n_obs[1], big.mark = ",")))
}))
rows[] <- lapply(rows, function(v) gsub("^-", "$-$", v))
write_tabular(rows, file.path(OUT_TAB, "table_S6_structural.tex"), align = "lrrr", size = "small",
              header_lines = "& Sham & Stimulated & $P$ \\\\")

where <- "Main text, Results (structural model); SI Table S6"
for (i in seq_len(nrow(res))) {
  nm <- paste(model_label[[res$model[i]]], "-", gsub("[$\\\\]", "", labels[[res$term[i]]]))
  dig <- ifelse(res$term[i] == "logit_eps", 1, 2)
  record(where, paste(nm, "sham"), fmt_num(natural(res$term[i], res$sham[i]), dig))
  record(where, paste(nm, "active"), fmt_num(natural(res$term[i], res$stim[i]), dig))
  record(where, paste(nm, "P"), fmt_p(res$p[i]))
}
write_numbers("06_structural")
