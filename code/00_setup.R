# Shared setup for the replication code: packages, paths, data loading, and
# the inference and formatting helpers used by every script.
#
# Run the scripts from the repository root, e.g.  Rscript code/run_all.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(sandwich)
  library(lme4)
})

if (!dir.exists("data/experiment2")) {
  stop("Run the scripts from the repository root (the folder that contains data/ and code/).")
}

OUT       <- "output"
OUT_TAB   <- file.path(OUT, "tables")
OUT_FIG   <- file.path(OUT, "figures")
OUT_NUM   <- file.path(OUT, "numbers")
for (d in c(OUT_TAB, OUT_FIG, OUT_NUM)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a

#### Experiment 2 data ####
# Pre-stimulation controls used throughout. For pair-level outcomes they are the
# average of the two partners' values.
controls <- c("risk_will_survey", "education", "math", "reciprocity_pre", "patience_pre")
ctrl_rhs <- paste(controls, collapse = " + ")
f_ind    <- as.formula(paste("PlayStag ~ stim + Round +", ctrl_rhs))
f_pair   <- as.formula(paste("stag_stag ~ stim + Round +", ctrl_rhs))

# Participants in the order of their original (pre-anonymization) identifiers.
# Several analyses sort by participant, and the sort order sets the order of the
# session clusters in the wild bootstrap. Sorting in this order makes every
# bootstrap P value identical to the published one; with any other order they
# would differ only by simulation noise. The order itself carries no information.
PARTICIPANT_ORDER <- c(
  "P49", "P01", "P29", "P25", "P41", "P37", "P05", "P33", "P17", "P09", "P10", "P38", "P21",
  "P39", "P50", "P34", "P18", "P06", "P45", "P11", "P30", "P42", "P43", "P12", "P46", "P26",
  "P40", "P22", "P13", "P31", "P47", "P51", "P27", "P14", "P19", "P28", "P02", "P48", "P15",
  "P35", "P36", "P32", "P03", "P52", "P23", "P07", "P16", "P04", "P20", "P08", "P44", "P24")

# Three analysis frames: one row per participant-game-round (1,040), one row per
# pair-game-round (520), and one row per participant (52).
build_frames <- function() {
  rounds <- read.csv("data/experiment2/exp2_rounds.csv", stringsAsFactors = FALSE) %>%
    rename(participant.code = participant_id, session = session_id) %>%
    mutate(participant.code = factor(participant.code, levels = PARTICIPANT_ORDER),
           PlayStag  = as.numeric(Action),
           stim      = as.integer(Group == 2),
           Game      = factor(Game),
           Round     = factor(Round),
           round_num = as.numeric(as.character(Round)),
           belief01  = pmin(pmax(as.numeric(Beliefs) / 100, 0), 1)) %>%
    as.data.frame()

  pairs <- rounds %>%
    group_by(session, pair_id_unique, Game, Round) %>%
    summarise(stag_stag = first(stag_stag), stim = first(stim), .groups = "drop") %>%
    left_join(rounds %>% group_by(pair_id_unique) %>%
                summarise(across(all_of(controls), mean), .groups = "drop"),
              by = "pair_id_unique") %>%
    as.data.frame()

  subjects <- read.csv("data/experiment2/exp2_participants.csv", stringsAsFactors = FALSE) %>%
    rename(participant.code = participant_id) %>%
    mutate(participant.code = factor(participant.code, levels = PARTICIPANT_ORDER)) %>%
    inner_join(distinct(rounds, participant.code, session, stim), by = "participant.code") %>%
    as.data.frame()

  list(rounds = rounds, pairs = pairs, subjects = subjects)
}

#### Session-clustered inference ####
# CR1 variance with the finite-sample correction G/(G-1) (N-1)/(N-K). Every
# cluster-robust test uses a t distribution with G - 1 degrees of freedom.
cr1_vcov <- function(X, u, cl) {
  G <- length(unique(cl)); N <- nrow(X); K <- ncol(X)
  bread <- solve(crossprod(X))
  scores <- rowsum(X * u, cl, reorder = FALSE)
  (G / (G - 1)) * ((N - 1) / (N - K)) * bread %*% crossprod(scores) %*% bread
}

cr1_t <- function(X, y, cl, j) {
  fit <- lm.fit(X, y); b <- fit$coefficients
  V <- cr1_vcov(X, fit$residuals, cl)
  c(b = unname(b[j]), se = sqrt(V[j, j]), t = unname(b[j] / sqrt(V[j, j])))
}

# Wild cluster bootstrap-t with the null imposed and Webb six-point weights.
wild_cluster_boot <- function(X, y, cl, j, B = 9999) {
  obs <- cr1_t(X, y, cl, j)
  X0 <- X[, -j, drop = FALSE]
  fit0 <- lm.fit(X0, y)
  yhat0 <- as.vector(X0 %*% fit0$coefficients); u0 <- fit0$residuals
  lev <- unique(cl); idx <- match(cl, lev)
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  t_boot <- replicate(B, {
    w <- sample(webb, length(lev), replace = TRUE)
    cr1_t(X, yhat0 + u0 * w[idx], cl, j)["t"]
  })
  mean(abs(t_boot) >= abs(obs["t"]))
}

# The same test, vectorized over draws for large B. With the same seed it draws
# the same weights in the same order as wild_cluster_boot.
wild_cluster_boot_fast <- function(X, y, cl, j, B = 9999, batch = 10000) {
  obs <- cr1_t(X, y, cl, j)
  X0 <- X[, -j, drop = FALSE]
  fit0 <- lm.fit(X0, y)
  yhat0 <- as.vector(X0 %*% fit0$coefficients); u0 <- fit0$residuals
  lev <- unique(cl); idx <- match(cl, lev); G <- length(lev)
  N <- nrow(X); K <- ncol(X); c1 <- (G / (G - 1)) * ((N - 1) / (N - K))
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  A <- solve(crossprod(X), t(X))
  a_j <- A[j, ]
  exceed <- 0; done <- 0
  while (done < B) {
    nb <- min(batch, B - done)
    W <- matrix(sample(webb, G * nb, replace = TRUE), G, nb)
    Ystar <- yhat0 + u0 * W[idx, , drop = FALSE]
    Bhat <- A %*% Ystar
    b <- Bhat[j, ]
    E <- Ystar - X %*% Bhat
    S <- rowsum(a_j * E, idx, reorder = FALSE)
    t_b <- b / sqrt(c1 * colSums(S^2))
    exceed <- exceed + sum(abs(t_b) >= abs(obs["t"]))
    done <- done + nb
  }
  exceed / B
}

# One OLS specification: estimate, session-clustered P, and wild bootstrap P.
session_test <- function(label, formula, data, term, B = 9999) {
  data <- data[complete.cases(data[, all.vars(formula)]), ]
  m <- lm(formula, data = data)
  X <- model.matrix(m); y <- model.response(model.frame(m))
  j <- which(colnames(X) == term)
  cl <- data$session; G <- length(unique(cl))
  b <- coef(m)[term]
  se <- sqrt(vcovCL(m, cluster = cl, type = "HC1")[term, term])
  data.frame(model = label, estimate = unname(b), se = se, N = nrow(data), G = G,
             p_session = 2 * pt(-abs(b / se), df = G - 1),
             p_wild = wild_cluster_boot(X, y, cl, j, B = B), row.names = NULL)
}

#### Formatting ####
fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
fmt_pp  <- function(x, digits = 1) formatC(100 * x, format = "f", digits = digits)
# P values: "<0.001"; four decimals below 0.01; otherwise three.
fmt_p <- function(p) {
  ifelse(p < 0.001, "\\ensuremath{<}0.001",
         ifelse(p < 0.01, formatC(p, format = "f", digits = 4),
                formatC(p, format = "f", digits = 3)))
}
fmt_ci <- function(lo, hi, digits = 2, pct = FALSE) {
  f <- if (pct) function(z) fmt_pp(z, digits) else function(z) fmt_num(z, digits)
  sprintf("[%s, %s]", f(lo), f(hi))
}

# Writes a LaTeX tabular, ready to \input.
write_tabular <- function(df, file, align = NULL, header_lines = NULL, size = NULL) {
  align <- align %||% paste0("l", strrep("r", ncol(df) - 1))
  body <- apply(df, 1, function(r) paste(r, collapse = " & "))
  header <- header_lines %||% paste(paste(colnames(df), collapse = " & "), "\\\\")
  lines <- c(if (!is.null(size)) sprintf("\\%s", size),
             sprintf("\\begin{tabular}{%s}", align), "\\toprule",
             header, "\\midrule",
             paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}")
  writeLines(lines, file)
  message("wrote ", file)
}

#### Numbers reported in the text ####
# Each script collects the statistics it reports with record() and writes them
# to output/numbers/<script>.csv, with the place in the paper they appear.
.numbers <- new.env()
.numbers$rows <- list()
record <- function(where, statistic, value) {
  .numbers$rows[[length(.numbers$rows) + 1]] <-
    data.frame(where = where, statistic = statistic, value = as.character(value))
  invisible(value)
}
write_numbers <- function(name) {
  f <- file.path(OUT_NUM, paste0(name, ".csv"))
  out <- bind_rows(.numbers$rows)
  out$value <- gsub("\\\\ensuremath\\{<\\}", "<", out$value)
  write.csv(out, f, row.names = FALSE)
  .numbers$rows <- list()
  message("wrote ", f)
}

# Saves a ggplot to PDF with fonts embedded (PNAS requires embedded fonts).
save_pdf <- function(fig, file, w, h) {
  tmp <- tempfile(fileext = ".pdf")
  ggplot2::ggsave(tmp, fig, width = w, height = h, encoding = "WinAnsi.enc")
  embedFonts(tmp, outfile = file.path(OUT_FIG, file),
             options = paste("-dPDFSETTINGS=/prepress -dCompatibilityLevel=1.5",
                             "-dEmbedAllFonts=true -dSubsetFonts=true",
                             "-c '<</NeverEmbed [ ]>> setdistillerparams' -f"))
  message("wrote ", file.path(OUT_FIG, file))
}
