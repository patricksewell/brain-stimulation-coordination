# Table S2: Experiment 1, stimulation effects in the four games, and the
# Holm correction across games.
#   Pair-clustered inference (30 pairs, t(29)) and, for comparison, mixed models
#   with participant (Stag choice) or pair random intercepts.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

wide <- read.csv("data/experiment1/exp1_participants_wide.csv", stringsAsFactors = FALSE)
stopifnot(nrow(wide) == 60, n_distinct(wide$session_id) == 30)
games <- c(stag = "Stag Hunt", BoS = "Battle of the Sexes", BoSC = "Chicken",
           pennies = "Matching Pennies")

# One row per participant, game and round; action = 1 for the option labeled A
# (Stag in the Stag Hunt).
exp1_long <- bind_rows(lapply(names(games), function(g) bind_rows(lapply(1:10, function(r)
  data.frame(session = wide$session_id, role = as.character(wide$role), stim = wide$stim,
             game = g, Round = r,
             action = as.numeric(wide[[paste0(g, ".", r, ".player.cooperate")]]),
             stringsAsFactors = FALSE))))) %>%
  mutate(subject = paste(session, role, sep = "_"), Round = factor(Round)) %>%
  filter(!is.na(action))

# Pair-level outcome for each game
exp1_pair <- exp1_long %>%
  select(session, role, game, Round, action, stim) %>%
  pivot_wider(names_from = role, values_from = action, names_prefix = "a") %>%
  mutate(outcome = case_when(
    game == "stag"    ~ as.numeric(a1 == 1 & a2 == 1),  # Stag-Stag
    game == "BoS"     ~ as.numeric(a1 == a2),           # coordination on either equilibrium
    game == "BoSC"    ~ as.numeric(a1 != a2),           # either pure-strategy equilibrium
    game == "pennies" ~ as.numeric(a1 == a2)))          # actions match

#### Baseline controls (measured before stimulation) ####
# Risk taking, patience and reciprocity follow the Global Preferences Survey
# weights (Falk et al. 2018); the lottery and patience staircases give 1-32 scores.
lottery_payoffs <- c(10, 30, 50, 70, 90, 110, 130, 150, 170, 190, 210, 230, 250, 270, 290, 310)
b <- function(i) as.numeric(wide[[paste0("falk_survey.1.player.input_button", i)]])
idx <- match(wide$icl.5.player.sure_payoff, lottery_payoffs)
risk_classification <- ifelse(wide$icl.5.player.choice == "A", 2 * idx,
                              ifelse(wide$icl.5.player.choice == "B", 2 * idx - 1, NA))
crt <- with(wide, (survey.1.player.crt_bat == 1) + (survey.1.player.crt_widget == 5) +
              (survey.1.player.crt_lake == 47) + (survey.1.player.crt_elves == 3) +
              (survey.1.player.crt_jerry == 29) + (survey.1.player.crt_athletics == 15))
z <- function(x) as.numeric(scale(x))
ctrl_s2 <- c("risk_taking", "patience", "reciprocity", "crt_scaled", "math", "trust", "education")
exp1_controls <- data.frame(
  session = wide$session_id, role = as.character(wide$role),
  risk_taking = 0.4729985 * risk_classification + 0.5270015 * b(1),
  patience = 0.7115185 * wide$time_elicitation_task.5.player.patience_result + 0.2884815 * b(2),
  reciprocity = 0.5 * (0.7 * b(6) + 0.3 * b(11)) +
    0.5 * ((0.6261938 / 2) * b(3) + (0.6261938 / 2) * b(4) + 0.3738062 * b(7)),
  crt_scaled = crt, math = b(9), trust = b(8),
  education = as.numeric(wide$survey.1.player.education %in% c("Master", "Doctorado")),
  stringsAsFactors = FALSE) %>%
  mutate(across(c(risk_taking, patience, reciprocity, crt_scaled, math, trust), z))
stopifnot(!anyNA(exp1_controls[, ctrl_s2]), sum(exp1_controls$education) == 31)

exp1_long_c <- exp1_long %>% left_join(exp1_controls, by = c("session", "role"))
exp1_pair_c <- exp1_pair %>%
  left_join(exp1_controls %>% group_by(session) %>%
              summarise(across(all_of(ctrl_s2), mean), .groups = "drop"), by = "session")

#### Pair-clustered OLS (each session is one pair) ####
fit_game <- function(d, y, controls = NULL) {
  m <- lm(as.formula(paste(y, "~", paste(c("stim", "Round", controls), collapse = " + "))), data = d)
  b <- coef(m)[["stim"]]
  se <- sqrt(vcovCL(m, cluster = d$session, type = "HC1")["stim", "stim"])
  G <- length(unique(d$session))
  c(estimate = b, se = se, p = 2 * pt(-abs(b / se), df = G - 1))
}
game_test <- function(g, long, pair, controls = NULL) {
  r <- if (g == "stag") fit_game(long[long$game == g, ], "action", controls)
       else fit_game(pair[pair$game == g, ], "outcome", controls)
  data.frame(game = games[[g]], estimate = r[["estimate"]], se = r[["se"]], p = r[["p"]])
}
clustered <- bind_rows(
  bind_rows(lapply(names(games), game_test, exp1_long, exp1_pair)) %>% mutate(p_holm = p.adjust(p, "holm")),
  bind_rows(lapply(names(games), game_test, exp1_long_c, exp1_pair_c, ctrl_s2)) %>% mutate(p_holm = p.adjust(p, "holm")))
ss <- rbind(fit_game(exp1_pair[exp1_pair$game == "stag", ], "outcome"),
            fit_game(exp1_pair_c[exp1_pair_c$game == "stag", ], "outcome", ctrl_s2))

#### Mixed models with random intercepts ####
fit_mixed <- function(g, long, pair, controls = NULL) {
  rhs <- paste(c("stim", "Round", controls), collapse = " + ")
  m <- if (g == "stag") lmer(as.formula(paste("action ~", rhs, "+ (1 | subject)")),
                             data = long[long$game == g, ], REML = FALSE)
       else lmer(as.formula(paste("outcome ~", rhs, "+ (1 | session)")),
                 data = pair[pair$game == g, ], REML = FALSE)
  b <- fixef(m)[["stim"]]; se <- sqrt(vcov(m)["stim", "stim"])
  data.frame(estimate = b, se = se, p = 2 * pnorm(-abs(b / se)))
}
mixed <- bind_rows(lapply(names(games), fit_mixed, exp1_long, exp1_pair),
                   lapply(names(games), fit_mixed, exp1_long_c, exp1_pair_c, ctrl_s2))
ss_mixed <- bind_rows(lapply(list(NULL, ctrl_s2), function(cc) {
  pd <- if (is.null(cc)) exp1_pair else exp1_pair_c
  m <- lmer(as.formula(paste("outcome ~", paste(c("stim", "Round", cc), collapse = " + "),
                             "+ (1 | session)")), data = pd[pd$game == "stag", ], REML = FALSE)
  b <- fixef(m)[["stim"]]; se <- sqrt(vcov(m)["stim", "stim"])
  data.frame(estimate = b, se = se, p = 2 * pnorm(-abs(b / se)))
}))

# Holm across the four games, with Stag-Stag in place of Stag choice (table caption)
holm_ss <- c(p.adjust(c(ss[1, "p"], clustered$p[2:4]), "holm"),
             p.adjust(c(ss[2, "p"], clustered$p[6:8]), "holm"))

#### Table S2 ####
fmt_signed <- function(x) sub("^-", "$-$", sub("^-(0[.]0)$", "\\1", x))
rows <- sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", clustered$game,
                rep(c("Stag choice", "Coordination", "Pure equilibrium", "Actions match"), 2),
                fmt_signed(fmt_pp(clustered$estimate)), fmt_pp(mixed$se), fmt_p(mixed$p),
                fmt_pp(clustered$se), fmt_p(clustered$p))
ss_rows <- sprintf("Stag Hunt & Stag--Stag & %s & %s & %s & %s & %s \\\\",
                   fmt_signed(fmt_pp(ss[, "estimate"])), fmt_pp(ss_mixed$se), fmt_p(ss_mixed$p),
                   fmt_pp(ss[, "se"]), fmt_p(ss[, "p"]))
writeLines(c("\\small", "\\begin{tabular}{llrrrrr}", "\\toprule",
             " & & & \\multicolumn{2}{c}{Participant/pair} & \\\\",
             " & & & \\multicolumn{2}{c}{random intercepts} & \\multicolumn{2}{c}{Pair-clustered} \\\\",
             "\\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
             "Game & Outcome & Est. (pp) & SE & $P$ & SE & $P$ \\\\", "\\midrule",
             "\\multicolumn{7}{l}{\\textit{Without controls}} \\\\", rows[1], ss_rows[1], rows[2:4],
             "\\addlinespace",
             "\\multicolumn{7}{l}{\\textit{With controls}} \\\\", rows[5], ss_rows[2], rows[6:8],
             "\\bottomrule", "\\end{tabular}"),
           file.path(OUT_TAB, "table_S2_experiment1.tex"))

where <- "Main text, Results (Experiment 1); SI Table S2"
record(where, "Stag choice with controls (pp)", fmt_pp(clustered$estimate[5]))
record(where, "Stag choice with controls, pair-clustered P", fmt_p(clustered$p[5]))
record(where, "Smallest Holm-adjusted P across the four games (all specifications)",
       fmt_num(min(c(clustered$p_holm, holm_ss)), 3))
write_numbers("04_experiment1")
