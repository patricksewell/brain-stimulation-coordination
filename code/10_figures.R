# Figures: main-text Figures 2-4 and SI Figures S4, S6 and S7.
# (Fig. S5 is drawn in 08_dynamics.R.) Fonts are embedded with Ghostscript, as
# PNAS requires; run in a UTF-8 locale.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")
library(ggplot2)
library(patchwork)

fr <- build_frames()
rounds <- fr$rounds; pairs <- fr$pairs; subjects <- fr$subjects

#### Main-text figures ####
arm_cols <- c(Sham = "darkred", Stimulated = "steelblue")
game_lab <- c(stag = "SH", stag_risk_dom = "SH RD")
arm <- function(stim) factor(stim, 0:1, c("Sham", "Stimulated"))

# Each figure is drawn at its printed size (by default the 6.5 in text width)
# with panel tags, so all text has the same size. The WinAnsi encoding
# renders the en dash; the pdf device draws "-" as a minus sign, so hyphens in
# labels are "­". Arrows come from the Symbol font via plotmath. Run in a
# UTF-8 locale. Fonts are embedded with Ghostscript, as PNAS requires.
pub_theme <- theme_classic(base_size = 8, base_family = "Helvetica") +
  theme(axis.line = element_line(linewidth = 0.3), axis.ticks = element_line(linewidth = 0.3),
        axis.text = element_text(size = 7, colour = "grey20"), legend.position = "none",
        plot.title = element_text(size = 7.5, colour = "grey20"), plot.title.position = "plot")
save_pub <- function(fig, file, h, w = 6.5) {
  fig <- fig + plot_annotation(tag_levels = "A", theme = theme(plot.margin = margin(0, 0, 0, 0))) &
    theme(plot.tag = element_text(size = 9, face = "bold", family = "Helvetica"))
  tmp <- tempfile(fileext = ".pdf")
  ggsave(tmp, fig, width = w, height = h, encoding = "WinAnsi.enc")
  # Ghostscript skips the standard 14 fonts (Helvetica, Symbol) unless told otherwise
  embedFonts(tmp, outfile = file.path(OUT_FIG, file),
             options = paste("-dPDFSETTINGS=/prepress -dCompatibilityLevel=1.5",
                             "-dEmbedAllFonts=true -dSubsetFonts=true",
                             "-c '<</NeverEmbed [ ]>> setdistillerparams' -f"))
}
# Panels split by facet; plots in the left column sit flush with the text margin
facet_theme <- theme(strip.background = element_blank(), strip.text = element_text(size = 8, face = "bold"),
                     panel.spacing = unit(14, "pt"), plot.margin = margin(5.5, 5.5, 5.5, 0))
# Arm legend placed inside a panel; just = the corner it is anchored to
legend_inside <- function(just) {
  theme(legend.position = "inside", legend.position.inside = just, legend.justification = just,
        legend.text = element_text(size = 7.5), legend.key.height = unit(9, "pt"),
        legend.key.width = unit(14, "pt"), legend.background = element_blank())
}

# Mean of session means with the SEM across sessions (sd / sqrt(sessions)), by
# arm and any grouping. Sessions are equal-sized, so the mean equals the raw
# group mean. lo/hi are mean -+ 1 SEM, kept within [0, 1].
means_se <- function(data, y, ...) {
  data %>%
    group_by(stim, session, ...) %>% summarise(v = mean({{ y }}), .groups = "drop") %>%
    group_by(stim, ...) %>% summarise(mean = mean(v), se = sd(v) / sqrt(n()), .groups = "drop") %>%
    mutate(Arm = arm(stim), lo = pmax(mean - se, 0), hi = pmin(mean + se, 1))
}

# Means by round with thin +-1 SEM bars, one panel per game (Figs. 2B, 2C, 3A)
round_panel <- function(means, ylab) {
  pd <- position_dodge(width = 0.3)
  ggplot(means %>% mutate(game = factor(game_lab[as.character(Game)], game_lab)),
         aes(round_num, mean, colour = Arm)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.3, alpha = 0.7,
                  position = pd, show.legend = FALSE) +
    geom_line(linewidth = 0.6, position = pd) +
    geom_point(size = 1.3, position = pd) +
    facet_wrap(~ game) +
    scale_colour_manual(values = arm_cols, name = NULL) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Round", y = ylab) + pub_theme + facet_theme
}

#### Figure 2: Stag and Stag--Stag by game and by round ####
# A: the submitted bar chart, with +-1 SEM across sessions
outcomes <- c("Stag choice", "Efficient coordination (Stag–Stag)")
by_game <- bind_rows(
  means_se(rounds, PlayStag, Game) %>% mutate(outcome = outcomes[1]),
  means_se(pairs, stag_stag, Game) %>% mutate(outcome = outcomes[2])) %>%
  mutate(game = game_lab[as.character(Game)], outcome = factor(outcome, outcomes))
dodge <- position_dodge(width = 0.75)
p2a <- ggplot(by_game, aes(game, mean, fill = Arm)) +
  geom_col(position = dodge, width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.15, linewidth = 0.35,
                colour = "grey20") +
  facet_wrap(~ outcome) +
  scale_fill_manual(values = arm_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = "Proportion") +
  pub_theme + facet_theme + legend_inside(c(0.01, 1)) +
  theme(strip.clip = "off")   # the long outcome name is as wide as its panel

# B, C: by round, each titled by its outcome above the game panels
pairs$round_num <- as.numeric(as.character(pairs$Round))
outcome_title <- theme(plot.title = element_text(size = 8.5, face = "bold", colour = "black", hjust = 0.5,
                                                 margin = margin(b = 4)),
                       plot.title.position = "panel")
p2b <- round_panel(means_se(rounds, PlayStag, Game, round_num), "Proportion choosing Stag") +
  labs(title = "Stag choice") + outcome_title
p2c <- round_panel(means_se(pairs, stag_stag, Game, round_num), "Proportion Stag–Stag") +
  labs(title = "Efficient coordination (Stag–Stag)") + outcome_title
# Figs. 2 and 3 take the PNAS 1.5-column width (11.4 cm); the submitted heights
# of A and B, C
fig_w <- 11.4 / 2.54
save_pub(p2a / p2b / p2c + plot_layout(heights = c(3.8, 3.6, 3.6)), "figure_2_primary.pdf",
         h = 6.9, w = fig_w)

#### Figure 3: beliefs by round, fitted Stag curves by game ####
# A: mean belief by round
p3a <- round_panel(means_se(rounds, belief01, Game, round_num), "Belief that partner chooses Stag") +
  labs(title = "Beliefs") + outcome_title + legend_inside(c(0.01, 0.02))

# B: the A11 belief model, with arm-specific slopes and arm-by-game locations,
# one panel per game. Points are 20-point belief bins at their mean belief,
# with area proportional to the number of choices. Dashed lines mark equal
# expected value.
d <- rounds %>% mutate(rd = as.integer(Game == "stag_risk_dom"))
m <- glm(PlayStag ~ 0 + I(1 - stim) + I((1 - stim) * belief01) + I((1 - stim) * rd) +
           stim + I(stim * belief01) + I(stim * rd), family = binomial, data = d)
by_rd <- function(x) x %>% mutate(game = factor(game_lab[rd + 1], game_lab))
grid <- expand.grid(belief01 = seq(0, 1, by = 0.01), stim = 0:1, rd = 0:1)
grid <- grid %>% mutate(p = predict(m, grid, type = "response"), Arm = arm(stim)) %>% by_rd()
bins <- d %>%
  mutate(bin = pmin(floor(belief01 * 5), 4), Arm = arm(stim)) %>%
  group_by(rd, Arm, bin) %>%
  summarise(belief01 = mean(belief01), stag = mean(PlayStag), n = n(), .groups = "drop") %>% by_rd()
ev_line <- by_rd(data.frame(rd = 0:1, thr = c(25 / 35, 27 / 56)))
p3b <- ggplot(mapping = aes(belief01, colour = Arm)) +
  geom_vline(data = ev_line, aes(xintercept = thr), linetype = "dashed", colour = "grey55", linewidth = 0.3) +
  geom_text(data = ev_line, aes(x = thr + 0.02, y = 0.02, label = "Equal EV"), inherit.aes = FALSE,
            hjust = 0, vjust = 0, size = 6.5 / .pt, colour = "grey40", family = "Helvetica") +
  geom_line(data = grid, aes(y = p), linewidth = 0.6) +
  geom_point(data = bins, aes(y = stag, size = n), alpha = 0.8, shape = 16) +
  facet_wrap(~ game) +
  scale_colour_manual(values = arm_cols) +
  scale_size_area(max_size = 3.5) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Stag choice by belief", x = "Belief that partner chooses Stag",
       y = "Probability of Stag") +
  pub_theme + facet_theme + outcome_title
save_pub(p3a / p3b + plot_layout(heights = c(1, 1.05)), "figure_3_beliefs.pdf", h = 4.6, w = fig_w)

#### Figure 4: lottery residuals, risk types, standardized differences ####

# A: residuals from the sham calibration
cal <- lm(risk_classification ~ risk_will_survey, data = subset(subjects, stim == 0))
subjects$resid <- subjects$risk_classification - predict(cal, subjects)
n_arm <- table(subjects$stim)
set.seed(20261109)
p4a <- ggplot(subjects %>% mutate(Arm = arm(stim)), aes(Arm, resid, colour = Arm)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_boxplot(aes(fill = Arm), width = 0.45, alpha = 0.15, linewidth = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.75, shape = 16) +
  scale_colour_manual(values = arm_cols, aesthetics = c("colour", "fill")) +
  scale_x_discrete(labels = paste0(c("Sham", "Stimulated"), "\n(n = ", n_arm, ")")) +
  labs(x = NULL, y = "Lottery residual") +
  pub_theme + theme(plot.margin = margin(5.5, 5.5, 5.5, 0))   # flush with the text margin

# B: risk types from the lottery certainty equivalent (risk neutral: 150)
types <- c("Risk­averse", "Near risk­neutral", "Risk­seeking")
risk_types <- subjects %>%
  mutate(Arm = arm(stim),
         type = factor(case_when(certainty_equiv < 145 ~ types[1], certainty_equiv <= 155 ~ types[2],
                                 TRUE ~ types[3]), types)) %>%
  count(Arm, type) %>% group_by(Arm) %>% mutate(share = n / sum(n)) %>% ungroup()
# A purple ramp for 4B and 4C, apart from the red and blue of the two arms
purples <- c("#E5D6E8", "#A773A6", "#6A2B66", "#3E1B45")
p4b <- ggplot(risk_types, aes(Arm, share)) +
  geom_col(aes(fill = type), width = 0.54, colour = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(share >= 0.1, scales::percent(share, 1), ""), colour = type),
            position = position_stack(vjust = 0.5), size = 7 / .pt, family = "Helvetica") +
  scale_fill_manual(values = setNames(purples[1:3], types), name = NULL) +
  scale_colour_manual(values = setNames(c("grey10", "grey10", "white"), types), guide = "none") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(x = NULL, y = "Share of participants") +
  pub_theme + theme(legend.position = "bottom", legend.direction = "vertical",
                     legend.text = element_text(size = 7), legend.key.size = unit(7, "pt"),
                     legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(4, "pt"),
                     plot.margin = margin(5.5, 5.5, 5.5, 14))

# C: difference in arm means of the standardized measure, +-1 SEM of the
# difference: each arm's SEM across sessions, combined in quadrature
measures <- c(risk_classification = "Lottery", BRET_Avg_Boxes = "BRET",
              reciprocity_post = "Reciprocity", patience_post = "Patience",
              crt = "Cognitive reflection")
std_diff <- bind_rows(lapply(names(measures), function(v) {
  arms <- subjects %>% mutate(z = as.numeric(scale(.data[[v]]))) %>%
    means_se(z) %>% arrange(stim)
  est <- diff(arms$mean); se <- sqrt(sum(arms$se^2))
  data.frame(measure = measures[[v]], est = est, lo = est - se, hi = est + se)
})) %>% mutate(measure = factor(measure, rev(measures)))   # Lottery at the top
# The direction of the difference is marked beneath the tick labels
p4c <- ggplot(std_diff, aes(est, measure)) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.45, colour = purples[4]) +
  geom_point(size = 1.6, colour = purples[4]) +
  annotate("text", x = c(-0.03, 0.03), y = -Inf, vjust = 2.9, hjust = c(1, 0), size = 6.5 / .pt,
           label = c('symbol("\\254")~"Sham higher"', '"Stimulated higher"~symbol("\\256")'),
           parse = TRUE, colour = "grey35", family = "Helvetica") +
  scale_x_continuous(limits = c(-0.5, 1.05), breaks = seq(-0.5, 1, 0.5)) +
  coord_cartesian(clip = "off") +
  labs(x = "Stimulated – sham difference (SD)", y = NULL) +
  pub_theme + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
                     axis.text.y = element_text(size = 7, colour = "grey10"),
                     axis.title.x = element_text(margin = margin(t = 11)))

# free() lets B's legend sit under its axis rather than below C's axis title
save_pub((p4a | free(p4b, type = "space", side = "b") | p4c) + plot_layout(widths = c(1, 1, 1.6)),
         "figure_4_risk.pdf", 2.5)

#### Check against the revision's figure values ####

#### SI figures ####
# The SI figures use their own theme and do not clip the error bars to [0, 1].
pub_theme_si <- theme_classic(base_size = 8, base_family = "Helvetica") +
  theme(axis.line = element_line(linewidth = 0.3), axis.ticks = element_line(linewidth = 0.3),
        axis.text = element_text(size = 7, colour = "grey20"),
        plot.title = element_text(size = 7.5, colour = "grey20"), plot.title.position = "plot",
        legend.text = element_text(size = 7.5), legend.key.height = unit(9, "pt"),
        legend.key.width = unit(14, "pt"), legend.background = element_blank())
means_se_si <- function(data, y, ...) {
  data %>%
    group_by(stim, session, ...) %>% summarise(v = mean({{ y }}), .groups = "drop") %>%
    group_by(stim, ...) %>% summarise(mean = mean(v), se = sd(v) / sqrt(n()), .groups = "drop") %>%
    mutate(Arm = arm(stim), lo = mean - se, hi = mean + se)
}

eur <- 0.02
max_pts <- c(stag = 35, stag_risk_dom = 56)
earn <- rounds %>% group_by(participant.code, session, stim) %>%
  summarise(euros = eur * sum(as.numeric(Payoff)), .groups = "drop")
stopifnot(nrow(earn) == 52)
rel <- rounds %>% group_by(participant.code, session, stim, Game) %>%
  summarise(rel = mean(as.numeric(Payoff)) / max_pts[[first(Game)]], .groups = "drop")

earn_m <- means_se_si(earn, euros)
rel_m <- means_se_si(rel, rel, Game) %>% mutate(game = factor(c(stag = "SH", stag_risk_dom = "SH RD")[Game],
                                                          c("SH", "SH RD")))
pdg <- position_dodge(width = 0.7)
p_earn <- ggplot(earn_m, aes(Arm, mean, fill = Arm)) +
  geom_col(width = 0.55) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.07, linewidth = 0.3) +
  geom_text(aes(y = mean / 2, label = sprintf("€%.2f", mean)), colour = "white", size = 7 / .pt,
            family = "Helvetica") +
  scale_fill_manual(values = arm_cols, guide = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16)) +
  labs(x = NULL, y = "Potential earnings (euros)") + pub_theme_si
# Benchmark: payoff from choosing Hare in every round, as a share of the maximum
hare_bench <- data.frame(x = 1:2, y = c(25 / 35, 27 / 56))
p_rel <- ggplot(rel_m, aes(game, mean, fill = Arm)) +
  geom_col(width = 0.6, position = pdg) +
  geom_segment(data = hare_bench, aes(x = x - 0.4, xend = x + 0.4, y = y, yend = y),
               inherit.aes = FALSE, linetype = "dashed", colour = "grey30", linewidth = 0.35) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, linewidth = 0.3, position = pdg) +
  scale_fill_manual(values = arm_cols, name = NULL) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  labs(x = NULL, y = "Payoff relative to efficient maximum") +
  pub_theme_si + theme(legend.position = "top")
fig_pay <- (p_earn | p_rel) + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 9, face = "bold", family = "Helvetica"))
save_pdf(fig_pay, "figure_S4_payoffs.pdf", w = 6.5, h = 2.8)

#### Distributions of the lottery score and the BRET ####
# Empirical CDFs by arm, one observation per participant.
cdf_panel <- function(v, xlab) {
  ggplot(subjects %>% mutate(Arm = arm(stim)), aes(.data[[v]], colour = Arm)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
    stat_ecdf(geom = "step", linewidth = 0.6) +
    scale_colour_manual(values = arm_cols, name = NULL) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0.01)) +
    labs(x = xlab, y = "Cumulative share of participants") + pub_theme_si
}
fig_cdf <- (cdf_panel("risk_classification", "Lottery score (1\u201332)") |
              cdf_panel("BRET_Avg_Boxes", "Boxes collected (mean of three rounds)")) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") &
  theme(legend.position = "top", plot.tag = element_text(size = 9, face = "bold", family = "Helvetica"))
save_pdf(fig_cdf, "figure_S6_cdfs.pdf", w = 6.5, h = 2.8)

#### Certainty equivalents from the lottery task ####
# One point per participant; violins (full kernel density) and box plots
# by arm; dashed line at the expected value
# of the lottery (150 euros), the risk-neutral certainty equivalent.
set.seed(20261119)
p_ce <- ggplot(subjects %>% mutate(Arm = arm(stim)), aes(Arm, certainty_equiv, colour = Arm)) +
  geom_hline(yintercept = 150, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
  geom_violin(aes(fill = Arm), trim = FALSE, alpha = 0.12, linewidth = 0.3, width = 0.8) +
  geom_boxplot(aes(fill = Arm), width = 0.18, alpha = 0.3, linewidth = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0, size = 1, alpha = 0.75, shape = 16) +
  scale_colour_manual(values = arm_cols, aesthetics = c("colour", "fill"), guide = "none") +
  scale_y_continuous(breaks = seq(0, 400, 100)) +
  labs(x = NULL, y = "Certainty equivalent (euros)") + pub_theme_si
save_pdf(p_ce, "figure_S7_certainty_equivalents.pdf", w = 3.2, h = 2.6)

record("Main text, Results (potential earnings); SI Fig. S4", "Potential earnings across both games, sham (euros)",
       fmt_num(earn_m$mean[earn_m$stim == 0], 2))
record("Main text, Results (potential earnings); SI Fig. S4", "Potential earnings across both games, active (euros)",
       fmt_num(earn_m$mean[earn_m$stim == 1], 2))
write_numbers("10_figures")
