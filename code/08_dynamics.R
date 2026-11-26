# Dynamics of play (SI section 3.5):
#   Table S8, Stag choice by the previous outcome (own action, partner's action);
#   Table S9, the same for rounds 1-5 and 6-10;
#   the switching-versus-persistence contrast reported in the main text;
#   Table S10 and Fig. S5, transition probabilities between pair outcomes.
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")
library(ggplot2)

rounds <- build_frames()$rounds
partner <- rounds %>%
  select(pair_id_unique, round_num, partner.code = participant.code, partner_stag = PlayStag)
dyn <- rounds %>%
  inner_join(partner, by = c("pair_id_unique", "round_num"), relationship = "many-to-many") %>%
  filter(participant.code != partner.code) %>%
  arrange(participant.code, pair_id_unique, round_num) %>%
  group_by(participant.code, pair_id_unique) %>%
  mutate(own_prev = lag(PlayStag), partner_prev = lag(partner_stag)) %>%
  ungroup() %>%
  filter(!is.na(own_prev)) %>%
  mutate(state = factor(paste0(ifelse(own_prev == 1, "S", "H"), ",",
                               ifelse(partner_prev == 1, "S", "H")),
                        levels = c("H,H", "S,H", "H,S", "S,S"))) %>%
  as.data.frame()
stopifnot(nrow(dyn) == 936)   # every participant, rounds 2-10

# Stag choice by group within one previous state. P values use t(G - 1) with G
# the number of sessions contributing to the cell.
cell_test <- function(d, s) {
  m <- lm(PlayStag ~ stim, data = d)
  b <- coef(m)[["stim"]]
  se <- sqrt(vcovCL(m, cluster = d$session, type = "HC1")["stim", "stim"])
  Gd <- length(unique(d$session))
  X <- model.matrix(~ stim, data = d)
  set.seed(20261005)
  data.frame(state = s, n_sham = sum(d$stim == 0), n_stim = sum(d$stim == 1),
             sham = mean(d$PlayStag[d$stim == 0]), stim = mean(d$PlayStag[d$stim == 1]),
             estimate = b, se = se, p = 2 * pt(-abs(b / se), df = Gd - 1),
             p_wild = wild_cluster_boot(X, d$PlayStag, d$session, 2, B = 9999))
}
state_rows <- function(x) data.frame(
  a = x$state, b = fmt_pp(x$sham), c = fmt_pp(x$stim), d = fmt_pp(x$estimate),
  e = fmt_pp(x$se), f = fmt_p(x$p), g = fmt_p(x$p_holm), h = fmt_p(x$p_wild),
  j = sprintf("%d / %d", x$n_sham, x$n_stim))
header <- c(paste("& \\multicolumn{3}{c}{Stag choice (\\%)} & &", "\\multicolumn{3}{c}{$P$} & \\\\"),
            "\\cmidrule(lr){2-4} \\cmidrule(lr){6-8}",
            "Previous state & Sham & Stimulated & Diff. & SE & Clustered & Holm & Bootstrap & Obs. \\\\")

#### Table S8 ####
by_state <- bind_rows(lapply(levels(dyn$state), function(s) cell_test(dyn[dyn$state == s, ], s))) %>%
  mutate(p_holm = p.adjust(p, "holm"))
write_tabular(state_rows(by_state) %>% mutate(across(everything(), ~ sub("^-", "$-$", .x))),
              file.path(OUT_TAB, "table_S8_previous_state.tex"), align = "lrrrrrrrr",
              size = "small", header_lines = header)

#### Table S9: rounds 1-5 and 6-10 ####
dyn$half <- ifelse(dyn$round_num <= 5, "Early", "Late")
s9 <- bind_rows(lapply(c("Early", "Late"), function(h) bind_rows(lapply(levels(dyn$state), function(s)
  cbind(cell_test(dyn[dyn$half == h & dyn$state == s, ], s), half = h))))) %>%
  mutate(p_holm = p.adjust(p, "holm"))
panel <- function(label) data.frame(a = label, b = "", c = "", d = "", e = "", f = "", g = "", h = "", j = "")
s9_rows <- function(h) { r <- state_rows(s9[s9$half == h, ]); r$a <- paste0("\\quad ", r$a); r }
write_tabular(bind_rows(panel("\\textit{Rounds 1--5}"), s9_rows("Early"),
                        panel("\\textit{Rounds 6--10}"), s9_rows("Late")) %>%
                mutate(across(everything(), ~ sub("^-", "$-$", .x))),
              file.path(OUT_TAB, "table_S9_previous_state_halves.tex"), align = "lrrrrrrrr",
              size = "small", header_lines = header)

#### Switching versus persistence after miscoordination ####
# Among choices that follow (S,H) or (H,S): does the treatment difference after
# (H,S) (switching to Stag) exceed that after (S,H) (persisting with Stag)?
mis <- dyn[dyn$state %in% c("S,H", "H,S"), ]
mis$after_hs <- as.integer(mis$state == "H,S")
m <- lm(PlayStag ~ stim * after_hs, data = mis)
b <- coef(m)[["stim:after_hs"]]
se <- sqrt(vcovCL(m, cluster = mis$session, type = "HC1")["stim:after_hs", "stim:after_hs"])
Gm <- length(unique(mis$session))
X <- model.matrix(m)
set.seed(20261004)
p_wild <- wild_cluster_boot(X, mis$PlayStag, mis$session, which(colnames(X) == "stim:after_hs"), B = 9999)

where <- "Main text, Results (miscoordination); SI section 3.5; SI Table S8"
for (s in c("H,S", "S,H")) {
  x <- by_state[by_state$state == s, ]
  record(where, paste0("Stag after (", s, "): sham / active (%)"),
         paste(fmt_pp(x$sham), "/", fmt_pp(x$stim)))
}
record(where, "Contrast (switching minus persistence), pp", fmt_pp(b))
record(where, "Contrast, 95% CI (pp)",
       fmt_ci(b - qt(0.975, Gm - 1) * se, b + qt(0.975, Gm - 1) * se, 1, pct = TRUE))
record(where, "Contrast, session-clustered P", fmt_p(2 * pt(-abs(b / se), df = Gm - 1)))
record(where, "Contrast, wild bootstrap P", fmt_p(p_wild))

#### Table S10 and Fig. S5: transitions between pair outcomes ####
# Individual-level transitions from the previous outcome (own, partner) to the
# current outcome (own, partner), by group.
lv <- c("H,H", "S,H", "H,S", "S,S")
trans <- dyn %>%
  mutate(now = factor(paste0(ifelse(PlayStag == 1, "S", "H"), ",",
                             ifelse(partner_stag == 1, "S", "H")), lv)) %>%
  count(stim, state, now) %>%
  complete(stim, state, now, fill = list(n = 0)) %>%
  group_by(stim, state) %>% mutate(prob = n / sum(n)) %>% ungroup()
cell <- function(from, to) {
  p <- trans$prob[trans$state == from & trans$now == to]
  names(p) <- trans$stim[trans$state == from & trans$now == to]
  p[c("0", "1")]
}
rows <- unlist(lapply(lv, function(from) {
  pr <- lapply(lv, function(to) cell(from, to))
  c(paste(from, "&", paste(sapply(pr, function(p) sprintf("%.2f / %.2f", p[1], p[2])), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(pr, function(p) sprintf("($\\Delta = %.2f$)", p[2] - p[1])), collapse = " & "),
          if (from == "S,S") "\\\\" else "\\\\[1.2ex]"))
}))
writeLines(c("\\small", "\\begin{tabular}{@{\\extracolsep{7pt}} ccccc}", "\\\\[-1.8ex]\\hline",
             "\\hline \\\\[-1.8ex]", "PrevOutcome & H,H & S,H & H,S & S,S \\\\", "\\hline \\\\[-1.1ex]",
             rows, "\\hline \\\\[-1.1ex]", "\\end{tabular}"),
           file.path(OUT_TAB, "table_S10_transitions.tex"))
message("wrote ", file.path(OUT_TAB, "table_S10_transitions.tex"))

library(tidygraph)
library(ggraph)
nodes <- data.frame(name = lv, x = c(-1, 0, 0, 1), y = c(0, 1, -1, 0))
markov_plot <- function(a, colour, title, legend) {
  edges <- trans %>% filter(stim == a) %>%
    transmute(from = as.character(state), to = as.character(now), prob)
  ggraph(tbl_graph(nodes = nodes, edges = edges, directed = TRUE), layout = "manual", x = x, y = y) +
    geom_edge_fan(aes(width = prob, alpha = prob), arrow = arrow(length = unit(4, "mm")),
                  end_cap = circle(10, "mm"), edge_colour = colour, show.legend = legend) +
    geom_node_point(shape = 21, size = 20, fill = "white", colour = "black", stroke = 0.5) +
    geom_node_text(aes(label = name), size = 5, fontface = "bold") +
    scale_edge_width(range = c(0.1, 2.5), name = "Probability") +
    scale_edge_alpha(range = c(0.1, 1), guide = "none") +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    labs(title = title) + theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.title = element_text(size = 12), legend.text = element_text(size = 10))
}
fig <- patchwork::wrap_plots(markov_plot(0, "darkred", "Sham", FALSE),
                             markov_plot(1, "steelblue", "Stimulated", TRUE), ncol = 2) &
  theme(plot.margin = margin(8, 8, 8, 8))
ggsave(file.path(OUT_FIG, "figure_S5_markov.png"), fig, width = 11, height = 5.6, dpi = 300, bg = "white")
message("wrote ", file.path(OUT_FIG, "figure_S5_markov.png"))
write_numbers("08_dynamics")
