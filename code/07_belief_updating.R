# Table S7: belief updating. Change in the reported belief between consecutive
# rounds, regressed on whether the partner chose Hare in the previous round and
# on the previous belief, each interacted with treatment (SI section 3.3).
# Run from the repository root (opening brain-stimulation-coordination.Rproj in
# RStudio sets this automatically).
if (!file.exists("code/00_setup.R")) {
  if (file.exists("00_setup.R")) setwd("..") else
    stop("Set the working directory to the repository root (the folder that contains code/ and data/), ",
         "e.g. setwd(\"path/to/brain-stimulation-coordination\"), or open brain-stimulation-coordination.Rproj.")
}
source("code/00_setup.R")

rounds <- build_frames()$rounds
partner <- rounds %>%
  select(pair_id_unique, round_num, partner.code = participant.code, partner_stag = PlayStag)
upd <- rounds %>%
  inner_join(partner, by = c("pair_id_unique", "round_num"), relationship = "many-to-many") %>%
  filter(participant.code != partner.code) %>%
  arrange(participant.code, pair_id_unique, round_num) %>%
  group_by(participant.code, pair_id_unique) %>%
  mutate(belief = as.numeric(Beliefs), prev_belief = lag(belief),
         hare_prev = 1 - lag(partner_stag)) %>%
  ungroup() %>%
  filter(!is.na(prev_belief)) %>%
  mutate(dbelief = belief - prev_belief, prev_belief_c = prev_belief - 50) %>%
  as.data.frame()
stopifnot(nrow(upd) == 936)
G <- length(unique(upd$session))

terms <- c("hare_prev", "stim:hare_prev", "prev_belief_c", "stim:prev_belief_c", "stim")
fit_updating <- function(d, label) {
  f <- if (length(unique(d$Game)) > 1) dbelief ~ stim * (hare_prev + prev_belief_c) + Game + Round
       else dbelief ~ stim * (hare_prev + prev_belief_c) + Round
  m <- lm(f, data = d)
  V <- vcovCL(m, cluster = d$session, type = "HC1")
  X <- model.matrix(m)
  out <- bind_rows(lapply(terms, function(tm) {
    b <- coef(m)[[tm]]; se <- sqrt(V[tm, tm])
    data.frame(sample = label, term = tm, estimate = b, se = se,
               p = 2 * pt(-abs(b / se), df = G - 1), p_wild = NA_real_)
  }))
  for (tm in c("stim:hare_prev", "stim:prev_belief_c")) {
    set.seed(20261023)
    out$p_wild[out$term == tm] <- wild_cluster_boot_fast(X, d$dbelief, d$session,
                                                         which(colnames(X) == tm), B = 999999)
  }
  out$n <- nrow(d)
  out
}
game_label <- c(stag = "SH", stag_risk_dom = "SH RD")
res <- bind_rows(fit_updating(upd, "Both games"),
                 bind_rows(lapply(names(game_label), function(g)
                   fit_updating(upd[upd$Game == g, ], game_label[[g]]))))
family <- res$sample != "Both games" & grepl(":", res$term)
res$p_holm <- NA_real_
res$p_holm[family] <- p.adjust(res$p_wild[family], "holm")

# Difference between the games in the treatment-by-Hare interaction, from one
# model in which every coefficient differs by game.
upd$rd <- as.integer(upd$Game == "stag_risk_dom")
m3 <- lm(dbelief ~ rd * (stim * (hare_prev + prev_belief_c) + Round), data = upd)
X3 <- model.matrix(m3); j3 <- which(colnames(X3) == "rd:stim:hare_prev")
ct3 <- cr1_t(X3, upd$dbelief, upd$session, j3)
set.seed(20261023)
p3_wild <- wild_cluster_boot_fast(X3, upd$dbelief, upd$session, j3, B = 999999)

#### Table S7 ####
lab <- c(hare_prev = "Partner chose Hare", `stim:hare_prev` = "\\quad $\\times$ Stimulated",
         prev_belief_c = "Previous belief", `stim:prev_belief_c` = "\\quad $\\times$ Stimulated",
         stim = "Stimulated")
samples <- c("Both games", "SH", "SH RD")
cell <- function(tm, f) sapply(samples, function(s) f(res[res$sample == s & res$term == tm, ]))
p_text <- function(p) ifelse(p < 0.001, "$P<0.001$", paste0("$P=", fmt_p(p), "$"))
rows <- bind_rows(lapply(terms, function(tm) {
  r <- data.frame(a = lab[[tm]], t(cell(tm, function(x)
    paste0(fmt_num(x$estimate, 2), " (", fmt_num(x$se, 2), ")"))))
  p <- data.frame(a = "", t(cell(tm, function(x) p_text(if (is.na(x$p_wild)) x$p else x$p_wild))))
  out <- bind_rows(r, p)
  if (grepl(":", tm)) out <- bind_rows(out,
    data.frame(a = "", t(cell(tm, function(x) ifelse(is.na(x$p_holm), "", paste0("Holm ", fmt_p(x$p_holm)))))))
  out
}))
rows <- bind_rows(rows, data.frame(a = "Obs.", t(cell("stim", function(x) format(x$n, big.mark = ",")))))
rows[] <- lapply(rows, function(v) gsub("^-", "$-$", v))
write_tabular(rows, file.path(OUT_TAB, "table_S7_belief_updating.tex"), align = "lrrr", size = "small",
              header_lines = "& Both games & SH & SH RD \\\\")

where <- "Main text, Results (belief updating); SI Table S7"
for (s in samples) {
  x <- res[res$sample == s & res$term == "stim:hare_prev", ]
  record(where, paste(s, "- Hare x Stimulated (belief points)"), fmt_num(x$estimate, 2))
  record(where, paste(s, "- Hare x Stimulated, wild bootstrap P"), fmt_p(x$p_wild))
  if (!is.na(x$p_holm)) record(where, paste(s, "- Hare x Stimulated, Holm P"), fmt_p(x$p_holm))
}
where <- "SI Table S7 caption (difference between the games)"
record(where, "SH RD minus SH, Hare x Stimulated (belief points)", fmt_num(ct3[["b"]], 2))
record(where, "95% CI", fmt_ci(ct3[["b"]] - qt(0.975, G - 1) * ct3[["se"]],
                               ct3[["b"]] + qt(0.975, G - 1) * ct3[["se"]], 2))
record(where, "session-clustered P", fmt_p(2 * pt(-abs(ct3[["t"]]), df = G - 1)))
record(where, "wild bootstrap P", fmt_p(p3_wild))
write_numbers("07_belief_updating")
