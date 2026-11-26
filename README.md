# Brain Stimulation Increases Strategic Risk-Taking and Coordination

Data and materials for:

> Sewell, P., Díez-Rodríguez, E., Soto-León, V., Apesteguia, J., and Oliviero, A. *Brain Stimulation Increases Strategic Risk-Taking and Coordination.* Manuscript under review at PNAS.

The study reports two sham-controlled, double-blind experiments in which transcranial static magnetic stimulation (tSMS) was applied over the left dorsolateral prefrontal cortex (F3) before participants played repeated coordination games. Both experiments were run in Spanish, in oTree, at the Hospital Nacional de Parapléjicos (Toledo, Spain).

The R code in `code/` reproduces every table, figure and statistic in the paper and its SI Appendix from these data (see [Replication code](#replication-code)).

**Correction to the SI Appendix.** Fig. S3d of the submitted SI Appendix shows Matching Pennies payoffs of +30 and −30. The payoffs used in Experiment 1 were +20 and −20: these were shown on the decision screens and paid in every session, as recorded in `exp1_choices.csv` (`game` = `MP`). No result in the paper or the SI Appendix depends on these values. The figure will be corrected in the next version of the SI Appendix.

## Contents

```
data/
  experiment1/
    exp1_choices.csv              one row per participant x game x round (2,400 rows)
    exp1_participants_wide.csv    one row per participant, all recorded variables (60 rows)
  experiment2/
    exp2_rounds.csv               one row per participant x game x round (1,040 rows)
    exp2_participants.csv         one row per participant, derived measures (52 rows)
    exp2_participants_wide.csv    one row per participant, item-level responses (52 rows)
  blinding_check.csv              both experiments: actual condition and end-of-session guess (112 rows)
code/                             R scripts that reproduce the paper (see below)
output/                           tables, figures and statistics written by the code
materials/
  Experiment2_instructions_screens.pdf   every screen of Experiment 2 in the original Spanish, with English translations
```

## Anonymization

oTree participant and session codes, start times, and all unused platform fields were removed. Sessions and participants carry new identifiers (`S01`, `P01`, ...), assigned within each experiment; in Experiment 1 they follow the chronological order of sessions. No names, contact details, or payment details were ever part of these files. The data keep age, gender, and education, which enter the balance checks and the baseline controls.

## Experiment 1

Sixty participants in 30 fixed pairs. Each pair took part in its own session, so session and pair coincide, and treatment was assigned by pair (15 active, 15 sham). The baseline survey, the lottery task, and the patience task were completed before stimulation; the four games were played after it, ten rounds each, with the same partner.

### `exp1_choices.csv`

| Variable | Meaning |
|---|---|
| `session_id` | session, which is also the pair (`S01` to `S30`) |
| `participant_id` | participant (`P01` to `P60`) |
| `role` | position in the pair (1 or 2) |
| `stim` | 1 = active stimulation, 0 = sham |
| `game` | `SH` Stag Hunt, `BoS` Battle of the Sexes, `Chicken`, `MP` Matching Pennies |
| `round` | 1 to 10 |
| `action_A` | 1 = chose the option labeled A on screen, 0 = option B. In the Stag Hunt, A is Stag. |
| `decision_time` | decision time as recorded by oTree |
| `payoff` | points earned in the round |

### `exp1_participants_wide.csv`

All variables recorded by oTree, plus `session_id`, `participant_id`, `role`, and `stim`. Columns follow oTree's naming, `app.round.player.field`. The main items are:

| Column | Meaning |
|---|---|
| `survey.1.player.age`, `gender`, `education`, `dominancia` | age, gender, education, handedness |
| `survey.1.player.crt_*` | answers to the six cognitive reflection questions |
| `falk_survey.1.player.input_button1` | willingness to take risks (1 to 10) |
| `input_button2` | willingness to give up something today to benefit more in the future (patience) |
| `input_button3`, `input_button4` | willingness to punish someone who treats you / others unfairly |
| `input_button5` | willingness to give to good causes |
| `input_button6` | "When someone does me a favor, I am willing to return it" |
| `input_button7` | "If I am treated very unjustly, I will take revenge at the first occasion…" |
| `input_button8` | "I assume that people have only the best intentions" (trust) |
| `input_button9` | "I am good at math" |
| `input_button10` | "I tend to postpone tasks even if I know it would be better to do them right away" |
| `input_button11` | gift chosen in the hypothetical reciprocity scenario |
| `falk_survey.1.player.donation` | amount donated from a hypothetical €700 windfall |
| `icl.*` | lottery staircase: five choices between a 50:50 lottery and a sure amount |
| `time_elicitation_task.*` | patience staircase: five choices between a payment today and in 12 months |
| `stag.*`, `BoS.*`, `BoSC.*`, `pennies.*` | game choices (`cooperate` = 1 for option A), decision times, and payoffs |
| `payment_info.*` | the game selected for payment and its payoff |

Items 1 to 10 were answered on 1 to 10 scales.

## Experiment 2

Fifty-two participants in 13 sessions of four. Treatment was assigned by session (7 sham sessions, 6 active). After stimulation, each participant played two 10-round Stag Hunt games, each with a fixed partner from the same session, and was rematched for the second game. The game order differs by session. Before every round, participants reported the probability that their partner would choose Stag. The post-stimulation tasks followed. On screen, Stag was labeled B; in these files it is recoded so that `Action = 1` means Stag.

| Game | Both Stag | Stag, partner Hare | Hare |
|---|---|---|---|
| SH (`stag`) | 35 | 0 | 25 |
| SH RD (`stag_risk_dom`) | 56 | 0 | 27 |

### `exp2_rounds.csv`

| Variable | Meaning |
|---|---|
| `session_id`, `participant_id` | anonymized session (`S01` to `S13`) and participant (`P01` to `P52`) |
| `pair_id_unique` | pair within a game (anonymized; each participant is in two pairs) |
| `Group` | 1 = sham, 2 = active stimulation |
| `Game` | `stag` = SH, `stag_risk_dom` = SH RD |
| `Round` | 1 to 10 |
| `game_order` | 1 = this game was played first in the session |
| `stagFirst` | 1 = the session played SH first |
| `Action` | 1 = Stag, 0 = Hare |
| `Beliefs` | stated probability (0 to 100, steps of 5) that the partner chooses Stag, before the choice |
| `Payoff`, `CumulativePayoff` | points in the round, and running total within the game |
| `DecisionTime` | decision time as recorded by oTree |
| `stag_stag` | 1 = both partners chose Stag |
| `coordination_failure` | 1 = chose Stag while the partner chose Hare |
| `pair.code`, `pair_id`, `pair_numeric` | position in the session (P1 to P4) and pair within the session |

The participant measures below are also attached to every row.

### `exp2_participants.csv`

| Variable | Meaning |
|---|---|
| `risk_will_survey` | pre: willingness to take risks (1 to 10) |
| `patience_pre` | pre: willingness to give up something today to benefit more in the future (1 to 10) |
| `pos_reciprocity_pre`, `neg_reciprocity_pre`, `reciprocity_pre` | pre: positive reciprocity (returning a favor), negative reciprocity (weighted average of the two punishment items and the revenge item), and their average |
| `math` | pre: "I am good at math" (1 to 10) |
| `age`, `gender`, `education` | demographics; `gender` = 0 for male and 1 otherwise (female or other); `education` = 1 for a master's degree or doctorate |
| `risk_classification` | post: lottery staircase score, 1 to 32 (higher = more risk taking) |
| `certainty_equiv` | post: certainty equivalent implied by the lottery staircase (risk neutral = 150) |
| `gamma`, `gamma_power` | CRRA coefficients implied by `certainty_equiv` |
| `BRET_boxes_collected1..3`, `BRET_bomb1..3` | post: Bomb Risk Elicitation Task, boxes collected and whether the bomb was collected, in each paid round |
| `BRET_Avg_Boxes`, `BRET_Avg_earnings` | averages over the three paid rounds |
| `patience_post` | post: patience staircase score, 1 to 32 |
| `reciprocity_post` | post: value (€) of the gift chosen in the reciprocity scenario |
| `crt` | post: number of correct answers to the six cognitive reflection questions |
| `risk_taking`, `risk_aggregate` | composite risk indices used in exploratory analyses |

### `exp2_participants_wide.csv`

The item-level responses behind `exp2_participants.csv`, in oTree's naming (`app.round.player.field`): the baseline survey (`falk_survey_updated.*`; buttons 1, 2, 3, 4, 6, 7, and 9 correspond to the items above), the lottery (`icl.*`), patience (`time_elicitation_task.*`), BRET (`BRET_trial.*`, `BRET.*`), cognitive reflection (`crt_long.*`), reciprocity (`reciprocity.*`), the two games (`stag_actionsflipped.*` = SH, `stag_risk_dom.*` = SH RD; `cooperate` = 1 for Stag), and payments.

## Blinding check (both experiments)

### `blinding_check.csv`

At the end of each session, participants were asked whether they believed they had received active stimulation, sham stimulation, or did not know. The answers were recorded in the lab logs.

| Variable | Meaning |
|---|---|
| `experiment` | 1 or 2 |
| `session_id`, `participant_id` | the same anonymized identifiers as in that experiment's data files |
| `stim` | 1 = active stimulation, 0 = sham |
| `guess` | `active`, `sham`, or `unsure` (the participant said they did not know) |

## Materials

`materials/Experiment2_instructions_screens.pdf` shows every screen of Experiment 2 in the original Spanish, in the order of the experiment, with an English translation of each. The SI Appendix of the paper (Section 6) gives a shorter translation of the instructions.

## Replication code

From the repository root, run

```
Rscript code/run_all.R
```

In RStudio, open `brain-stimulation-coordination.Rproj` (this sets the working directory to the repository root) and run `source("code/run_all.R")`. From any other R session, first set the working directory to the repository root with `setwd()`.

Each script can also be run on its own (from the repository root). The scripts write LaTeX tables to `output/tables/`, figures to `output/figures/`, and the statistics reported in the text to `output/numbers/` (one CSV per script, with the place in the paper where each number appears).

| Script | Reproduces |
|---|---|
| `00_setup.R` | shared setup: packages, data loading, session-clustered inference, wild cluster bootstrap |
| `01_balance.R` | Table S1 |
| `02_treatment_effects.R` | main treatment effects; Table S3; the direct lottery comparison |
| `03_robustness.R` | omitting sessions or pairs, collection wave, AR(1) errors, trends over rounds (SI 3.2) |
| `04_experiment1.R` | Table S2 and the Experiment 1 results |
| `05_beliefs.R` | Tables S4 and S5; first-round beliefs; the mixed-strategy benchmark (SI 3.3) |
| `06_structural.R` | Table S6, the random parameter model (SI 3.4) |
| `07_belief_updating.R` | Table S7 |
| `08_dynamics.R` | Tables S8–S10, Fig. S5, and the switching-versus-persistence contrast (SI 3.5) |
| `09_risk_measures.R` | lottery and BRET results, risk types, Holm's correction across the five measures, correlations and attenuation (SI 3.6) |
| `10_figures.R` | Figs. 2–4 and Figs. S4, S6 and S7 |
| `11_blinding.R` | the blinding check: guesses by actual condition, and the χ² test (SI 2.3) |

**Inference.** Treatment was assigned by session, so standard errors are clustered by session (13 clusters in Experiment 2, CR1 correction, t distribution with 12 degrees of freedom; 30 pair-sessions in Experiment 1). Wild cluster bootstrap P values impose the null and use Webb six-point weights. Every bootstrap uses a fixed seed, so the code reproduces the published P values exactly. Several analyses sort participants, and the sort order fixes the order of the session clusters in the bootstrap; `00_setup.R` therefore lists the participants in the order of their original identifiers (the list carries no other information).

**Lottery choices.** The structural model uses the five choices of the lottery task. The data hold the final score (1–32), which determines the whole sequence of choices; `06_structural.R` rebuilds the sequence from it.

**Not reproduced from these files.** Session dates and start times, and participants' payments, come from the lab records and are not part of the public data.

**Software.** R 4.5.0 with dplyr, tidyr, sandwich, lme4, nlme, numDeriv, ggplot2, patchwork, scales, ggraph and tidygraph. The figure scripts embed fonts with Ghostscript (`gs` must be on the path) and should be run in a UTF-8 locale. `output/sessionInfo.txt` lists the exact package versions used. A full run takes about 5 minutes, mostly for the bootstrap tests with 999,999 draws.
