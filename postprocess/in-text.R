#### In text results
library(zoo)
library(tidyverse)
# source("postprocess/clean-results.R") ## if not already loaded from figures-tables.R
source("postprocess/figures-tables.R")
lastdate <- as.yearqtr("2024 Q4")
year_start <- as.yearqtr(paste(format(lastdate, "%Y"), "Q1"))

quarters <- seq(year_start, lastdate, by = 0.25)

quarters

## incidence Q4
df_res_labeled %>%
  filter(scn_short == "baseline") %>%
  filter(variable == "new_sym") %>%
  filter(date == lastdate)
# incidence Y2024
df_res_labeled %>%
  filter(scn_short == "baseline") %>%
  filter(variable == "new_sym") %>%
  filter(date %in% quarters) %>%
  group_by(variable) %>%
  summarize(low = sum(low), 
            med = sum(med),
            high = sum(high))
## prevalence Q4
df_res_labeled %>%
  filter(scn_short == "baseline") %>%
  filter(variable %in% c("sym", "latent")) %>%
  filter(date == lastdate)
## prevalence WHO
df_res_labeled %>%
  filter(scn_short == "baseline") %>%
  filter(variable %in% c("sym", "latent")) %>%
  filter(date == as.yearqtr("2013 Q1"))

## CDR
df_res %>%
  filter(scenario == "baseline") %>%
  filter(variable %in% c("cdr")) %>%
  group_by(variable) %>%
  summarize(low = mean(low),
            med= mean(med),
            high = mean(high
                        ))

## Worst case Y10
df_res_labeled %>%
  filter(scn_name == "Food insecurity") %>%
  filter(variable %in% c("add_death_cum", "add_death_rel",
                         "add_sym_cum" ,"add_sym_rel")) %>%
  filter(date == as.yearqtr("2034 Q4"))
## Worst case Y10
df_res_labeled %>%
  filter(scn_name == "Healthcare access") %>%
  filter(variable %in% c("add_death_cum", "add_death_rel",
                         "add_sym_cum" ,"add_sym_rel")) %>%
  filter(date == as.yearqtr("2034 Q4"))

### Appendix: convergence diagnostics
# run lightweight diagnostics summary (no draws extraction)
s <- monitor(fit1, print = FALSE)

# s is a matrix/data.frame with columns like:
# mean, se_mean, sd, n_eff, Rhat

# remove warmup-related junk if present (usually not needed, but safe)
s <- as.data.frame(s)

s %>%  mutate(name = row.names(s)) %>%
  filter(!grepl("proj", name)) %>%
  filter(!grepl("new_inf", name))-> ss

# compute min/max, ignoring NA
rhat_min <- min(ss$Rhat, na.rm = TRUE)
rhat_max <- max(ss$Rhat, na.rm = TRUE)

ess_min <- min(ss$n_eff, na.rm = TRUE)
ess_max <- max(ss$n_eff, na.rm = TRUE)

MCSE_min <- min(ss$MCSE_Q50/ss$sd, na.rm = TRUE)
MCSE_max <- max(ss$MCSE_Q50/ss$sd, na.rm = TRUE)

cat("Rhat  min:", rhat_min, " max:", rhat_max, "\n")
cat("ESS   min:", ess_min,  " max:", ess_max,  "\n")
cat("MCSE   min:", MCSE_min,  " max:", MCSE_max,  "\n")

