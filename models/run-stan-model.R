## Running the complete state model
## #loading the data
library(tidyverse)
library(zoo)
library(ggplot2)
library(rstan)

source("helper.R")
source("models/priors.R")

### Date expansion
full_dates <- data.frame("date" = c(tb_km$date,zoo::as.yearqtr(seq.Date(as.Date(last(tb_km$date)), length.out = future +1,by = 'quarter')[-1])))
pop_km_x <- pop_km %>% right_join(tb_km, by ="date") 

data <- list(
  pop_init = pop_km %>% filter(date == min(date)) %>% pull(pop_assumed),
  Q = Q,
  N = N,
  H = N-Q,
  S = length(fi_start),
  fi_effect_size = fi_bmi * fi_es,
  fi_time_start = fi_start,
  fi_time_end = fi_end,
  ha_effect_size = ha_es,
  skip_full = as.numeric(tb_km %>% mutate(skip = is.na(n)) %>%pull(skip)), # full data run
  skip_stop = as.numeric(tb_km %>% mutate(n = case_when(
    date < zoo::as.yearqtr("2020 Q1") ~ n)) %>% ## nothing beyond covid run
      mutate(skip = is.na(n)) %>%pull(skip)),
  skip_break = as.numeric(tb_km %>% mutate(n = case_when(
    date < zoo::as.yearqtr("2020 Q1") ~ n, ## exclude only covid run
    date > zoo::as.yearqtr("2023 Q4") ~ n)) %>%
      mutate(skip = is.na(n)) %>%pull(skip)),
  TB_rep = tb_km %>% replace(is.na(.), 0) %>%pull(n),
  skip = as.numeric(tb_km %>% mutate(n = case_when(
    date < zoo::as.yearqtr("2020 Q1") ~ n, ## exclude only covid run
    date > zoo::as.yearqtr("2023 Q4") ~ n)) %>%
      mutate(skip = is.na(n)) %>%pull(skip)),
  xi = c(rep(0,4),(pop_km_x %>% pull(birthrateY_adj)) /4, rep(last(pop_km_x %>% pull(birthrateY_adj))/4, future-4)),
  mu = c(rep(0,4),(pop_km_x %>% pull(deathrateY))/4, rep(last(pop_km_x %>% pull(deathrateY))/4, future-4)),
  p_fast = 0.5,
  prot_reinf_lo = 0.2,
  prot_reinf_hi = 0.4,
  r_fast_sym_lo = -y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
  r_fast_sym_hi = -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1],
  r_fast_slow_lo = -y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[2],
  r_fast_slow_hi = -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[2],
  r_fast_clear_lo = 0.000002,
  r_fast_clear_hi = 0.000006,
  r_slow_sym_lo = -y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[1],
  r_slow_sym_hi = -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[1],
  r_slow_clear_lo = -y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[2],
  r_slow_clear_hi = -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[2],
  r_sym_cure_lo = .008,
  r_sym_cure_hi = .018,
  r_sym_diag_lo = .15, # is around 300K
  r_sym_diag_hi = .35,
  r_sym_die_lo = .027,
  r_sym_die_hi = .047,
  r_diag_die = 0.0251,
  r_diag_sym = 0.0615,
  r_diag_cure = 0.413,
  
  r_ha = 0.5,
  r_nutrition = 0.7,
  re_sd = 0.5,
  time_ha = 10,
  time_nutrition = 20  

)

pop_init = pop_km_x %>% filter(date == min(date)) %>% pull(pop_assumed)
xi = c(rep(0,4),(pop_km_x %>% pull(birthrateY_adj)) /4, rep(last(pop_km_x %>% pull(birthrateY_adj))/4, future-4))
mu = c(rep(0,4),(pop_km_x %>% pull(deathrateY))/4, rep(last(pop_km_x %>% pull(deathrateY))/4, future-4))
popp <- rep(0,length(xi))
popp[1] = pop_init
for(i in 2:length(xi)) { popp[i] = popp[i-1] * (1 - mu[i] + xi[i])}
pop2 <- popp


fit1 <- stan(
  file = "models/model.stan",  # Stan program
  data = data,    # named list of data
  chains = 3,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter =5000,            # total number of iterations per chain
  cores = 3,              # number of cores (could use one per chain)
  refresh = 1000             # progress shown
)
# fit1
res <- list("fit1" = fit1, "tb_tmp" = tb_km)
saveRDS(res, paste0("results/stan-results.rds"))


## with covid included


data <- list(
  pop_init = pop_km %>% filter(date == min(date)) %>% pull(pop_assumed),
  Q = Q,
  N = N,
  H = N-Q,
  S = length(fi_start),
  fi_effect_size = fi_bmi * fi_es,
  fi_time_start = fi_start,
  fi_time_end = fi_end,
  ha_effect_size = ha_es,
  TB_rep = tb_km %>% replace(is.na(.), 0) %>%pull(n),
  skip = as.numeric(tb_km %>% mutate(skip = is.na(n)) %>%pull(skip)), # full data run
  # skip = as.numeric(tb_km %>% mutate(n = case_when(
    # date < zoo::as.yearqtr("2020 Q1") ~ n)) %>% ## nothing beyond covid run
    # date < zoo::as.yearqtr("2020 Q1") ~ n, ## exclude only covid run
    # date > zoo::as.yearqtr("2023 Q4") ~ n)) %>%
      # mutate(skip = is.na(n)) %>%pull(skip)),
  xi = c(rep(0,4),(pop_km_x %>% pull(birthrateY_adj)) /4, rep(last(pop_km_x %>% pull(birthrateY_adj))/4, future-4)),
  mu = c(rep(0,4),(pop_km_x %>% pull(deathrateY))/4, rep(last(pop_km_x %>% pull(deathrateY))/4, future-4)),
  p_fast = 0.5,
  prot_reinf_lo = 0.2,
  prot_reinf_hi = 0.4,
  r_fast_sym_lo = -y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
  r_fast_sym_hi = -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1],
  r_fast_slow_lo = -y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[2],
  r_fast_slow_hi = -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[2],
  r_fast_clear_lo = 0.000002,
  r_fast_clear_hi = 0.000006,
  r_slow_sym_lo = -y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[1],
  r_slow_sym_hi = -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[1],
  r_slow_clear_lo = -y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[2],
  r_slow_clear_hi = -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[2],
  r_sym_cure_lo = .008,
  r_sym_cure_hi = .018,
  r_sym_diag_lo = .15, # is around 300K
  r_sym_diag_hi = .35,
  r_sym_die_lo = .027,
  r_sym_die_hi = .047,
  r_diag_die = 0.0251,
  r_diag_sym = 0.0615,
  r_diag_cure = 0.413,
  
  r_ha = 0.5,
  r_nutrition = 0.7,
  time_ha = 10,
  time_nutrition = 20  
  
)

fit2 <- stan(
  file = "models/model.stan",  # Stan program
  data = data,    # named list of data
  chains = 3,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter =5000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 100             # progress shown
)

# fit1
res2 <- list("fit2" = fit2, "tb_tmp" = tb_km)
saveRDS(res2, paste0("results/stan-results-withCOVID.rds"))

#### Sensitivity data #######
gamma_data <- list(
  # data <- list(
  pop_init = pop_km %>% filter(date == min(date)) %>% pull(pop_assumed),
  Q = Q,
  N = N,
  H = N-Q,
  S = length(fi_start),
  fi_effect_size = fi_bmi * fi_es,
  fi_time_start = fi_start,
  fi_time_end = fi_end,
  ha_effect_size = ha_es,
  skip_full = as.numeric(tb_km %>% mutate(skip = is.na(n)) %>%pull(skip)), # full data run
  skip_stop = as.numeric(tb_km %>% mutate(n = case_when(
    date < zoo::as.yearqtr("2020 Q1") ~ n)) %>% ## nothing beyond covid run
      mutate(skip = is.na(n)) %>%pull(skip)),
  skip_break = as.numeric(tb_km %>% mutate(n = case_when(
    date < zoo::as.yearqtr("2020 Q1") ~ n, ## exclude only covid run
    date > zoo::as.yearqtr("2023 Q4") ~ n)) %>%
      mutate(skip = is.na(n)) %>%pull(skip)),
  TB_rep = tb_km %>% replace(is.na(.), 0) %>%pull(n),
  skip = as.numeric(tb_km %>% mutate(n = case_when(
    date < zoo::as.yearqtr("2020 Q1") ~ n, ## exclude only covid run
    date > zoo::as.yearqtr("2023 Q4") ~ n)) %>%
      mutate(skip = is.na(n)) %>%pull(skip)),
  xi = c(rep(0,4),(pop_km_x %>% pull(birthrateY_adj)) /4, rep(last(pop_km_x %>% pull(birthrateY_adj))/4, future-4)),
  mu = c(rep(0,4),(pop_km_x %>% pull(deathrateY))/4, rep(last(pop_km_x %>% pull(deathrateY))/4, future-4)),
  p_fast = 0.5,
  prot_reinf_lo = 0.2,
  prot_reinf_hi = 0.4,
  r_fast_sym_lo = gamma_mv(mean(c(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
                                  -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1])), 
                           sd_from_ci(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
                                      -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1]))[1],
  r_fast_sym_hi = gamma_mv(mean(c(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
                                  -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1])), 
                           sd_from_ci(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[1],
                                      -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[1]))[3],
  r_fast_slow_lo = gamma_mv(mean(c(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[2],
                                   -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[2])), 
                            sd_from_ci(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[2],
                                       -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[2]))[1],
  r_fast_slow_hi = gamma_mv(mean(c(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[2],
                                   -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[2])), 
                            sd_from_ci(-y2qFull(c(-y_fast_sym_lo, -y_fast_slow_lo),4)[2],
                                       -y2qFull(c(-y_fast_sym_hi, -y_fast_slow_hi),4)[2]))[3],
  r_fast_clear_lo = gamma_mv(0.000004,sd_from_ci(.000002, .000006))[1],
  r_fast_clear_hi =  gamma_mv(0.000004,sd_from_ci(.000002, .000006))[3],
  r_slow_sym_lo = gamma_mv(mean(c(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[1],
                                  -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[1])), 
                           sd_from_ci(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[1],
                                      -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[1]))[1],
  r_slow_sym_hi = gamma_mv(mean(c(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[1],
                                  -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[1])), 
                           sd_from_ci(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[1],
                                      -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[1]))[3],
  r_slow_clear_lo = gamma_mv(mean(c(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[2],
                                    -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[2])), 
                             sd_from_ci(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[2],
                                        -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[2]))[1],
  r_slow_clear_hi = gamma_mv(mean(c(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[2],
                                    -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[2])), 
                             sd_from_ci(-y2qFull(c(-y_slow_sym_lo, -y_slow_clear_lo),4)[2],
                                        -y2qFull(c(-y_slow_sym_hi, -y_slow_clear_hi),4)[2]))[3],
  r_sym_cure_lo = gamma_mv(mean(c(.008,.018)),
                           sd_from_ci(.008,.018))[1],
  r_sym_cure_hi = gamma_mv(mean(c(.008,.018)),
                           sd_from_ci(.008,.018))[3],
  r_sym_diag_lo = gamma_mv(mean(c(.15,.35)),
                           sd_from_ci(.15,.35))[1],
  r_sym_diag_hi = gamma_mv(mean(c(.15,.35)),
                           sd_from_ci(.15,.35))[3],
  r_sym_die_lo = gamma_mv(mean(c(.027,.047)),
                          sd_from_ci(.027,.047))[1],
  r_sym_die_hi = gamma_mv(mean(c(.027,.047)),
                          sd_from_ci(.027,.047))[3],
  r_diag_die = 0.0251,
  r_diag_sym = 0.0615,
  r_diag_cure = 0.413,
  
  r_ha = 0.5,
  r_nutrition = 0.7,
  time_ha = 10,
  time_nutrition = 20,
  re_sd = 0.5
  
)
           