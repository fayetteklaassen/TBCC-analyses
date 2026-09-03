### Coding up the Sensitivity analyses ####
library(tidyverse)
sensitivity_grid <- expand_grid(
  parameter = c("r_slow_sym", "r_fast_sym", "r_sym_diag", "r_fast_clear", "r_slow_clear"),
  prior_config = c("baseline", "weak", "strong"),  
  model_config = c("gamma", "uniform")# 3 alternatives
) %>% 
  bind_rows(data.frame("parameter" = "random_effect", "prior_config" = c("baseline", "weak", "estimated", "strong"),
                       "model_config" = c("uniform"))) %>%
  mutate(scenario_name = paste(parameter, prior_config, model_config, sep = "-")) %>%
  bind_rows(data.frame("parameter" = "data", "prior_config" = "data", "model_config" = "uniform", "scenario_name" = c("no_covid", "stop_covid", "yes_covid"))) %>%
  mutate(run_id = row_number())


transform_odds <- function(x, factor) {
  odds <- x / (1 - x)
  (factor * odds) / (1 + factor * odds)
}


adjust_gamma_prior <- function(shape, rate, factor) {
  
  mean_prior <- shape / rate
  
  new_shape <- shape * factor
  new_rate  <- new_shape / mean_prior
  
  list(
    shape = new_shape,
    rate  = new_rate
  )
}

variables_to_keep <- c("new_inf", "new_sym", "new_diagnosed", "new_died")
results <- tibble()
set.seed(1234)
# Run them in batches
for (batch in 1:13) {  # ~3 runs per batch
  batch_runs <- sensitivity_grid %>% filter(run_id %in% ((batch-1)*3+1):(batch*3))
# batch_runs <- sensitivity_grid[35:37,]
  for (run in 1:nrow(batch_runs)) {
    scenario <- batch_runs[run,]
    scenario_name = scenario$scenario_name
    if(scenario$prior_config == "weak") {factor1 <- 2; factor2<- 1.5}
    if(scenario$prior_config == "strong") {factor1 <- 0.5; factor2 <- .75}
    
    if(scenario$model_config == "uniform") {mod <- "models/model.stan"
    dd <- data
    }
    if(scenario$model_config == "gamma") {mod <- "models/model-gamma.stan"
    dd <- gamma_data
    }
    
    
    if(scenario$prior_config != "baseline"){
    if(scenario$parameter == "random_effect"){
      if(scenario$prior_config == "estimated"){
        if(scenario$model_config == "uniform") mod <-  "models/model-re.stan"
          if(scenario$model_config == "gamma") mod <-  "models/model-gamma-re.stan"
      } else{
        dd[[scenario$parameter]] = dd[[scenario$parameter]]*factor
      }
    }else{
      if(scenario$model_config == "uniform"){
    dd[[paste0(scenario$parameter,"_lo")]] <- transform_odds(dd[[paste0(scenario$parameter,"_lo")]], factor1)
    dd[[paste0(scenario$parameter,"_hi")]] <- transform_odds(dd[[paste0(scenario$parameter,"_hi")]], factor1)
      } else {
        tmp <- adjust_gamma_prior(dd[[paste0(scenario$parameter,"_lo")]], dd[[paste0(scenario$parameter,"_hi")]], factor2)
        dd[[paste0(scenario$parameter,"_lo")]] <- as.numeric(tmp[1])
          dd[[paste0(scenario$parameter,"_hi")]] <- as.numeric(tmp[2])
      }
    }
    }
    if(scenario$scenario_name == "no_covid") dd$skip = dd$skip_break
    if(scenario$scenario_name == "stop_covid") dd$skip = dd$skip_stop
    if(scenario$scenario_name == "yes_covid") dd$skip = dd$skip_full
    
    
    fit <- stan(
      file = mod,  # Stan program
      data = dd,    # named list of data
      chains = 2,# number of Markov chains
      warmup = 2000,          # number of warmup iterations per chain
      iter =3000,            # total number of iterations per chain
      cores = 2,              # number of cores (could use one per chain)
      refresh = 1000             # progress shown
    )
    
    results <- bind_rows(results, summStan(fit) %>%
      filter(variable %in% variables_to_keep) %>%
      mutate(sensitivity = paste0(scenario_name, "-take2")))

    rm(fit); gc()  # Clean immediately
  }
  
  cat(paste0("Batch ", batch, " complete. Continuing...\n"))
}

# results %>% mutate(clean_sensitivity = str_remove(sensitivity, "-take2$")) %>%
#   group_by(quarter,variable,quantile, scenario) %>%
#   distinct(clean_sensitivity, .keep_all = TRUE) %>% ungroup() %>% # Keep first of each clean name
#   mutate(sensitivity = clean_sensitivity) %>%
#   select(-clean_sensitivity) -> results2
# write_csv(results2, "results/results2.csv")

results2 %>%
  filter(!sensitivity %in% c("no_covid", "yes_covid", "stop_covid")) %>%
  rbind(results %>% mutate(sensitivity = str_remove(sensitivity, "-take2$"))) %>% write_csv("results3.csv")
