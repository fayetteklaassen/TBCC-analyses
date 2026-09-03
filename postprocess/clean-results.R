source("helper.R")
source("postprocess/stan-summary.R")

notif <- read_csv(paste0(dir_dat,"data-products/notifications.csv")) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  group_by(date) %>%
  summarize(n = sum(n), .groups = 'drop')

outcomes <- read_csv(paste0(dir_dat,"data-products/outcomes.csv")) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  group_by(date, outcome) %>%
  summarize(n = sum(n), .groups = 'drop') %>%
  filter(outcome == "died")

pop_km <- read_csv(paste0(dir_dat,"data-products/population-karamoja.csv"))%>%
  mutate(date = zoo::as.yearqtr(date))

full_dates <- data.frame("date" = c(tb_km$date,zoo::as.yearqtr(seq.Date(as.Date(last(tb_km$date)), length.out = future +1,by = 'quarter')[-1])))

### WHO data
# ASYMPTOMATIC
aTBC <- c(157, 196, 244)
aTBU <- c(233, 281, 338)
TBC <- c(NA, 253, NA) 
who <- data.frame(
  date = zoo::as.yearqtr("2013 Q1"),
  n = 253,
  outcome = "TB prevelance estimate (survey)"
)

obs <- notif %>% mutate(outcome = "notified") %>%
  bind_rows(outcomes) %>%
  left_join(pop_km %>% transmute(date, pop = pop_assumed)) %>%
  mutate(n = n/pop*100000) %>%
  transmute(date, n, outcome) %>%
 bind_rows(who)
  
#### Stan REsults ####
res <- readRDS(file = "results/stan-results.rds")

d <- summStan(res$fit1) %>%
  mutate(scenario = case_when(is.na(scenario) ~ "timeseries",
                            is.numeric(scenario) ~ paste(scenario)))
# d2 <- summaryFixed(res$fit1)

q2d <- data.frame("date" = zoo::as.yearqtr(full_dates$date), "quarter" = 1:N)
d %>% left_join(q2d, by = "quarter")-> dfull


latent <- d %>%
  filter(variable %in% c("latent_fast", "latent_slow")) %>%
  group_by(scenario, quarter, quantile) %>%
  summarise(
    variable = "latent",
    value = sum(value),
    .groups = "drop"
  ) #%>%
  # mutate(
  #   scenario = case_when(
  #     is.na(scenario) ~ "timeseries",
  #     is.numeric(scenario) ~ paste(scenario))
  # )

cdr <- d %>%
  filter(variable %in% c("new_sym", "new_diagnosed")) %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  mutate(
    variable = "cdr",
    value = 100 * new_diagnosed / new_sym#,
    # scenario = case_when(
    #   is.na(scenario) ~ "timeseries",
    #   is.numeric(scenario) ~ paste(scenario))
  ) %>%
  select(scenario, quarter, quantile, variable, value)

d <- bind_rows(d, latent, cdr)

d %>% 
  left_join(d %>%
              filter(variable == "pop_check") %>%
              select(quarter, quantile, scenario, pop_check = value),
            by = c("quarter", "quantile", "scenario")) %>%
              mutate(value = if_else(
                grepl("rel", variable) | variable == "cdr",
                value,
                value / pop_check * 100000),
                     pop_check = NULL) %>%
  mutate(
  quarter_full = if_else(scenario == "timeseries",
                 quarter,
                 quarter + Q)) %>% 
  left_join(q2d, by = c("quarter_full" = "quarter")) %>%
  pivot_wider(id_cols = c("date", "variable", "scenario"),
              values_from = "value",
              names_from = "quantile") %>% 
  # mutate(scn = case_when(
  #   scenario == "1" ~ "baseline",
  #   scenario == "2" ~ "FI_low",
  #   scenario == "3" ~ "FI_med",
  #   scenario == "4" ~ "FI_high",
  #   scenario == "5" ~ "HA_low",
  #   scenario == "6" ~ "HA_med",
  #   scenario == "7" ~ "HA_high",
  #   scenario == "8" ~ "CC_low",
  #   scenario == "9" ~ "CC_med",
  #   scenario == "10" ~ "CC_high"
  # ),
  # scn_name = case_when(
  #   scenario == "1" ~ "baseline",
  #   scenario == "2" ~ "FI",
  #   scenario == "3" ~ "FI",
  #   scenario == "4" ~ "FI",
  #   scenario == "5" ~ "HA",
  #   scenario == "6" ~ "HA",
  #   scenario == "7" ~ "HA",
  #   scenario == "8" ~ "CC",
  #   scenario == "9" ~ "CC",
  #   scenario == "10" ~ "CC"
  # ),
  # scn_int = case_when(
  #   scenario == "1" ~ "-",
  #   scenario == "2" ~ "low",
  #   scenario == "3" ~ "med",
  #   scenario == "4" ~ "high",
  #   scenario == "5" ~ "low",
  #   scenario == "6" ~ "med",
  #   scenario == "7" ~ "high",
  #   scenario == "8" ~ "low",
  #   scenario == "9" ~ "med",
  #   scenario == "10" ~ "high",
  # )
  # ) %>%
  filter(date < as.yearqtr("2036 Q2")) -> df_res


# Create label mappings
variable_labels <- tribble(
  ~variable, ~label_short, ~label_long,
  "new_sym", "Symptomatic", "New symptomatic TB cases",
  "new_diagnosed", "Diagnosed", "New TB diagnoses",
  "new_died", "Deaths", "New TB deaths",
  "new_inf", "Infections", "New latent infections",
  "new_died_diag", "Deaths (diagnosed)", "Deaths in diagnosed cases",
  "sym", "TB disease (prevalence)", "Prevalent symptomatic cases",
  "latent", "Latent TB (prevalence)", "Latent TB infection",
  "latent_fast", "Fast latent (prevalence)", "Fast-progressing latent TB",
  "latent_slow", "Slow latent (prevalence)", "Slow-progressing latent TB",
  "latent_clear", "Latent clear (prevalence)", "Cleared latent TB infection",
  # "cdr", "CDR", "Case detection rate",
  "add_sym_cum", "Additional symptomatic (cum)", "Cumulative additional symptomatic cases",
  "add_death_cum", "Additional deaths (cum)", "Cumulative additional TB deaths"
)

scenario_labels <- tribble(
  ~scenario, ~scn_short, ~scn_name, ~scn_int, ~label,
  "timeseries","baseline", "Baseline", "Baseline", "Baseline",
  "1","baseline", "Baseline", "Baseline", "Baseline",
  "2","FI_low", "Food insecurity", "Best-case", "Food insecurity (best-case)",
  "3","FI_med", "Food insecurity", "Main", "Food insecurity (main)",
  "4","FI_high", "Food insecurity", "Worst-case", "Food insecurity (worst-case)",
  "5","HA_low", "Healthcare access", "Best-case", "Healthcare access (best-case)",
  "6","HA_med", "Healthcare access", "Main", "Healthcare access (main)",
  "7","HA_high", "Healthcare access", "Worst-case", "Healthcare access (worst-case)",
  "8","CC_low", "Combined effect", "Best-case", "Climate change (best-case)",
  "9","CC_med", "Combined effect", "Main", "Climate change (main)",
  "10","CC_high", "Combined effect", "Worst-case", "Climate change (worst-case)"
)

# Apply labels to your dataframe
df_res_labeled <- df_res %>% 
  left_join(variable_labels, by = "variable") %>%
  left_join(scenario_labels, by = c("scenario")) %>%
  mutate(
    scn_label = coalesce(label, scenario),  # Use full label for titles
    label = coalesce(label_short, variable),  # Use short label for legends
  label = factor(label, 
                 levels = c("Symptomatic", "Diagnosed", "Deaths"),
                 ordered = TRUE),
  label_long = factor(label_long,
                      levels = c("New symptomatic TB cases", 
                                 "New TB diagnoses", 
                                 "New TB deaths"),
                      ordered = TRUE)
  )
# View(df_res_labeled)
