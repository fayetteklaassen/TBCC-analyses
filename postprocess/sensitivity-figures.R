####
results <- read_csv("results/results3.csv")

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

### Prep results 
res_clean <- results %>% 
  mutate(sensitivity = str_remove(sensitivity, "-take2$")) %>%
  filter(is.na(scenario)) %>%
  pivot_wider(id_cols = c("quarter", "sensitivity",
                          "variable"),
              names_from = "quantile",
              values_from = "value") %>%
  left_join(sensitivity_grid,
            by = c("sensitivity" = "scenario_name")) %>%
  mutate(runid= paste0(run_id))  %>%
  left_join(q2d, by = c("quarter" = "quarter")) %>%
  left_join(variable_labels, by = c("variable")) 
  
### Prep results  for data!!!!
analyses_labels <- tribble(
  ~sensitivity, ~scn_name, ~label_cov, ~color,
  
  # Exclude Covid-19 (blues)
  "no_covid", "Food insecurity", "Exclude Covid-19 (Food insecurity)", "#9ECAE1",
  "no_covid", "Healthcare access", "Exclude Covid-19 (Healthcare access)", "#3182BD",
  "no_covid", "Combined effect", "Exclude Covid-19 (Combined effect)", "#08519C",
  "no_covid", "Baseline", "Exclude Covid-19 (Baseline)", "#C6DBEF",
  
  # Include Covid-19 (oranges)
  "yes_covid", "Food insecurity", "Include Covid-19 (Food insecurity)", "#FD8D3C",
  "yes_covid", "Healthcare access", "Include Covid-19 (Healthcare access)", "#E6550D",
  "yes_covid", "Combined effect", "Include Covid-19 (Combined effect)", "#7F240D",
  "yes_covid", "Baseline", "Include Covid-19 (Baseline)", "#FDAE6B",
  
  # Exclude data after start of Covid-19 (purples)
  "stop_covid", "Food insecurity", "Exclude data after start of Covid-19 (Food insecurity)", "#BCBDDC",
  "stop_covid", "Healthcare access", "Exclude data after start of Covid-19 (Healthcare access)", "#756BB1",
  "stop_covid", "Combined effect", "Exclude data after start of Covid-19 (Combined effect)", "#3F007D",
  "stop_covid", "Baseline", "Exclude data after start of Covid-19 (Baseline)", "#DADAEB"
)
col_palette_covid <- setNames(analyses_labels$color, analyses_labels$label_cov)

res_clean_data <- results %>% 
  mutate(sensitivity = str_remove(sensitivity, "-take2$"),
         scenario = case_when(is.na(scenario) ~ "timeseries",
                              TRUE ~ paste(scenario))) %>%
  mutate(
    quarter_full = if_else(scenario == "timeseries",
                           quarter,
                           quarter + Q)) %>% 
  left_join(q2d, by = c("quarter_full" = "quarter")) %>%
  pivot_wider(id_cols = c("date", "sensitivity",
                          "variable", "scenario"),
              names_from = "quantile",
              values_from = "value") %>%
  left_join(scenario_labels, by = c("scenario")) %>%
  left_join(variable_labels, by = c("variable")) %>%
  left_join(sensitivity_grid,
            by = c("sensitivity" = "scenario_name")) %>%
  mutate(runid= paste0(run_id)) %>%
  left_join(analyses_labels, by = c("scn_name", "sensitivity"))


### Prior sensitivity ####

s1 <- ggplot(res_clean %>% 
         # filter(parameter %in% c("r_slow_sym")),
         filter(parameter %in% c("r_slow_sym", "r_fast_sym", "r_sym_diag", "r_fast_clear", "r_slow_clear")),
       aes(x = date, y = med, col = prior_config, linetype = model_config, group = run_id)) +
  geom_line() + 
  theme_manuscript() +
  scale_y_continuous("Per 100K") +
  geom_ribbon(aes(ymin = low, ymax = high, fill = prior_config), alpha = 0.2, col = NA) +
  facet_grid(label_long~parameter, scales = "free_y")
s1
save_figure(s1, "prior-sensitivity.png", height= 10)
#### Data sensitivity ####
s2 <- ggplot(res_clean_data %>% 
         filter(parameter == "data") %>%
         filter(scn_name != "Combined effect") %>%
         filter(scn_int %in% c("Main", "Baseline")),
       aes(x = date, y = med, col = label_cov)) +
  geom_line() + 
  ggtitle("Sensitivity to inclusion of Covid-19 data")+
  theme_manuscript() +
  scale_y_continuous("Per 100K") + 
  geom_ribbon(aes(ymin = low, ymax = high, fill = label_cov), alpha = 0.2, col = NA) +
  facet_wrap(scn_name~label_long, scales = "free_y") +
  scale_color_manual(values = col_palette_covid)+
  scale_fill_manual(values = col_palette_covid)
s2
save_figure(s2, "sensitivity-data.png", height= 20)
### Model sensitivity

prior_names <- tribble(
  ~prior_config, ~prior_label,
  "baseline", "N(0,0.5)",
  "estimated", "N(0,s)",
  "strong" ,"N(0,0.25)",
  "weak" , "N(0,1)"
)
ggplot(res_clean %>% 
         filter(parameter %in% c("random_effect")) %>%
         left_join(prior_names) %>%
         filter(prior_config !="estimated") %>%
         left_join(q2d, by = c("quarter" = "quarter")),
         aes(x = date, y = med, col = prior_label, group = run_id)) +
  geom_line() + 
  theme_manuscript() +
  scale_x_yearqtr()+
  ggtitle("Sensitivity to model specification") + 
  scale_y_continuous("Per 100K") +
  geom_ribbon(aes(ymin = low, ymax = high, fill = prior_label), alpha = 0.2, col = NA) +
  facet_wrap(~label_long, scales = "free_y")

