### Figure 1: data + estimates of INCIDENCE ####
### 
source("postprocess/clean-results.R")
library(flextable)
library(officer)

# scenarios <- c(
#   "1" = "baseline",
#   "2" = "FI_low",
#   "3" = "FI_med",
#   "4" = "FI_high",
#   "5" = "HA_low",
#   "6" = "HA_med",
#   "7" = "HA_high",
#   "8" = "CC_low",
#   "9" = "CC_med",
#   "10" = "CC_high"
# )
# df_res <- df_res %>%
#   mutate(scenario = recode(as.character(scenario), !!!scenarios))
### replace all scenario with scn???
f1 <- df_res_labeled %>%
  filter(scenario == "timeseries") %>%
  filter(date < as.yearqtr("2025 Q1")) %>%
  filter(variable %in% c("new_sym", "new_diagnosed", "new_died", "new_died_diag")) %>% 
  ggplot(aes(x = date, y = med, col = label_short)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high, fill = label_short),
              col = NA, alpha = .2) +
  geom_point(data = obs %>% rename(med = n) %>%
               filter(outcome != "TB prevelance estimate (survey)") %>%
               mutate(state = NA, type = NA,
                      outcome = case_when(outcome == "died" ~ "Deaths (reported)",
                                          outcome == "notified" ~ "Diagnosed (reported)")), 
             aes(x = date, y = med, shape = outcome),
             size = 3, colour = "black", inherit.aes = FALSE) +
  ggtitle("Estimated incidence of TB disease states")+
  # scale_y_log10("# per 100K, log(10) transformed") +
  scale_y_continuous("Per 100K") +
  theme_manuscript() +
  scale_x_yearqtr(include_quarter = TRUE, date_breaks_major = 2) +
  scale_shape_manual(values = c(
    "Diagnosed (reported)" = 16,
    "Deaths (reported)" = 17
  )) +
  scale_color_manual(values = col_palette_variable)+
  scale_fill_manual(values = col_palette_variable)
f1
save_figure(f1, "fig-estimated-incidence.png")
### Supplement Figure: estimates of PREVALENCE ####
f2 <- df_res_labeled %>%
  filter(scenario == "timeseries") %>%
  filter(date < as.yearqtr("2025 Q1")) %>%
  filter(variable %in% c("sym","latent", "latent_fast", "latent_slow")) %>%
  ggplot(aes(x = date, y = med, col = label_short)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high, fill = label_short),
              col = NA, alpha = .2) +
  geom_point(data = obs %>% rename(med = n) %>%
               filter(outcome == "TB prevelance estimate (survey)") %>%
               mutate(state = NA, type = NA), 
             aes(x = date, y = med, shape = outcome),
             size = 3, colour = "black", inherit.aes = FALSE) +
  ggtitle("Estimated prevalence of TB disease states")+
  scale_y_log10("# per 100K, log(10) transformed") +
  # scale_y_continuous("# per 100K") +
  theme_manuscript()+
  scale_color_manual(values = col_palette_variable)+
  scale_fill_manual(values = col_palette_variable)+
  scale_x_yearqtr(include_quarter = TRUE, date_breaks_major = 2)
f2
save_figure(f2, "estimated-prevalence.png")
#### Supplement Figure : case detection rate ####
f3 <- df_res_labeled %>%
  filter(variable %in% c("cdr")) %>%
  filter(!scn_int %in% c("Low","High")) %>% 
  ggplot(aes(x = date, y = med, col = scn_label)) +
  geom_line() +
  ggtitle("Case detection rate (CDR)") + 
  scale_y_continuous("CDR (%)") +
  geom_ribbon(aes(ymin = low, ymax = high, fill = scn_label),
              alpha = .2, col = NA) +
  theme_manuscript() 
f3
save_figure(f3, "cdr.png")
#### Figure 2: Projections [[FACTOR BY VARIABLE]] ####
f4<- df_res_labeled %>%
  filter(!scn_int %in% c("Best-case","Worst-case")) %>%
  filter(variable %in% c("new_died", "new_diagnosed", "new_sym")) %>%
  ggplot(aes(x = date, y = med, col = scn_label)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high, fill = scn_label),
              alpha = .2, col = NA) +
  theme_manuscript() + 
  scale_y_continuous("Per 100K") +
  # scale_y_log10("Per 100K") +
  ggtitle("Estimated TB burden, historical and scenario projections") +
  facet_wrap(~label_long, scale = "free_y", ncol = 1)
f4
save_figure(f4, "projected-burden.png", height = 10)
### Supplement figure : Projections x Sensitivity! #####
#### Figure 2: Projections [[FACTOR BY VARIABLE]] ####

f5 <- df_res_labeled %>%
  filter(!scn_short %in% c("baseline", "baseline")) %>% 
  filter(variable %in% c("new_died", "new_diagnosed", "new_sym")) %>%
  select(date, med, low, high, scn_int, variable, label_long, scn_name) %>%
  ggplot(aes(x = date, y = med, col = scn_int)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high, fill = scn_int),
              alpha = .2, col = NA) +
  geom_line(data = df_res_labeled %>%
              filter(scenario == 1) %>%
              filter(variable %in% c("new_died", "new_diagnosed", "new_sym")) %>%
              transmute(date, med, variable, label_long, scn_int = "Baseline"),
            aes(x= date, y = med, col = scn_int),
            inherit.aes = FALSE) +
  geom_ribbon(data = df_res_labeled %>%
                filter(scenario == 1) %>%
                filter(variable %in% c("new_died", "new_diagnosed", "new_sym")) %>%
  transmute(date, low,high, variable, label_long, scn_int = "Baseline"),
              aes(x = date,ymin = low, ymax = high, fill = scn_int),
              alpha = .2, col = NA,
              inherit.aes = FALSE) +
  scale_y_continuous("Per 100K") +
  scale_color_manual(
    values = c(col_palette_intensity)
  ) +
  scale_fill_manual(
    values = col_palette_intensity
  ) +
  theme_manuscript() + 
  scale_x_yearqtr(date_breaks_major = 2)+
  ggtitle("Projected incidence sensitivity analyses")+
  facet_grid(label_long~scn_name, scales = "free_y")

f5
save_figure(f5, "projected-incidence-sensitivity.png", height = 10)
### Figure 3: additional SEQUENTIAL vs CUMULATIVE 
f6 <- df_res_labeled %>%
  filter(scn_name %in% c("Food insecurity", "Healthcare access")) %>%
  filter(!scn_short %in% c("baseline")) %>%
  filter(stringr::str_detect(variable, "cum|add|rel")) %>%
  mutate(
    outcome = str_extract(variable, "death|sym"),
    outcome_lab = case_when(outcome == "death" ~ "TB deaths",
                            outcome == "sym" ~ "TB cases"),
    outcome_lab = factor(outcome_lab, levels = c("TB cases", "TB deaths")),
    rel = if_else(str_detect(variable, "rel$"), "Relative", "Absolute"),
    cum = if_else(str_detect(variable, "cum"), "Cumulative incidence", "Incidence"),
    cum = factor(cum, levels= c("Incidence", "Cumulative incidence"))) %>% 
  filter(rel == "Absolute") %>% 
  ggplot(aes(x = date, y = med, col = scn_label, linetype = scn_label)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high, fill = scn_label),
              alpha = .2, col = NA) +
  theme_manuscript() + 
  scale_y_continuous("Per 100K")+
  scale_x_yearqtr(date_breaks_major = 2)+
  ggtitle("Additional TB disease burden, relative to baseline")+
  facet_wrap(outcome_lab~cum, scale = "free_y")+
  scale_color_manual(values = col_palette_scenario_intensity)+
  scale_fill_manual(values = col_palette_scenario_intensity)+
  scale_linetype_manual(values = line_palette_scenario_intensity)

f6

save_figure(f6, "additional-burden.png", height = 10)
### Table 2: Y1 Y5 Y10

df_res_labeled %>%
  filter(date %in% c(as.yearqtr("2026 Q1"),
                     as.yearqtr("2031 Q1"),
                     as.yearqtr("2036 Q1"))) %>%
  mutate(year = case_when(
    date == as.yearqtr("2026 Q1") ~ "Y1",
    date == as.yearqtr("2031 Q1") ~ "Y5",
    date == as.yearqtr("2036 Q1") ~ "Y10")
  ) %>%
  filter(variable %in% c("add_sym_cum" , "add_death_cum")) %>%
  filter(scn_int == "med") %>%
  mutate(med = round(med, 0),
         low = round(low, 0),
         high = round(high, 0),
         CI95 = sprintf("%.0f (%.0f-%.0f)", med, low, high)) %>%
  pivot_wider(id_cols = c("variable", "scn"), names_from = year, values_from = CI95)->tab2

# Convert to Word-ready table
ft <- flextable(tab2)
ft <- autofit(ft)
# Create Word document
doc <- read_docx()
doc <- body_add_par(doc, "Table 2. Additional cases and deaths", style = "heading 1")
doc <- body_add_flextable(doc, ft)

print(doc, target = "results/table2.docx")
    
### Table S3: Y1 Y5 Y10

df_res_labeled %>%
  filter(date %in% c(as.yearqtr("2025 Q4"),
                     as.yearqtr("2030 Q4"),
                     as.yearqtr("2034 Q4"))) %>%
  mutate(year = case_when(
    date == as.yearqtr("2025 Q4") ~ "Y1",
    date == as.yearqtr("2030 Q4") ~ "Y5",
    date == as.yearqtr("2034 Q4") ~ "Y10")
  ) %>% 
  filter(variable %in% c("add_sym_cum" , "add_death_cum")) %>%
  mutate(med = round(med, 0),
         low = round(low, 0),
         high = round(high, 0),
         CI95 = sprintf("%.0f (%.0f-%.0f)", med, low, high)) %>%
  pivot_wider(id_cols = c("variable", "scn_short"), names_from = year, values_from = CI95)->tab3

# Convert to Word-ready table
ft <- flextable(tab3)
ft <- autofit(ft)
# Create Word document
doc <- read_docx()
doc <- body_add_par(doc, "Table 3. Additional cases and deaths", style = "heading 1")
doc <- body_add_flextable(doc, ft)

print(doc, target = "results/table3.docx")
    