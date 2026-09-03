##### Sensitivty / Appendix results #####
library(tidyverse)
library(rstan)
library(posterior)

res <- readRDS("results/stan-results.rds")
fit1 <- res$fit1
rm(res)


# posterior_samples <- as_draws_df(fit1) %>%
posterior_samples <- as_draws_df(fit1) %>%
  select(
    "r_fast_slow",
    "r_fast_clear", "r_slow_clear", 
    "r_slow_sym", "r_fast_sym",
    "r_sym_diag",
    "r_sym_cure",
    "r_sym_die",
    "prot_reinf",
    "inv_sqrt_phi", "force") %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value")

# # Define priors (as a tibble)
priors <- tribble(
  ~parameter, ~dist, ~min, ~max,
  "r_fast_slow", "uniform", 0.4, 0.5,
  "r_fast_clear", "uniform", 0, 1,
  "r_sym_die", "uniform", 0, 1
)

# Create plot
p <- posterior_samples %>%
  ggplot(aes(x = value, fill = "Posterior")) +
  geom_density(alpha = 0.6, color = NA) +
  facet_wrap(~parameter, scales = "free") +
  # Add priors
  # stat_function(
  #   data = tibble(parameter = c("force", "prot_reinf", "r_sym_cure")),
  #   fun = function(x) dunif(x, min = 0.4, max = 0.5),
  #   aes(fill = "Prior"),
  #   geom = "area",
  #   alpha = 0.3
  # ) +
  # scale_fill_manual(
  #   values = c("Posterior" = "#3366FF", "Prior" = "#FF6633"),
  #   guide = guide_legend(title = "Distribution")
  # ) +
  theme_manuscript() +
  # theme(
  #   legend.position = "top",
  #   strip.text = element_text(size = 11, face = "bold")
  # ) +
  labs(title = "Posterior Distributions",
       x = "Parameter Value",
       y = "Density")


print(p)

save_figure(p, "posteriors.png")
library(bayesplot)
library(tidyverse)
library(gridExtra)

key_params <- c("r_fast_slow",
                "r_fast_clear", "r_slow_clear", 
                "r_slow_sym", "r_fast_sym",
                "r_sym_diag",
                "r_sym_cure",
                "r_sym_die",
                "prot_reinf",
                "inv_sqrt_phi", "force")

posterior_samples <- as_draws_df(fit1) %>%
  select(
    "r_fast_slow",
    "r_fast_clear", "r_slow_clear", 
    "r_slow_sym", "r_fast_sym",
    "r_sym_diag",
    "r_sym_cure",
    "r_sym_die",
    "prot_reinf",
    "inv_sqrt_phi", "force") %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value")

a1 <- traceplot(fit1, pars = key_params)
a1
save_figure(a1, "traceplots.png", height = 10)

### Appendix: convergence diagnostics
# run lightweight diagnostics summary (no draws extraction)
s <- monitor(fit1, print = FALSE)

# s is a matrix/data.frame with columns like:
# mean, se_mean, sd, n_eff, Rhat

# remove warmup-related junk if present (usually not needed, but safe)
s <- as.data.frame(s)

s %>%  mutate(name = row.names(s)) %>%
  filter(!grepl("proj", name)) %>%
  filter(!grepl("new_inf", name)) -> ss

# diagnostics_table <- fit1$summary(variables = key_params) %>%
ss %>% filter(name %in% key_params) %>%
  select(name, mean, sd, Q5, Q95, Rhat, Bulk_ESS, Tail_ESS, MCSE_SD) %>%
  mutate(
    mean = round(mean, 4),
    sd = round(sd, 4),
    q5 = round(Q5, 4),
    q95 = round(Q95, 4),
    rhat = round(Rhat, 4),
    ess_bulk = round(Bulk_ESS, 0),
    ess_tail = round(Tail_ESS, 0),
    mcse = round(MCSE_SD, 4),
    converged = ifelse(Rhat < 1.01, "✓", "✗")
  ) %>%
  transmute(
    "Parameter" = name,
    "Mean" = mean,
    "SD" = sd,
    "5%" = q5,
    "95%" = q95,
    "Rhat" = rhat,
    "ESS (bulk)" = ess_bulk,
    "ESS (tail)" = ess_tail,
    "Converged" = converged
  ) -> diagnostics_table

# Save as table
ft <- flextable(diagnostics_table)
ft <- autofit(ft)
ft <- align(ft, align = "center", part = "all")

doc <- read_docx()
doc <- body_add_heading(doc, "Appendix Table A1: Model Convergence Diagnostics", 
                        level = 1)
doc <- body_add_flextable(doc, ft)
doc <- body_add_par(doc, "Rhat < 1.01 indicates convergence. ESS = effective sample size.")

print(doc, target = "results/appendix_table_a1_diagnostics.docx")


