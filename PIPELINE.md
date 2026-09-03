# Pipeline

## Purpose
To run the model, clean and filter the results, and render result figures and tables.

## Prerequisites
install renv, restore packages with renv::restore()
install rstan (version xxx)

## Input/output
- input data is in `../01-data/data-products/` (hidden)
- output data goes to `results/`

## Steps for reproducability
1. Model run: run `models/run-model-stan.R` (dependent on `models/priors.R`, `models/helper.R` and `models/model.stan`) -> write `results/stan-results.rds` and `results/stan-results-WITHCOVID.rds` 
--> this may take ~10-20 minutes, depending on computer speed (to run 5000 iterations on 3 chains, non-paralelized implementation).
2. Figures/results: run `postprocess/figures-tables.R` (dependent on `postprocess/clean-results.R` and `postprocess/style.R` and `postprocess/summary-stan.R`) -> write `results/<figs&tabs>` and output 
3. Inline results: run `postprocess/in-text.R`(if run in isolation, uncomment the first line, to load the results/data objects)
