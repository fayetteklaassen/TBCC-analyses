library(glue)
quantile_names <- c(
  "2.5%"  = "_p2_5",
  "50%"   = "_p50",
  "97.5%" = "_p97_5"
)
quantile_labels <- c(
  "2.5%" = "low",
  "50%" = "med",
  "97.5%" = "high"
)
vector_params <- c(
  "latent_fast" = "latent_fast",
  "latent_slow" = "latent_slow",
  "latent_clear" = "latent_clear",
  # "latent" = "latent",
  "sym" = "sym",
  "diagnosed" = "diagnosed",
  "pop_check" = "pop_check",
  "susceptible" = "susceptible",
  "new_diagnosed" = "new_diagnosed",
  "recovered" = "recovered",
  "new_died_sym" = "new_died_sym",
  "new_died_diag" = "new_died_diag",
  "new_died" = "new_died",
  "new_cured" = "new_cured",
  "new_inf" = "new_inf",
  "uninf" = "uninf",
  "new_sym" = "new_sym",
  "new_ltfu" = "new_ltfu")

array_params <- c(
  "proj_latent_fast" = "latent_fast",
  "proj_latent_slow" = "latent_slow",
  "proj_sym" = "sym",
  "proj_diagnosed" = "diagnosed",
  "proj_susceptible" = "susceptible",
  "proj_new_diagnosed" = "new_diagnosed",
  "proj_recovered" = "recovered",
  "proj_new_died_sym" = "new_died_sym",
  "proj_new_died_diag" = "new_died_diag",
  "proj_new_died" = "new_died",
  "proj_new_cured" = "new_cured",
  "proj_new_inf" = "new_inf",
  "proj_uninf" = "uninf",
  "proj_new_sym" = "new_sym",
  "proj_new_ltfu" = "new_ltfu",
  "proj_add_death" = "add_death",
  "proj_add_death_cum" = "add_death_cum",
  "proj_add_sym" = "add_sym",
  "proj_add_sym_cum" = "add_sym_cum",
  "proj_add_death_rel" = "add_death_rel",
  "proj_add_death_cum_rel" = "add_death_cum_rel",
  "proj_add_sym_rel" = "add_sym_rel",
  "proj_add_sym_cum_rel" = "add_sym_cum_rel",
  "proj_pop_check" = "pop_check"
)

pars_vector <- purrr::cross2(
  names(vector_params),
  as.character(1:Q)
) %>%
  purrr::map_chr(
    ~ glue("{.x[[1]]}[{.x[[2]]}]")
  )
S <- length(fi_start) ## from helper!!!!
H <- future ## from from helper!!!!

pars_array <- purrr::cross3(
  names(array_params),
  as.character(1:S),
  as.character(1:H)
) %>%
  purrr::map_chr(
    ~ glue("{.x[[1]]}[{.x[[2]]},{.x[[3]]}]")
  )

pars <- c(pars_vector, pars_array)

params <- c(vector_params, array_params)

split_array_indexing <- function(elnames) {
  
  # Matches a valid indexed variable in Stan, capturing it into two groups
  # regex <- '^([A-Za-z_][A-Za-z_0-9]+)\\[([0-9]+)\\]$'
  regex <- '^([A-Za-z_][A-Za-z_0-9]+)\\[([0-9]+)(?:,([0-9]+))?\\]$'
  # Match everything into a data.frame
  captured <- stringr::str_match(elnames, regex)
  captured <- as.data.frame(captured, stringsAsFactors = FALSE)
  
  # Rename cols, coerce the index to a number
  colnames(captured) <- c(
    "parname",
    "variable",
    "idx1",
    "idx2"
  )
  captured$idx1 <- as.numeric(captured$idx1)
  captured$idx2 <- as.numeric(captured$idx2)
  
  # If idx2 is missing:
  # treat idx1 as quarter
  # and scenario = 1
  
  captured <- captured %>%
    dplyr::mutate(
      period = ifelse(is.na(idx2),"historic", "projection"),
      scenario = ifelse(is.na(idx2), NA_integer_, idx1),
      quarter  = ifelse(is.na(idx2), idx1, idx2),
      quarter_full = if_else(
        period == "historic",
        quarter,
        Q + quarter
      ),
      is_projection = !is.na(idx2)
    ) %>%
    dplyr::select(
      parname,
      variable,
      period,
      scenario,
      quarter,
      quarter_full,
      is_projection
    )
  
  captured
}

summStan <- function(fit1){
  rstan::summary(
    fit1,
    pars = pars,
    probs = c(0.025, 0.5, 0.975)
  )$summary %>% # Get rid of the per-chain summaries by indexing into `$summary`
    as.data.frame %>%
    tibble::as_tibble(rownames = "parname") -> melted
  
  vars_of_interest <- c("variable","scenario", "quarter", names(quantile_names))
  
  
  stan_extracts <- dplyr::left_join(
    melted,
    split_array_indexing(melted$parname),
    by = "parname"
  ) %>%
    # Reformat the dates, and rename some of the variable names
    dplyr::mutate(variable = params[variable]) %>%
    # Eliminate things like R-hat that we don't care about right now
    dplyr::select_at(vars_of_interest) %>%
    # Melt things more to get down to three columns
    tidyr::gather(names(quantile_names), key = "quantile", value = "value") %>%
    mutate(quantile = unname(quantile_labels[quantile]))
    # Create the finalized names for the quantiles, and delete the now-unneeded
    # quantile variable
    # dplyr::mutate(
    #   variable = params[variable]
    #   # variable = paste0(variable, quantile_names[I(quantile)]),
    #   # quantile = NULL
    # ) %>%
    # Cast everything back out
    # tidyr::spread(key = "variable", value = "value")
  
  d <- stan_extracts
  return(d)
}


summaryFixed <- function(fit1) {
  
  c(
    # "r_fast_prog",
    # "r_slow_prog",
    "force",
    "theta",
    "init_sym",
    "init_fast",
    "init_slow",
    "init_diagnosed",
    "r_sym_diag",
    "r_sym_die",
    "r_sym_cure",
    "r_slow_sym",
    "r_slow_clear",
    "r_fast_sym",
    "r_fast_slow",
    "r_fast_clear",
    "prot_reinf"
  ) -> pars_of_interest
  
  # Used for renaming quantiles output by Stan
  quantile_names <- c(
    "2.5%"  = "_p2_5",
    "50%"   = "",
    "97.5%" = "_p97_5"
  )
  
  rstan::summary(
    fit1,
    pars = pars_of_interest,
    probs = c(0.025, 0.5, 0.975)
  )$summary %>% # Get rid of the per-chain summaries by indexing into `$summary`
    as.data.frame %>%
    tibble::as_tibble(rownames = "par") -> melted
  
  # These are the variables that are going to be selected from the melted
  # representation created above
  vars_of_interest <- c("par", names(quantile_names))
  
  result <- dplyr::select_at(melted, vars_of_interest)
  
  result
}

plotStan <- function(){
  
}