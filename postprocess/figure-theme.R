### Figures Theme #####
theme_manuscript <- function() {
  theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10)
    )
}

# Linetype palette 
line_palette_scenario_intensity <- c(
  # Food Insecurity (blues)
  "Food insecurity (best-case)" = "dotted",
  "Food insecurity (main)" = "solid",
  "Food insecurity (worst-case)" = "dashed",
  
  # Health Access (oranges)
  "Healthcare access (best-case)" = "dotted",
  "Healthcare access (main)" = "solid",
  "Healthcare access (worst-case)" = "dashed",
  
  # Climate Change (purples)
  "Climate change (best-case)" = "dotted",
  "Climate change (main)" = "solid",
  "Climate change (worst-case)" = "dashed",
  
  # Baseline
  "Baseline" = "solid"
)
# Color palette (colorblind-friendly)
col_palette_scenario_intensity <- c(
  # Food Insecurity (blues)
  "Food insecurity (best-case)" = "#9ECAE1",
  "Food insecurity (main)" = "#3182BD",
  "Food insecurity (worst-case)" = "#08519C",
  
  # Health Access (oranges)
  "Healthcare access (best-case)" = "#FD8D3C",
  "Healthcare access (main)" = "#E6550D",
  "Healthcare access (worst-case)" = "#7F240D",
  
  # Climate Change (purples)
  "Climate change (best-case)" = "#BCBDDC",
  "Climate change (main)" = "#756BB1",
  "Climate change (worst-case)" = "#3F007D",
  
  # Baseline
  "Baseline" = "#000000"
)
col_palette_scenario_sensitivity <- c(
  # Food Insecurity (blues)
  "Exclude Covid-19 (best-case)" = "#9ECAE1",
  "Exclude Covid-19 (main)" = "#3182BD",
  "Exclude Covid-19 (worst-case)" = "#08519C",
  
  # Health Access (oranges)
  "Include Covid-19 (best-case)" = "#FD8D3C",
  "Include Covid-19 (main)" = "#E6550D",
  "Include Covid-19 (worst-case)" = "#7F240D",
  
  # Climate Change (purples)
  "Exclude data after start of Covid-19 (best-case)" = "#BCBDDC",
  "Exclude data after start of Covid-19 (main)" = "#756BB1",
  "Exclude data after start of Covid-19 (worst-case)" = "#3F007D",
  
  # Baseline
  "Baseline" = "#000000"
)

col_palette_intensity <- c(
  "Best-case" = "#9ECAE1",      # light blue
  "Main" = "#3182BD",      # main blue
  "Worst-case" = "#08519C",     # dark blue
  "Baseline" = "#000000"  # black
)

col_palette_variable <- c(
  "New symptomatic TB cases" = "#2CA02C",    # green
  "New TB diagnoses" = "#1F77B4",         # blue
  "New TB deaths" = "#D62728",                # red
  "Latent TB (prevalence)" = "#3F007D",                # purple
  "Fast latent (prevalence)" = "#D62728",                # purple
  "Slow latent (prevalence)" = "#1F77B4",                # purple
  "TB disease (prevalence)" = "#2CA02C",              # green
  
  # Short labels too
  "Symptomatic" = "#2CA02C",              # green
  "Diagnosed" = "#1F77B4",                # blue
  "Deaths" = "#D62728",                   # red
  "Deaths (diagnosed)" = "#E6550D",                   # red
  "Latent TB infection" = "#9467BD",      # purple
  "Fast-progressing latent TB" = "#9467BD",  # purple
  "Slow-progressing latent TB" = "#9467BD"   # purple
)

# Use in plots: 
#scale_color_manual(values = col_palette_scenario)+
#scale_fill_manual(values = col_palette_scenario)

# ============================================================================
# CUSTOM SCALE FOR ZOO::YEARQTR DATES (works directly with yearqtr objects)
# ============================================================================

scale_x_yearqtr <- function(include_quarter = TRUE,
                            date_breaks_major = 5,  # years
                            date_breaks_minor = 1){ # years) {
  
  # Convert yearqtr to numeric for plotting
  scale_x_continuous(
    name = "", 
    breaks = function(limits) {
      # Create major breaks every N years
      years_min <- floor(limits[1])
      years_max <- ceiling(limits[2])
      major_breaks <- seq(years_min, years_max, by = date_breaks_major)
      return(major_breaks)
    },
    minor_breaks = function(limits) {
      # Create minor breaks every N years (quarterly)
      years_min <- floor(limits[1])
      years_max <- ceiling(limits[2])
      minor_breaks <- seq(years_min, years_max, by = date_breaks_minor / 4)
      return(minor_breaks)
    },
    labels = function(x) {
      # Convert numeric back to yearqtr format
      yq <- zoo::yearqtr(x)
      if (include_quarter) {
        paste0(zoo::as.yearqtr(yq))  # Shows as "2025 Q1" format
      } else {
        format(yq, "%Y")  # Shows just year
      }
    },
    expand = c(0.01, 0.01)
  )
}


save_figure <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  # Saves to results/figures/ folder
  dir.create("results/figures", showWarnings = FALSE)
  
  ggsave(
    filename = paste0("results/figures/", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = "png"
  )
  
  cat("✓ Saved:", filename, "\n")
}
