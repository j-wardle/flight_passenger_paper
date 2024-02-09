# setwd("~/iata_analysis/flight_spread/src/peak_sizes")
# setwd("Z:/jwardle/flight_spread/src/peak_sizes")
# orderly::orderly_develop_start(parameters = list(combine_months = FALSE, beta = 0.75, delta = 0.25, gamma = 0.25, init_inf = 100, start_month = "feb", start_year = 2020, population_year = 2020, seed_country = "China", pathogen = "sarscov2", flight_scenario = "sarscov2"))

## Function to find times to epidemic peak in each patch

size_at_peak <- function(results_data) {
  
  results_all <- pivot_wider(results_data,
                             names_from = "variable",
                             values_from = "value")
  
  results_all$population <- results_all$susceptible +
    results_all$exposed +
    results_all$infected +
    results_all$recovered
  results_all$scaled_infections <- 1000 * results_all$infected / results_all$population
  
  results_peak <-
    results_all %>%
    group_by(sim, patch) %>%
    slice(which.max(scaled_infections))
  
  peak_sizes <-
    results_peak %>%
    ungroup() %>%
    group_by(patch) %>%
    filter(scaled_infections != 0) %>%
    summarise(count_threshold = n(),
              peak_size = quantile(scaled_infections,
                                   c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975),
              mean = mean(scaled_infections),
              mean_pop = mean(population))
  
  peak_sizes <- peak_sizes %>%
    mutate(peak_size = round(peak_size, 1)) %>%
    tidyr::pivot_wider(id_cols = c(patch, count_threshold, mean, mean_pop),
                       names_from = q,
                       values_from = peak_size) %>%
    select(patch, count_threshold, mean, mean_pop, `0.5`, `0.025`, `0.975`)
  
  peak_sizes
  
}

output <- readRDS("model_output.rds")

x <- size_at_peak(output)

x$beta <- beta
x$delta <- delta
x$gamma <- gamma
x$init_inf <- init_inf
x$pathogen <- pathogen
x$flight_scenario <- flight_scenario
x$start_month <- start_month
x$start_year <- start_year

saveRDS(x, "peak_size.rds")