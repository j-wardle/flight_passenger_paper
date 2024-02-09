# setwd("~/iata_analysis/flight_spread/src/peak_times")
# setwd("Z:/jwardle/flight_spread/src/peak_times")
# orderly::orderly_develop_start(parameters = list(combine_months = FALSE, beta = 0.6, delta = 1, gamma = 0.5, init_inf = 10, obs_year = 2014, start_month = "May"))


## Function to find times to epidemic peak in each patch

time_to_peak <- function(results_data) {

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
    filter(scaled_infections == max(scaled_infections))

  peak_times <-
    results_peak %>%
    ungroup() %>%
    group_by(patch) %>%
    filter(scaled_infections != 0) %>%
    summarise(count_threshold = n(),
              peak_time = quantile(time,
                                   c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975),
              mean = mean(time))

  peak_times <- peak_times %>%
    mutate(peak_time = round(peak_time, 1)) %>%
    tidyr::pivot_wider(id_cols = c(patch, count_threshold, mean),
                       names_from = q,
                       values_from = peak_time) %>%
    select(patch, count_threshold, mean, `0.5`, `0.025`, `0.975`)

  peak_times

}

output <- readRDS("model_output.rds")

x <- time_to_peak(output)

x$beta <- beta
x$delta <- delta
x$gamma <- gamma
x$init_inf <- init_inf
x$pathogen <- pathogen
x$flight_scenario <- flight_scenario
x$start_month <- start_month
x$start_year <- start_year

saveRDS(x, "peak_time.rds")