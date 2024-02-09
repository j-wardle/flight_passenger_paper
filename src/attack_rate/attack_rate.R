# setwd("~/iata_analysis/flight_spread/src/attack_rate")
# setwd("Z:/jwardle/flight_spread/src/attack_rate")
# orderly::orderly_develop_start(parameters = list(combine_months = FALSE, beta = 0.75, delta = 0.25, gamma = 0.25, init_inf = 100, start_month = "feb", start_year = 2020, population_year = 2020, seed_country = "China", pathogen = "sarscov2", flight_scenario = "sarscov2"))


## Find time to first 10 infections in country

attack_rate <- function(results_data) {
  
  results_all <- pivot_wider(results_data,
                             names_from = "variable",
                             values_from = "value")
  
  results_all$population <- results_all$susceptible +
    results_all$exposed +
    results_all$infected +
    results_all$recovered
  
  results_all <- results_all %>% 
    group_by(sim, patch) %>% 
    mutate(starting_population = first(population))
  
  results_all <- results_all %>% 
    filter(time == max(time)) %>% 
    mutate(attack_rate = 1000 * recovered / population,
           pop_change = 100 * (starting_population - population)/starting_population)
  
  # could possibly filter out at this step based on acceptable threshold for population_change
  
  results_attacks <-
    results_all %>%
    ungroup() %>%
    group_by(patch) %>%
    filter(attack_rate != 0) %>%
    summarise(count_threshold = n(),
              attack_rate = quantile(attack_rate,
                                   c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975),
              mean = mean(attack_rate),
              mean_pop = mean(population),
              mean_pop_change = mean(pop_change))
  
  results_attacks <- results_attacks %>%
    mutate(attack_rate = round(attack_rate, 1)) %>%
    tidyr::pivot_wider(id_cols = c(patch, count_threshold, mean, mean_pop, mean_pop_change),
                       names_from = q,
                       values_from = attack_rate) %>%
    select(patch, count_threshold, mean, mean_pop, mean_pop_change, `0.5`, `0.025`, `0.975`)
  
  results_attacks
  
}

output <- readRDS("model_output.rds")

x <- attack_rate(output)

x$beta <- beta
x$delta <- delta
x$gamma <- gamma
x$init_inf <- init_inf
x$pathogen <- pathogen
x$flight_scenario <- flight_scenario
x$start_month <- start_month
x$start_year <- start_year

saveRDS(x, "attack_rate.rds")