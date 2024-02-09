# setwd("~/iata_analysis/flight_spread/src/invaded_countries")
# setwd("Z:/jwardle/flight_spread/src/invaded_countries")
# orderly::orderly_develop_start(parameters = list(combine_months = FALSE, beta = 0.75, delta = 0.25, gamma = 0.25, init_inf = 100, start_month = "feb", start_year = 2020, population_year = 2020, seed_country = "China", pathogen = "sarscov2", flight_scenario = "sarscov2"))
# # Set parameters for developing task
# i <- 20
# orderly::orderly_develop_start(parameters = list(combine_months = FALSE,
#                                                  beta = params[i, "beta"],
#                                                  delta = params[i, "delta"],
#                                                  gamma = params[i, "gamma"],
#                                                  init_inf = params[i, "init_inf"],
#                                                  start_month = params[i, "start_month"],
#                                                  start_year = params[i, "start_year"],
#                                                  population_year = params[i, "population_year"],
#                                                  seed_country = params[i, "seed_country"],
#                                                  pathogen = params[i, "pathogen"],
#                                                  flight_scenario = params[i, "flight_scenario"])
# )


# Load in model outputs
output <- readRDS("model_output.rds")

# Calculate how many countries have been invaded
# (cumulative infections 10 or above) at each timepoint

results_data <- output %>%
  pivot_wider(id_cols = c(sim, patch, time),
              names_from = "variable",
              values_from = "value") %>%
  mutate(total_infections = ifelse(infected > 0, infected + recovered, 0)) %>%
  group_by(sim, patch) %>%
  mutate(sum_infected = sum(infected))

results_data <- results_data %>%
  group_by(sim, patch) %>%
  mutate(arrival_time = min(time[total_infections > 9])) %>%
  mutate(invaded = ifelse(time >= arrival_time, 1, 0))

results_data <- results_data %>%
  group_by(sim, time) %>%
  summarise(invaded_countries = sum(invaded))

results_data <- results_data %>%
  ungroup() %>%
  group_by(time) %>%
  summarise(invasions = quantile(invaded_countries,
                                    c(0.025, 0.5, 0.975)),
            q = c(0.025, 0.5, 0.975),
            mean = mean(invaded_countries)
  )

results_data <- results_data %>%
  mutate(invasions = round(invasions, 1)) %>%
  tidyr::pivot_wider(id_cols = c(time, mean),
                     names_from = q,
                     values_from = invasions) %>%
  select(time, mean, `0.5`, `0.025`, `0.975`)

# Add information on parameters to the output
results_data$beta <- beta
results_data$delta <- delta
results_data$gamma <- gamma
results_data$init_inf <- init_inf
results_data$pathogen <- pathogen
results_data$flight_scenario <- flight_scenario
results_data$start_month <- start_month
results_data$start_year <- start_year

saveRDS(results_data, "invaded_countries.rds")
