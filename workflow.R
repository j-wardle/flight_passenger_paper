# Workflow for project

library(orderly)
library(dplyr)

# FIGURES 1&2 ----
# Tasks for extracting, formatting and analysing IATA passenger data (requires IATA datasets)
a <- orderly::orderly_run("get_flight_data")
orderly::orderly_commit(a)
a <- orderly::orderly_run("estimate_flight_probs")
orderly::orderly_commit(a)
a <- orderly::orderly_run("quantify_flight_disruption")
orderly::orderly_commit(a)
a <- orderly::orderly_run("visualize_passenger_data") # For Figures 1 and 2
orderly::orderly_commit(a)

# Modelling tasks

# Note we ran simulations on a cluster to improve spreed, but the basic task sequence is set out below
# See the src folder for code for each task

# Set scenario parameters ----

nathistory_params <- data.frame(pathogen = c("mers", "sarscov2", "zika"),
                                beta = c(0.4, 0.75, 2/7),
                                delta = c(1/6, 1/4, 1/14),
                                gamma = c(1/3, 1/4, 1/7)
)

flight_params <- data.frame(flight_scenario = rep(c("mers", "sarscov2", "zika"), each = 4),
                            start_year = c(2014, 2014, 2015, 2015,
                                           2019, 2019, 2020, 2020,
                                           2015, 2015, 2016, 2016),
                            start_month = c("may", "jun", "may", "jun",
                                            "jan", "feb", "jan", "feb",
                                            "feb", "mar", "feb", "mar"),
                            population_year = c(rep(2015, times = 4),
                                                rep(2020, times = 4),
                                                rep(2016, times = 4)),
                            seed_country = c(rep("South Korea", times = 4),
                                             rep("China", times = 4),
                                             rep("Brazil", times = 4)),
                            init_inf = rep(100, times = 12)
)

params <- merge(nathistory_params, flight_params)

params <- params %>%
  mutate(across(where(is.numeric), round, digits=3))

# Loop for running model scenarios ----

# Note this may take a while to run. It can be sped up using a cluster or running the tasks in parallel.
# The run_flight_model tasks requires installation of the following packages:
# remotes::install_github("sangeetabhatia03/multipatchr@rate_to_prob")
# remotes::install_github("sangeetabhatia03/mover")

for (i in 1:(nrow(params))) {

  a <- orderly::orderly_run("run_flight_model",
                       parameters = list(combine_months = FALSE,
                                         beta = params[i, "beta"],
                                         delta = params[i, "delta"],
                                         gamma = params[i, "gamma"],
                                         init_inf = params[i, "init_inf"],
                                         start_month = params[i, "start_month"],
                                         start_year = params[i, "start_year"],
                                         population_year = params[i, "population_year"],
                                         seed_country = params[i, "seed_country"],
                                         pathogen = params[i, "pathogen"],
                                         flight_scenario = params[i, "flight_scenario"]))
  orderly::orderly_commit(a)
}


# Model summaries ----

# Invaded countries over time: FIGURE 3 ----

for (i in 1:(nrow(params))) {

  a <- orderly::orderly_run("invaded_countries",
                       parameters = list(combine_months = FALSE,
                                         beta = params[i, "beta"],
                                         delta = params[i, "delta"],
                                         gamma = params[i, "gamma"],
                                         init_inf = params[i, "init_inf"],
                                         start_month = params[i, "start_month"],
                                         start_year = params[i, "start_year"],
                                         population_year = params[i, "population_year"],
                                         seed_country = params[i, "seed_country"],
                                         pathogen = params[i, "pathogen"],
                                         flight_scenario = params[i, "flight_scenario"])
  )
  orderly_commit(a)

}


source("orderly-helper-scripts/dependencies_collate_invadedcountries.R")

a <- orderly_run("collate_invaded_countries",
                 parameters = list(init_inf = 100))
orderly_commit(a)

a <- orderly_run("compare_invaded_countries",
                 parameters = list(init_inf = 100))
orderly_commit(a)



# Invasion time differences: FIGURE 4 ----

for (i in 1:(nrow(params))) {

  a <- orderly::orderly_run("arrival_times",
                       parameters = list(combine_months = FALSE,
                                         beta = params[i, "beta"],
                                         delta = params[i, "delta"],
                                         gamma = params[i, "gamma"],
                                         init_inf = params[i, "init_inf"],
                                         start_month = params[i, "start_month"],
                                         start_year = params[i, "start_year"],
                                         population_year = params[i, "population_year"],
                                         seed_country = params[i, "seed_country"],
                                         pathogen = params[i, "pathogen"],
                                         flight_scenario = params[i, "flight_scenario"])
  )
  orderly_commit(a)

}

# bring all arrival times together into one place

# generate the orderly.yml file for this task with dependencies dictated by params

source("orderly-helper-scripts/dependencies_collate_arrival.R")

a <- orderly_run("collate_arrival_times",
                 parameters = list(init_inf = 100)
)
orderly_commit(a)
a <- orderly_run("compare_arrival_times",
                 parameters = list(init_inf = 100)
)
orderly_commit(a)


# Invasion order similarity: FIGURE 5 ----

for (i in 1:(nrow(params))) {

  a <- orderly::orderly_run("invasion_order",
                       parameters = list(combine_months = FALSE,
                                         beta = params[i, "beta"],
                                         delta = params[i, "delta"],
                                         gamma = params[i, "gamma"],
                                         init_inf = params[i, "init_inf"],
                                         start_month = params[i, "start_month"],
                                         start_year = params[i, "start_year"],
                                         population_year = params[i, "population_year"],
                                         seed_country = params[i, "seed_country"],
                                         pathogen = params[i, "pathogen"],
                                         flight_scenario = params[i, "flight_scenario"])
  )
  orderly_commit(a)

}

# bring all invasion orders together

source("orderly-helper-scripts/dependencies_collate_invasionorder.R")

a <- orderly_run("collate_invasion_order",
                 parameters = list(init_inf = 100))
orderly_commit(a)

a <- orderly_run("invasion_order_similarity",
                 parameters = list(init_inf = 100))
orderly_commit(a)


# Other summary tasks ----

# Epidemic peak times

for (i in 1:(nrow(params))) {

  a <- orderly::orderly_run("peak_times",
                       parameters = list(combine_months = FALSE,
                                         beta = params[i, "beta"],
                                         delta = params[i, "delta"],
                                         gamma = params[i, "gamma"],
                                         init_inf = params[i, "init_inf"],
                                         start_month = params[i, "start_month"],
                                         start_year = params[i, "start_year"],
                                         population_year = params[i, "population_year"],
                                         seed_country = params[i, "seed_country"],
                                         pathogen = params[i, "pathogen"],
                                         flight_scenario = params[i, "flight_scenario"])
  )
  orderly_commit(a)

}

# bring all peak times together

source("orderly-helper-scripts/dependencies_collate_peaktime.R")

a <- orderly_run("collate_peak_times",
                 parameters = list(init_inf = 100))
orderly_commit(a)

a <- orderly_run("compare_peak_times",
                 parameters = list(init_inf = 100)
)
orderly_commit(a)


# Epidemic peak sizes

for (i in 1:(nrow(params))) {

  a <- orderly::orderly_run("peak_sizes",
                       parameters = list(combine_months = FALSE,
                                         beta = params[i, "beta"],
                                         delta = params[i, "delta"],
                                         gamma = params[i, "gamma"],
                                         init_inf = params[i, "init_inf"],
                                         start_month = params[i, "start_month"],
                                         start_year = params[i, "start_year"],
                                         population_year = params[i, "population_year"],
                                         seed_country = params[i, "seed_country"],
                                         pathogen = params[i, "pathogen"],
                                         flight_scenario = params[i, "flight_scenario"])
  )
  ordelry_commit(a)

}

# bring all peak sizes together

source("orderly-helper-scripts/dependencies_collate_peaksize.R")

a <- orderly_run("collate_peak_sizes",
                 parameters = list(init_inf = 100))
orderly_commit(a)

a <- orderly_run("compare_peak_sizes",
                 parameters = list(init_inf = 100))
orderly_commit(a)


# Now process the epidemic attack rates (across the whole simulation period)

for (i in 1:(nrow(params))) {

  a <- orderly::orderly_run("attack_rate",
                       parameters = list(combine_months = FALSE,
                                         beta = params[i, "beta"],
                                         delta = params[i, "delta"],
                                         gamma = params[i, "gamma"],
                                         init_inf = params[i, "init_inf"],
                                         start_month = params[i, "start_month"],
                                         start_year = params[i, "start_year"],
                                         population_year = params[i, "population_year"],
                                         seed_country = params[i, "seed_country"],
                                         pathogen = params[i, "pathogen"],
                                         flight_scenario = params[i, "flight_scenario"])
  )
  orderly_commit(a)

}



# bring all attack rates together

source("orderly-helper-scripts/dependencies_collate_attackrate.R")

a <- orderly_run("collate_attack_rates",
                 parameters = list(init_inf = 100))
orderly_commit(a)

a <- orderly_run("compare_attack_rates",
                 parameters = list(init_inf = 100))
orderly_commit(a)
