# setwd("~/iata_analysis/flight_spread/src/run_flight_model")
# orderly::orderly_develop_start(use_draft = FALSE, parameters = list(combine_months = FALSE, beta = 0.6, delta = 1.0, gamma = 0.5, init_inf = 10, start_month = "jan", start_year = 2019, population_year = 2019, seed_country = "China"))

pops_for_sim <- readRDS("pops_for_sim.rds")
probs_for_sim <- readRDS("probs_for_sim.rds")
# avg_probs_for_sim <- readRDS("avg_probs_for_sim.rds")

pops_for_sim <- filter(pops_for_sim, year == population_year)

# Choose the movement data we want to use in the model scenarios
# Dynamically define the months to be used in the model simulation using year and start month

# Put starting month and year into date format
first_month <- as.Date(paste0("1/", toupper(start_month), "/", start_year), format = "%d/%B/%Y")

# Create a 12 month sequence starting from the initial seeding month
month_sequence <- seq(first_month, by = "month", length.out = 12)

# Create vector of months in the format they are saved in probs_for_sim object
file_suffixes <- paste0(lubridate::year(month_sequence),
                        "_",
                        stringr::str_pad(lubridate::month(month_sequence), width = 2, side = "left", pad = "0")
)

# Select the corresponding months from probs_for_sim object
movement_probs <- map(file_suffixes, function(m) {
  
  probs_for_sim[[m]]
  
})

# Get number of days in the months we are running model
dt <- unname(lubridate::days_in_month(month_sequence))

# Set seed and number of simulations
set.seed(42)
sims <- 100

# Get index of the seed country so that we can apply init_inf to correct country

seed_index <- which(pops_for_sim[,"country_name"] == seed_country)

# Define the starting state
starting_state <- list(
  s_patches = pops_for_sim$population,
  e_patches = 0,
  i_patches = c(rep(0, times = seed_index - 1), init_inf, rep(0, times = nrow(pops_for_sim) - seed_index)),
  r_patches = 0,
  birth_rates = 0,
  death_rates = 0,
  transmission_rates = beta,
  infection_rates = delta,
  recovery_rates = gamma
)


# Run multipatchr model for each movement scenario
# s1 <- Sys.time()
scenario1 <- run_patch_model_vary_movement(movement_probs, dt, sims, starting_state)
scenario1 <- simulation_as_df(scenario1)
# e1 <- Sys.time()

countries <- as.vector(pops_for_sim$country_name)

scenario1$patch <- factor(scenario1$patch, labels = countries)

exclude <- c("birth_rate", "death_rate", "transmission_rate", "infection_rate", "recovery_rate")
scenario1 <- filter(scenario1, !(variable %in% exclude))

saveRDS(scenario1, "scenario_output.rds")

random <- runif(1000, 0, 10000)

saveRDS(random, "random_number_sequence.rds")

#check cluster is using the correct multipatchr version
arguments <- as.list(args(multipatchr:::get_number_migrating))
saveRDS(arguments, "multipatchr_args.rds")
