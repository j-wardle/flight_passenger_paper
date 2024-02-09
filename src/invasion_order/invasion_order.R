# setwd("~/iata_analysis/flight_spread/src/invasion_order")
# setwd("Z:/jwardle/flight_spread/src/invasion_order")
# orderly::orderly_develop_start(parameters = list(combine_months = FALSE, beta = 0.75, delta = 0.25, gamma = 0.25, init_inf = 100, start_month = "feb", start_year = 2020, population_year = 2020, seed_country = "China", pathogen = "sarscov2", flight_scenario = "sarscov2"))


## Find time to first 10 infections in country

# this function uses cumulative sum of infected (but will double count some people)
order_of_invasion <- function(results_data) {
  
  results_data <- results_data %>%
    group_by(sim, patch) %>%
    filter(variable == "infected") %>%
    mutate(cum_inf = cumsum(value))
  
  ten_case <- results_data %>%
    filter(cum_inf > 9) %>%
    group_by(sim, patch) %>%
    slice_head()
  
  ten_case <- ten_case %>% 
    group_by(sim) %>%
    arrange(time) %>%
    mutate(rank = rank(time, ties.method = "first"))
  
  ten_case <- ten_case %>%
    ungroup() %>%
    group_by(patch) %>%
    summarise(count_threshold = n(),
              rank_summary = quantile(rank,
                                      c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975),
              mean = mean(rank)
    )
  
  ten_case <- ten_case %>%
    mutate(rank_summary = round(rank_summary, 1),
           mean = round(mean, 1)) %>%
    tidyr::pivot_wider(id_cols = c(patch, count_threshold, mean),
                       names_from = q,
                       values_from = rank_summary) %>%
    select(patch, count_threshold, mean, `0.5`, `0.025`, `0.975`)
  
  ten_case <- ten_case %>% 
    ungroup() %>%
    arrange(mean) %>%
    mutate(order = rank(mean, ties.method = "first"))
  
  ten_case
  
}

# this function counts infected and recovered. Takes time when infected + recovered > 10
# note: occasionally a country hits this threshold by importing recovereds, without having experienced an epidemic wave
# only count if there has been at least one infection in the country 
order_of_invasion2 <- function(results_data) {
  
  results_data <- results_data %>%
    pivot_wider(id_cols = c(sim, patch, time),
                names_from = "variable",
                values_from = "value") %>% 
    mutate(total_infections = ifelse(infected > 0, infected + recovered, 0)) %>% 
    group_by(sim, patch) %>% 
    mutate(sum_infected = sum(infected))
  
  ten_case <- results_data %>%
    filter(total_infections > 9) %>%
    group_by(sim, patch) %>%
    slice_head()
  
  ten_case <- ten_case %>% 
    group_by(sim) %>%
    arrange(time) %>%
    mutate(rank = rank(time, ties.method = "first"))
  
  ten_case <- ten_case %>%
    ungroup() %>%
    group_by(patch) %>%
    summarise(count_threshold = n(),
              rank_summary = quantile(rank,
                                      c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975),
              mean = mean(rank)
    )
  
  ten_case <- ten_case %>%
    mutate(rank_summary = round(rank_summary, 1),
           mean = round(mean, 1)) %>%
    tidyr::pivot_wider(id_cols = c(patch, count_threshold, mean),
                       names_from = q,
                       values_from = rank_summary) %>%
    select(patch, count_threshold, mean, `0.5`, `0.025`, `0.975`)
  
  ten_case <- ten_case %>% 
    ungroup() %>%
    arrange(mean) %>%
    mutate(order = rank(mean, ties.method = "first"))
  
  ten_case
  
}


# this function counts infected and recovered. Takes time when infected + recovered > 10
# note: occasionally a country hits this threshold by importing recovereds, without having experienced an epidemic wave
# only count if there has been at least one infection in the country 
order_of_invasion3 <- function(results_data) {
  
  results_data <- results_data %>%
    pivot_wider(id_cols = c(sim, patch, time),
                names_from = "variable",
                values_from = "value") %>% 
    mutate(total_infections = ifelse(infected > 0, infected + recovered, 0)) %>% 
    group_by(sim, patch) %>% 
    mutate(sum_infected = sum(infected))
  
  ten_case <- results_data %>%
    filter(total_infections > 9) %>%
    group_by(sim, patch) %>%
    slice_head()
  
  ten_case <- ten_case %>% 
    group_by(sim) %>%
    arrange(time) %>%
    mutate(rank = rank(time, ties.method = "first"))
  
  ten_case <- ten_case %>%
    ungroup() %>%
    group_by(patch) %>%
    summarise(count_threshold = n(),
              rank_summary = quantile(rank,
                                      c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975),
              mean = mean(rank)
    )
  
  ten_case <- ten_case %>%
    mutate(rank_summary = round(rank_summary, 1),
           mean = round(mean, 1)) %>%
    tidyr::pivot_wider(id_cols = c(patch, count_threshold, mean),
                       names_from = q,
                       values_from = rank_summary) %>%
    select(patch, count_threshold, mean, `0.5`, `0.025`, `0.975`)
  
  ten_case <- ten_case %>% 
    ungroup() %>%
    arrange(mean) %>%
    mutate(order = rank(`0.5`, ties.method = "first"))
  
  ten_case
  
}

output <- readRDS("model_output.rds")

x <- order_of_invasion(output)

x$beta <- beta
x$delta <- delta
x$gamma <- gamma
x$init_inf <- init_inf
x$pathogen <- pathogen
x$flight_scenario <- flight_scenario
x$start_month <- start_month
x$start_year <- start_year

saveRDS(x, "invasion_order.rds")

y <- order_of_invasion2(output)

y$beta <- beta
y$delta <- delta
y$gamma <- gamma
y$init_inf <- init_inf
y$pathogen <- pathogen
y$flight_scenario <- flight_scenario
y$start_month <- start_month
y$start_year <- start_year

saveRDS(y, "invasion_order_incl_recovered.rds")


z <- order_of_invasion3(output)

z$beta <- beta
z$delta <- delta
z$gamma <- gamma
z$init_inf <- init_inf
z$pathogen <- pathogen
z$flight_scenario <- flight_scenario
z$start_month <- start_month
z$start_year <- start_year

saveRDS(z, "invasion_order_incl_recovered_using_median_order.rds")