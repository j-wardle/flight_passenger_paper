# setwd("Z:/jwardle/flight_spread/src/collate_arrival_times")
# orderly::orderly_develop_start(use_draft = TRUE, parameters = list(init_inf = 10))

dependencies <- list.files(pattern = "^invasion_time")

arrival_times_df <- map_dfr(1:length(dependencies), function(x) {
  
  readRDS(paste0("invasion_time", x, ".rds"))
  
})

seeds <- init_inf
arrival_times_df <- arrival_times_df %>% 
  filter(init_inf == seeds)

saveRDS(arrival_times_df, "arrival_times.rds")
