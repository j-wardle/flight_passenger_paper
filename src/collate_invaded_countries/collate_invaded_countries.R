# setwd("Z:/jwardle/flight_spread/src/collate_arrival_times")
# orderly::orderly_develop_start(use_draft = TRUE)

dependencies <- list.files(pattern = "^invaded_countries")

invaded_countries_df <- map_dfr(1:length(dependencies), function(x) {
  
  readRDS(paste0("invaded_countries", x, ".rds"))
  
})

saveRDS(invaded_countries_df, "invaded_countries.rds")
