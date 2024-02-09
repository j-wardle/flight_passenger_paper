# setwd("Z:/jwardle/flight_spread/src/collate_attack_rates")
# orderly::orderly_develop_start(use_draft = TRUE)

dependencies <- list.files(pattern = "^attack_rate")

attack_rates_df <- map_dfr(1:length(dependencies), function(x) {
  
  readRDS(paste0("attack_rate", x, ".rds"))
  
})

saveRDS(attack_rates_df, "attack_rates.rds")
