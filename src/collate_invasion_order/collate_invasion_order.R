# setwd("Z:/jwardle/flight_spread/src/collate_invasion_order")
# orderly::orderly_develop_start(use_draft = TRUE)

dependencies <- list.files(pattern = "^invasion_order")

invasion_order_df <- map_dfr(1:length(dependencies), function(x) {
  
  readRDS(paste0("invasion_order", x, ".rds"))
  
})

saveRDS(invasion_order_df, "invasion_order.rds")
