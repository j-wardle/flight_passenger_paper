# setwd("Z:/jwardle/flight_spread/src/collate_peak_times")
# orderly::orderly_develop_start(use_draft = TRUE)

dependencies <- list.files(pattern = "^peak_time")

peak_times_df <- map_dfr(1:length(dependencies), function(x) {
  
  readRDS(paste0("peak_time", x, ".rds"))
  
})

saveRDS(peak_times_df, "peak_times.rds")
