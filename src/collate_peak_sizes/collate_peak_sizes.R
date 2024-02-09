# setwd("Z:/jwardle/flight_spread/src/collate_peak_sizes")
# orderly::orderly_develop_start(use_draft = TRUE)

dependencies <- list.files(pattern = "^peak_size")

peak_sizes_df <- map_dfr(1:length(dependencies), function(x) {
  
  readRDS(paste0("peak_size", x, ".rds"))
  
})

saveRDS(peak_sizes_df, "peak_sizes.rds")
