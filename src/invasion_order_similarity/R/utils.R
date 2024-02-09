compare_patches <- function(invasion_data, path, flights, start, seed_location, n) {
  
  sim_patches <- invasion_data %>% 
    filter(pathogen == path & flight_scenario == flights & start_month == start & patch != seed_location)

  sim_patches_contemp <- sim_patches %>% 
    filter(disruption == "disrupted") %>% 
    arrange(`0.5`) %>% 
    slice_head(n = n)
  
  sim_patches_hist <- sim_patches %>% 
    filter(disruption == "normal") %>% 
    arrange(`0.5`) %>% 
    slice_head(n = n)
  
  match_numb <- length(sim_patches_hist$patch[sim_patches_hist$patch %in% sim_patches_contemp$patch])
  
  match_prop <- match_numb / length(sim_patches_contemp$patch)
  
  out <- data.frame(n = n,
                    sim_patches_hist = length(sim_patches_hist$patch),
                    sim_patches_contemp = length(sim_patches_contemp$patch),
                    match_prop = match_prop,
                    match_numb = match_numb
  )
  
  out
  
}
