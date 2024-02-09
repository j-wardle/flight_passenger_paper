# Wrapper function to use run_patch_model with dynamic movement matrix

## Output is a list of lists
## Level one: one list element per simulation run
## Level two: one list element per timestep model is run before
## Level three: two elements. 1 is the model state at that point in time, 2 is movement matrix

run_patch_model_vary_movement <- function(movement, dt, sims, starting_state) {
  
  model_output <- vector(mode = "list", length = sims)
  
  for (sim in seq_len(sims)) {
    
    model_output[[sim]] <- vector(mode = "list", length = length(movement))
    
    model_output[[sim]][[1]] <- run_patch_model(
      state_in = state(
        s_patches = starting_state$s_patches,
        e_patches = starting_state$e_patches,
        i_patches = starting_state$i_patches,
        r_patches = starting_state$r_patches,
        birth_rates = starting_state$birth_rates,
        death_rates = starting_state$death_rates,
        transmission_rates = starting_state$transmission_rates,
        infection_rates = starting_state$infection_rates,
        recovery_rates = starting_state$recovery_rates, # day^-1
        movement_rate = movement[[1]]
      ),
      dt = dt[1],
      nsim = 1
    )
    
    final_state <- as.data.frame(model_output[[sim]][[1]][[1]][[dt[1]]])
    
    for (i in 2:length(movement)) {
      
      model_output[[sim]][[i]] <- run_patch_model(
        state_in = state(
          s_patches = final_state$value[final_state$variable == "susceptible"],
          e_patches = final_state$value[final_state$variable == "exposed"],
          i_patches = final_state$value[final_state$variable == "infected"],
          r_patches = final_state$value[final_state$variable == "recovered"],
          birth_rates = starting_state$birth_rates,
          death_rates = starting_state$death_rates,
          transmission_rates = starting_state$transmission_rates,
          infection_rates = starting_state$infection_rates,
          recovery_rates = starting_state$recovery_rates, # day^-1
          movement_rate = movement[[i]]
        ),
        dt = dt[i]+1, # add 1 because the first state is from the last day of previous month
        nsim = 1
      )
      
      final_state <- as.data.frame(model_output[[sim]][[i]][[1]][[dt[i]+1]])
      
      # remove the first state from month 2 as it is repeat of last state in month 1
      model_output[[sim]][[i]] <- list(model_output[[sim]][[i]][[1]][2:(dt[i]+1)])
      
    }
    
    if (length(movement) == 2) {
      model_output[[sim]] <- c(model_output[[sim]][[1]][[1]], model_output[[sim]][[2]][[1]])
    }
    if (length(movement) == 3) {
      model_output[[sim]] <- c(model_output[[sim]][[1]][[1]],
                               model_output[[sim]][[2]][[1]],
                               model_output[[sim]][[3]][[1]])
    }
    if (length(movement) == 12) {
      model_output[[sim]] <- c(model_output[[sim]][[1]][[1]],
                               model_output[[sim]][[2]][[1]],
                               model_output[[sim]][[3]][[1]],
                               model_output[[sim]][[4]][[1]],
                               model_output[[sim]][[5]][[1]],
                               model_output[[sim]][[6]][[1]],
                               model_output[[sim]][[7]][[1]],
                               model_output[[sim]][[8]][[1]],
                               model_output[[sim]][[9]][[1]],
                               model_output[[sim]][[10]][[1]],
                               model_output[[sim]][[11]][[1]],
                               model_output[[sim]][[12]][[1]])
    }
    
    
  }
  
  model_output
  
}


## Below are some additional previous functions for working with multipatchr

run_patch_model <- function(state_in, dt, nsim, seed = NULL) {
  
  
  out <- vector(mode = "list", length = nsim)
  ## Run multiple simulations.
  ## Each simulation outputs a list of length dt
  ## Each element in this list is a state object.
  for (sim in seq_len(nsim)) {
    this_sim <- vector(mode = "list", length = dt)
    this_sim[[1]] <- state_in
    if (dt > 1) {
      for (time in 2:dt) {
        this_sim[[time]] <- multipatchr::update_state(
          state = this_sim[[time - 1]],
          dt = 1#,
          #seed = seed,
        )
      }
    }
    
    out[[sim]] <- this_sim
    
  }
  
  out
  
}


## Each simulation a list of lists with the outer list being nsim runs
## and each element a collection of dt state objects
# simulation_as_df_orig <- function(simulation) {
#
#   out <- purrr::map_dfr(
#     simulation,
#     function(sim) {
#       purrr::map_dfr(sim, as.data.frame, .id = "time")
#     },
#     .id = "sim"
#   )
#   out$time <- as.integer(out$time)
#   out
# }

# much quicker version of the original function now commented out above

simulation_as_df <- function(simulation) {
  
  simulations <- lapply(simulation, function(x) {
    
    lapply(x, function(y) {
      
      y[["patches"]]
      
    })
  })
  
  out <- purrr::map_dfr(
    simulations,
    function(sim) {
      purrr::map_dfr(sim, function(sim) {
        x <- as.data.frame(do.call("rbind", sim))
        x$patch <- as.integer(rownames(x))
        x
      },
      .id = "time")
    },
    .id = "sim"
  )
  out$time <- as.integer(out$time)
  
  out <- pivot_longer(out,
                      cols = susceptible:recovery_rate,
                      names_to = "variable",
                      values_to = "value")
  out$value <- as.numeric(out$value)
  
  out
}

plot_simulation <- function(simulation,
                            vars = c(
                              "susceptible",
                              "exposed",
                              "infected",
                              "recovered")) {
  
  simulation <- dplyr::filter(simulation, variable %in% vars)
  # p <- ggplot(simulation, aes(time, value, col = variable, group = sim)) +
  #     geom_line(alpha = 0.3) +
  #     scale_color_manual(values = project_theme$seir_scale) +
  #     project_theme$theme +
  #     project_theme$legend +
  #     project_theme$axis_labels +
  #     facet_wrap(~patch)
  
  p <- ggplot(simulation, aes(time, value, col = variable, group = interaction(sim, variable))) +
    # geom_line(alpha = 0.3) +
    geom_line() +
    scale_color_manual(values = project_theme$seir_scale) +
    project_theme$theme +
    project_theme$legend +
    project_theme$axis_labels +
    facet_wrap(~patch, scales = "free")
  
  p
}


plot_infections <- function(simulation,
                            vars = c(
                              "susceptible",
                              "exposed",
                              "infected",
                              "recovered")) {
  
  simulation <- dplyr::filter(simulation, variable == "infected")
  # p <- ggplot(simulation, aes(time, value, col = variable, group = sim)) +
  #     geom_line(alpha = 0.3) +
  #     scale_color_manual(values = project_theme$seir_scale) +
  #     project_theme$theme +
  #     project_theme$legend +
  #     project_theme$axis_labels +
  #     facet_wrap(~patch)
  
  p <- ggplot(simulation, aes(time, value, col = variable, group = interaction(sim, variable))) +
    geom_line(alpha = 0.3) +
    geom_line() +
    scale_color_manual(values = project_theme$seir_scale) +
    project_theme$theme +
    project_theme$legend +
    project_theme$axis_labels +
    facet_wrap(~patch, scales = "free")
  
  p
}
