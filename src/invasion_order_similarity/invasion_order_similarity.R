# setwd("Z:/jwardle/flight_spread/src/invasion_order_similarity")
# orderly::orderly_develop_start(use_draft = TRUE, parameters = list(init_inf = 100))

invasion_order <- readRDS("invasion_order.rds")

# label which sims are for disrupted years

invasion_order <- invasion_order %>%
  mutate(disruption = case_when(
    flight_scenario == "mers" & start_year == 2014 ~ "normal",
    flight_scenario == "mers" & start_year == 2015 ~ "disrupted",
    flight_scenario == "sarscov2" & start_year == 2019 ~ "normal",
    flight_scenario == "sarscov2" & start_year == 2020 ~ "disrupted",
    flight_scenario == "zika" & start_year == 2015 ~ "normal",
    flight_scenario == "zika" & start_year == 2016 ~ "disrupted",
  )
  )

scenarios <- merge(data.frame(pathogen = c("mers", "sarscov2", "zika")),
                   data.frame(flights = c("mers", "sarscov2", "zika"),
                              seed = c("South Korea", "China", "Brazil"),
                              starting_month = c("jun", "feb", "mar")))

order_success <- map_dfr(1:(nrow(scenarios)), function(i) {

  out <- map_dfr(1:100, function(patch_order) {

    compare_patches(invasion_order, scenarios$pathogen[i], scenarios$flights[i],
                    start = scenarios$starting_month[i],
                    seed_location = scenarios$starting_month[i],
                    n = patch_order)

  }, .id = "invasion_order")

  out$pathogen <- scenarios$pathogen[i]
  out$flights <- scenarios$flights[i]
  out

})

order_success$pathogen <- factor(order_success$pathogen,
                                 levels = c("sarscov2", "mers", "zika"),
                                 labels = c("Nat. history: SARS-CoV-2-like", "Nat. history: MERS-like", "Nat. history: Zika-like"))

order_success$flights <- factor(order_success$flights,
                                levels = c("sarscov2", "mers", "zika"),
                                labels = c("Flights: SARS-CoV-2", "Flights: MERS", "Flights: Zika"))


order_success_plot <-
  order_success %>%
  ggplot() +
  geom_line(aes(x = as.numeric(invasion_order), y = 100*match_prop)) +
  xlab(expression(paste("First ",
                        italic("n"),
                        " countries invaded using contemporary flight data"))) +
  ylab(expression(paste("% of ",
                        italic("n"), " identified using historical flight data"))) +
  scale_x_continuous(limits = c(0,100),
                     breaks = seq(0, 100, length = 6)) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0, 100, length = 6)) +
  facet_grid(rows = vars(flights), cols = vars(pathogen), #scales = "free",
             labeller = label_wrap_gen(width = 14)) +
  theme_classic() +
  theme(strip.text = element_text(size = 7),
        legend.position = "none",
        axis.text = element_text(size=7),
        axis.title = element_text(size=10),
        panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_line(size = 0.5))

# Save Figure 5 - order success plot
ggsave("order_success_plot.png", order_success_plot,
       width = 5, height = 5, units = "in",
       dpi = 600)

ggsave("order_success_plot.jpg", order_success_plot,
       width = 5, height = 5, units = "in",
       dpi = 600)
