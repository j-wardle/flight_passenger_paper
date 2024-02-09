# setwd("~/iata_analysis/flight_spread/src/compare_invasion_order")
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

invasion_order$pathogen <- factor(invasion_order$pathogen,
                                  levels = c("sarscov2", "mers", "zika"),
                                  labels = c("Pathogen: SARS-CoV-2-like", "Pathogen: MERS-like", "Pathogen: Zika-like"))

invasion_order$flight_scenario <- factor(invasion_order$flight_scenario,
                                         levels = c("sarscov2", "mers", "zika"),
                                         labels = c("Flights: SARS-CoV-2", "Flights: MERS", "Flights: Zika"))



# convert to wider format
all_wider <- invasion_order %>%
  # select(patch, mean, order, `0.5`, `0.025`, `0.975`, start_month, pathogen, flight_scenario, disruption) %>%
  select(-start_year) %>% 
  pivot_wider(names_from = disruption,
              values_from = c(mean, order, `0.5`, `0.025`, `0.975`))

# Find differences in  arrival times for different scenarios

all_wider$diff <- all_wider$mean_disrupted - all_wider$mean_normal

all_wider$metric <- "invasion_order"

# Save dataframe of the differences for later compilation
saveRDS(all_wider, "invasion_order_differences.rds")


# Create individual plot of the differences
# (In separate later task we will produce all panels at once)

# change colour of jitter points to represent bins of ~10 or 20 countries

all_wider$rank_bins <- factor(plyr::round_any(all_wider$order_normal, 10, f = ceiling),
                              levels = seq(10, 200, 10),
                              labels = seq(10, 200, 10))

# Put rankings in bins of 20 to reduce visual clutter
all_wider$rank_bins20 <- factor(plyr::round_any(all_wider$order_normal, 20, f = ceiling),
                                levels = seq(20, 200, 20),
                                labels = seq(20, 200, 20))

# # Define the number of colors you want
# nb.cols <- 20
# mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)

# extend colour scheme
# nb.colsp6 <- 10
# mycolorsp6 <- colorRampPalette(brewer.pal(9, "YlOrRd"))(nb.colsp6)

### Alternative plot: just show changes in invasion order for the early invaded countries

# Select the first 10 countries invaded (outside of seed) for each non-disrupted scenario

first_invaded <- all_wider %>% 
  filter(order_normal < 12 & order_normal > 1)

# switch back to long format so that we can use disruption category to state shape
# https://community.rstudio.com/t/pivot-longer-on-multiple-column-sets-pairs/43958/9

first_invaded_long <- first_invaded %>%
  pivot_longer(`0.5_normal`:`0.975_disrupted`,
               names_to = c(".value", "disruption"),
               names_pattern = "(.+)_(.+)")

first_invaded_long$order_normal <- as.factor(first_invaded_long$order_normal - 1)

first_invaded_long$disruption <- factor(first_invaded_long$disruption,
                                        levels = c("normal", "disrupted"),
                                        labels = c("normal", "disrupted"))

first_invaded_long$patch_3letter <- countrycode::countrycode(first_invaded_long[[1]],
                                                         origin = "country.name",
                                                         destination = "iso3c")

first_invaded_long <- first_invaded_long %>% 
  select(patch, patch_3letter, everything()) %>% 
  mutate(patch_3letter = ifelse(disruption == "disrupted", patch_3letter, NA))

first_invaded_long <- first_invaded_long %>% 
  group_by(pathogen, flight_scenario, order_normal, start_month) %>% 
  mutate(label_shift = max(`0.975`) + 1)

cols <- c("normal" = "#00BFC4", "disrupted" = "#F8766D")

# create plot for the first set of seed months  
p1 <-
  first_invaded_long %>%
  filter((flight_scenario == "Flights: MERS" & start_month == "may") | (flight_scenario == "Flights: SARS-CoV-2" & start_month == "jan") | (flight_scenario == "Flights: Zika" & start_month == "feb")) %>% 
  ggplot() +
  geom_pointrange(aes(x = order_normal, y = `0.5`, ymin = `0.025`, ymax = `0.975`,
                      shape = disruption, col = disruption),
                  size = 0.2, position = position_dodge(width = 0.7)) +
  labs(y = "Invasion rank",
       x = "First 10 countries invaded in the\nnon-disrupted version of the flight scenario",
       col = "Flight disruption",
       shape = "Flight disruption") +
  scale_color_manual(values = cols) +
  facet_grid(rows = vars(flight_scenario), cols = vars(pathogen), scales = "free",
             labeller = label_wrap_gen(width = 10)) +
  # facet_wrap(~flight_scenario + pathogen, scales = "free") +
  theme_classic() +
  theme(strip.text = element_text(size = 7),
        legend.position = "none",
        axis.text = element_text(size=7),
        axis.title = element_text(size=10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1

ggsave("results_fig_invasion_order_month_set_early.png", p1,
       width = 5, height = 5, units = "in",
       dpi = 600)
# png("results_fig_invasion_order_month_set_early2.png")
# print(p1)
# dev.off()

# create plot for the second set of seed months
p2 <- 
  first_invaded_long %>%
  filter((flight_scenario == "Flights: MERS" & start_month == "jun") | (flight_scenario == "Flights: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flights: Zika" & start_month == "mar")) %>% 
  ggplot() +
  geom_pointrange(aes(x = order_normal, y = `0.5`, ymin = `0.025`, ymax = `0.975`,
                      shape = disruption, col = disruption),
                  size = 0.2, position = position_dodge(width = 0.7)) +
  labs(y = "Distribution of invasion ranks across simulations\n(median and 95% CrI)",
       x = "Countries with the ten lowest median invasion ranks\n(in status quo version of the flight scenario)",
       col = "Flight disruption",
       shape = "Flight disruption") +
  scale_color_manual(values = cols) +
  facet_grid(rows = vars(flight_scenario), cols = vars(pathogen), scales = "free") +#,
             # labeller = label_wrap_gen(width = 10)) +
  # facet_wrap(~flight_scenario + pathogen, scales = "free") +
  theme_classic() +
  theme(strip.text = element_text(size = 7),
        legend.position = "none",
        axis.text = element_text(size=7),
        axis.title = element_text(size=10),
        # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)
        )
p2

ggsave("results_fig_invasion_order_month_set_later.png", p2,
       width = 5, height = 5, units = "in",
       dpi = 600)



p2a <-
  first_invaded_long %>%
  filter((flight_scenario == "Flights: MERS" & start_month == "jun") | (flight_scenario == "Flights: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flights: Zika" & start_month == "mar")) %>% 
  ggplot() +
  geom_pointrange(aes(x = order_normal, y = `0.5`, ymin = `0.025`, ymax = `0.975`,
                      shape = disruption, col = disruption),
                  size = 0.2, position = position_dodge(width = 0.7)) +
  geom_text(aes(x = order_normal, y = label_shift, label = patch_3letter),
           # vjust  = -0.2,
           size = 1.5) +
  labs(y = "Distribution of invasion ranks across simulations\n(median and 95% CrI)",
       x = "Countries with the ten lowest median invasion ranks\n(in the status quo version of the flight scenario)",
       col = "Flight disruption",
       shape = "Flight disruption") +
  scale_color_manual(values = cols) +
  facet_grid(rows = vars(flight_scenario), cols = vars(pathogen), scales = "free") +#,
  # labeller = label_wrap_gen(width = 10)) +
  # facet_wrap(~flight_scenario + pathogen, scales = "free") +
  theme_classic() +
  theme(strip.text = element_text(size = 7),
        legend.position = "none",
        axis.text = element_text(size=7),
        axis.title = element_text(size=10),
        # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)
  )
p2a

ggsave("results_fig_invasion_order_month_set_later_country_labels.png", p2a,
       width = 5, height = 5, units = "in",
       dpi = 600)

# png("results_fig_invasion_order_month_set_later2.png")
# print(p2)
dev.off()
