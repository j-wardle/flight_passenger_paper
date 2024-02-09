# setwd("Z:/jwardle/flight_spread/src/compare_invaded_countries")
# orderly::orderly_develop_start(use_draft = TRUE, parameters = list(init_inf = 100))

invaded_countries <- readRDS("invaded_countries.rds")

global_flight_changes <- readRDS("global_flight_changes.rds")
seed_location_flight_changes <- readRDS("seed_location_flight_changes.rds")

# label which sims are for disrupted years

invaded_countries <- invaded_countries %>% 
  mutate(disruption = case_when(
    flight_scenario == "mers" & start_year == 2014 ~ "normal",
    flight_scenario == "mers" & start_year == 2015 ~ "disrupted",
    flight_scenario == "sarscov2" & start_year == 2019 ~ "normal",
    flight_scenario == "sarscov2" & start_year == 2020 ~ "disrupted",
    flight_scenario == "zika" & start_year == 2015 ~ "normal",
    flight_scenario == "zika" & start_year == 2016 ~ "disrupted",
  )
  )

# update variables and values to be consistent with invaded_countries

process_flight_data <- function(flight_df) {
  
  flight_changes <- flight_df %>% 
    mutate(time = (id-0.5) * 365/12) %>% 
    rename(flight_scenario = pathogen) %>% 
    pivot_longer(cols = "year1":"year2",
                 names_to = "disruption",
                 values_to = "passengers") %>% 
    mutate(disruption = replace(disruption, disruption == "year1", "normal"),
           disruption = replace(disruption, disruption == "year2", "disrupted"),
           flight_scenario = replace(flight_scenario, flight_scenario == "sars", "sarscov2")) %>% 
    mutate(start_month = case_when(
      flight_scenario == "mers" & start_timing == 1 ~ "may",
      flight_scenario == "mers" & start_timing == 2 ~ "jun",
      flight_scenario == "sarscov2" & start_timing == 1 ~ "jan",
      flight_scenario == "sarscov2" & start_timing == 2 ~ "feb",
      flight_scenario == "zika" & start_timing == 1 ~ "feb",
      flight_scenario == "zika" & start_timing == 2 ~ "mar",
    )
    )
  
  flight_changes <- flight_changes[rep(1:nrow(flight_changes), times = 3),]
  flight_changes$pathogen <- rep(c("sarscov2", "mers", "zika"),
                                 each = nrow(flight_changes)/3)
  
  x <- bind_rows(invaded_countries, filter(flight_changes, id != 13))
  
  x$pathogen <- factor(x$pathogen,
                       levels = c("sarscov2", "mers", "zika"),
                       # labels = c("Pathogen: SARS-CoV-2-like", "Pathogen: MERS-like", "Pathogen: Zika-like"))
                       labels = c("Nat. history: SARS-CoV-2-like", "Nat. history: MERS-like", "Nat. history: Zika-like"))
  
  x$flight_scenario <- factor(x$flight_scenario,
                              levels = c("sarscov2", "mers", "zika"),
                              labels = c("Flights: SARS-CoV-2",
                                         "Flights: MERS",
                                         "Flights: Zika"))
  
  x
  
}

# Apply flight change function to global flight data and seed location flight data
seed_location_flight_changes <- process_flight_data(seed_location_flight_changes)

global_flight_changes <- process_flight_data(global_flight_changes)

# # Filter based on either month 1 or month 2
# 
# set1 <- x %>% 
#   filter((flight_scenario == "Flight scenario: MERS" & start_month == "may") | (flight_scenario == "Flight scenario: SARS-CoV-2" & start_month == "jan") | (flight_scenario == "Flight scenario: Zika" & start_month == "feb"))
# 
# set2 <- x %>% 
#   filter((flight_scenario == "mers" & start_month == "jun") | (flight_scenario == "sarscov2" & start_month == "feb") | (flight_scenario == "zika" & start_month == "mar"))



# Create scatter plot (x is time, y is no. countries invaded)

# global_flight_changes %>%
#   filter((flight_scenario == "Flight scenario: MERS" & start_month == "may") | (flight_scenario == "Flight scenario: SARS-CoV-2" & start_month == "jan") | (flight_scenario == "Flight scenario: Zika" & start_month == "feb")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.8) +
#   facet_grid(flight_scenario ~ pathogen) +
#   labs(y = "Number of countries with infections",
#        x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight data") +
#   scale_fill_discrete(name = "Flight data") +
#   theme(panel.grid.major = element_line(),
#         panel.grid.minor = element_line())
# 
# 
# global_flight_changes %>%
#   filter((flight_scenario == "Flight scenario: MERS" & start_month == "jun") | (flight_scenario == "Flight scenario: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flight scenario: Zika" & start_month == "mar")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.8) +
#   facet_grid(flight_scenario ~ pathogen) +
#   labs(y = "Number of countries with infections",
#        x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight data") +
#   scale_fill_discrete(name = "Flight data") +
#   theme(panel.grid.major = element_line(),
#         panel.grid.minor = element_line())



# Add additional layer with the flight data

# first scale the flight values

global_flight_changes$rel_passengers <- (global_flight_changes$passengers / max(global_flight_changes$passengers, na.rm = T)) * 200
seed_location_flight_changes$rel_passengers <- (seed_location_flight_changes$passengers / max(seed_location_flight_changes$passengers, na.rm = T)) * 200

global_flight_changes_b <- global_flight_changes %>% 
  group_by(flight_scenario) %>% 
  mutate(rel_passengers = (passengers / max(passengers, na.rm = T)) * 200)

# Limit seed-location cahnges to later set of months
seed_location_set2 <- seed_location_flight_changes %>% 
  filter((flight_scenario == "Flights: MERS" & start_month == "jun") | (flight_scenario == "Flights: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flights: Zika" & start_month == "mar"))

seed_location_set1 <- seed_location_flight_changes %>% 
  filter((flight_scenario == "Flights: MERS" & start_month == "may") | (flight_scenario == "Flights: SARS-CoV-2" & start_month == "jan") | (flight_scenario == "Flights: Zika" & start_month == "feb"))

global_flight_changes_b <- global_flight_changes_b %>% 
  mutate(disruption = case_when(disruption == "normal" ~ "historical",
                                disruption == "disrupted" ~ "contemporary"))

# Create plot ----
option1e <-
  global_flight_changes_b %>%
  filter((flight_scenario == "Flights: MERS" & start_month == "jun") | (flight_scenario == "Flights: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flights: Zika" & start_month == "mar")) %>% 
  ggplot() +
  geom_hline(yintercept = 100, linetype = 2, colour = 'grey', linewidth = 0.3) +
  geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
              alpha = 0.3) +
  geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.3) +
  geom_point(aes(x = time, y = (change*100) + 100),
             shape = 19, size = 0.7) +
  # col = 'black', linewidth = 0.2) +
  geom_point(data = seed_location_set2,
             aes(x = time, y = (change*100) + 100),
             shape = 1, size = 0.7) +
  # facet_grid(flight_scenario ~ pathogen, labeller = label_wrap_gen(width = 10)) +
  # facet_grid(flight_scenario ~ pathogen) +
  facet_grid(rows = vars(flight_scenario), cols = vars(pathogen), #scales = "free",
             labeller = label_wrap_gen(width = 14)) +
  labs(# y = "Number of countries with infections",
    y = "Number of countries with infections (lines)\nChange in departing passengers, % (points)",
    x = "Time (days)") +
  theme_classic() +
  scale_colour_discrete(name = "Flight data used in model") +
  scale_fill_discrete(name = "Flight data used in model") +
  theme_classic() +
  theme(strip.text = element_text(size = 7),
        # legend.position = "none",
        legend.position = "bottom",
        axis.text = element_text(size=7),
        axis.title = element_text(size=10))
# theme(panel.grid.major = element_line(),
#       panel.grid.minor = element_line())
option1e

ggsave("results_fig_invaded_countries_month_set_later_allflightchanges.png", option1e,
       width = 5, height = 5, units = "in",
       dpi = 600)
# 
#   
# option1 <- global_flight_changes %>%
#   filter((flight_scenario == "Flight scenario: MERS" & start_month == "jun") | (flight_scenario == "Flight scenario: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flight scenario: Zika" & start_month == "mar")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.8) +
#   geom_point(aes(x = time, y = rel_passengers, col = disruption)) +
#   facet_grid(flight_scenario ~ pathogen) +
#   labs(y = "Number of countries with infections",
#        x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight data") +
#   scale_fill_discrete(name = "Flight data") +
#   theme(panel.grid.major = element_line(),
#         panel.grid.minor = element_line())
# 
# option1b <- global_flight_changes_b %>%
#   filter((flight_scenario == "Flight scenario: MERS" & start_month == "jun") | (flight_scenario == "Flight scenario: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flight scenario: Zika" & start_month == "mar")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.8) +
#   geom_point(aes(x = time, y = rel_passengers, col = disruption)) +
#   facet_grid(flight_scenario ~ pathogen) +
#   labs(y = "Number of countries with infections",
#        x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight data") +
#   scale_fill_discrete(name = "Flight data") +
#   theme(panel.grid.major = element_line(),
#         panel.grid.minor = element_line())
# option1b
# 
# 
# 
# # option1c <-
#   global_flight_changes_b %>%
#   filter((flight_scenario == "Flight scenario: MERS" & start_month == "jun") | (flight_scenario == "Flight scenario: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flight scenario: Zika" & start_month == "mar")) %>% 
#   ggplot() +
#   geom_hline(yintercept = 100, linetype = 2) +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.8) +
#   geom_point(aes(x = time, y = rel_passengers, col = disruption)) +
#   geom_point(data = seed_location_set2,
#              aes(x = time, y = (change*100) + 100),
#              shape = 21) +
#   facet_grid(flight_scenario ~ pathogen) +
#   labs(y = "Number of countries with infections",
#        x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight data") +
#   scale_fill_discrete(name = "Flight data") +
#   theme(panel.grid.major = element_line(),
#         panel.grid.minor = element_line())
# option1c
# 
# option1d <- 
# global_flight_changes_b %>%
#   filter((flight_scenario == "Flights: MERS" & start_month == "jun") | (flight_scenario == "Flights: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flights: Zika" & start_month == "mar")) %>% 
#   ggplot() +
#   geom_hline(yintercept = 100, linetype = 2) +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.3) +
#   # geom_point(aes(x = time, y = rel_passengers, col = disruption)) +
#   geom_point(data = seed_location_set2,
#              aes(x = time, y = (change*100) + 100),
#              shape = 21, size = 0.6) +
#   facet_grid(flight_scenario ~ pathogen, labeller = label_wrap_gen(width = 10)) +
#   labs(# y = "Number of countries with infections",
#     y = "Number of countries with infections (lines)\n% flights in disrupted year (dots)",
#     x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight disruption") +
#   scale_fill_discrete(name = "Flight disruption") +
#   theme_classic()
#   # theme(panel.grid.major = element_line(),
#   #       panel.grid.minor = element_line())
# option1d
# 
# ggsave("results_fig_invaded_countries_month_set_later.png", option1d)
# 
# 
# 

# 
# option2a <- seed_location_flight_changes %>%
#   filter((flight_scenario == "Flight scenario: MERS" & start_month == "jun") | (flight_scenario == "Flight scenario: SARS-CoV-2" & start_month == "feb") | (flight_scenario == "Flight scenario: Zika" & start_month == "mar")) %>% 
#   ggplot() +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.8) +
#   geom_point(aes(x = time, y = rel_passengers, col = disruption)) +
#   facet_grid(flight_scenario ~ pathogen) +
#   labs(y = "Number of countries with infections",
#        x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight data") +
#   scale_fill_discrete(name = "Flight data") +
#   theme(panel.grid.major = element_line(),
#         panel.grid.minor = element_line())
# 
# 
# set2 %>%
#   ggplot() +
#   geom_ribbon(aes(x = time, ymin = `0.025`, ymax = `0.975`, fill = disruption),
#               alpha = 0.3) +
#   geom_line(aes(x = time, y = `0.5`, col = disruption), linewidth = 0.8) +
#   geom_point(aes(x = time, y = rel_passengers, col = disruption)) +
#   facet_grid(flight_scenario ~ pathogen) +
#   labs(y = "Number of countries with infections",
#        x = "Time (days)") +
#   theme_classic() +
#   scale_colour_discrete(name = "Flight data") +
#   scale_fill_discrete(name = "Flight data") +
#   theme(panel.grid.major = element_line(),
#         panel.grid.minor = element_line())
