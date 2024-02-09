# setwd("Z:/jwardle/flight_spread/src/compare_attack_rates")
# orderly::orderly_develop_start(use_draft = TRUE, parameters = list(init_inf = 100))

attack_rates <- readRDS("attack_rates.rds")

# label which sims are for disrupted years

attack_rates <- attack_rates %>% 
  mutate(disruption = case_when(
    flight_scenario == "mers" & start_year == 2014 ~ "normal",
    flight_scenario == "mers" & start_year == 2015 ~ "disrupted",
    flight_scenario == "sarscov2" & start_year == 2019 ~ "normal",
    flight_scenario == "sarscov2" & start_year == 2020 ~ "disrupted",
    flight_scenario == "zika" & start_year == 2015 ~ "normal",
    flight_scenario == "zika" & start_year == 2016 ~ "disrupted",
  )
  )

# convert to wider format
all_wider <- attack_rates %>%
  select(patch, mean, beta, delta, gamma, init_inf, start_month, pathogen, flight_scenario, disruption) %>%
  pivot_wider(names_from = disruption,
              values_from = mean)

# Find differences in  arrival times for different scenarios

all_wider$diff <- all_wider$disrupted - all_wider$normal

all_wider$metric <- "attack_rates"

# Save dataframe of the differences for later compilation
saveRDS(all_wider, "peak_size_differences.rds")

# Create individual plot of the differences
# (In separate later task we will produce all panels at once)

all_wider$group <- "all"

# get the ranking of normal year times per param set
# across(matches()) terminology allows us to select the column that is named after the normal_year parameter of the task

all_wider1 <- all_wider %>% 
  filter((flight_scenario == "mers" & start_month == "may") | (flight_scenario == "sarscov2" & start_month == "jan") | (flight_scenario == "zika" & start_month == "feb"))

all_wider2 <- all_wider %>% 
  filter((flight_scenario == "mers" & start_month == "jun") | (flight_scenario == "sarscov2" & start_month == "feb") | (flight_scenario == "zika" & start_month == "mar"))

# all_wider <- all_wider1 %>% 
#   group_by(pathogen, flight_scenario) %>%
#   arrange(normal) %>%
#   mutate(rank = rank(normal, ties.method = "first"))

all_wider <- all_wider2 %>%
  group_by(pathogen, flight_scenario) %>%
  arrange(normal) %>%
  mutate(rank = rank(normal, ties.method = "first"))

# create stripchart
# as per http://www.sthda.com/english/wiki/ggplot2-stripchart-jitter-quick-start-guide-r-software-and-data-visualization

# pathogen and flight_scenario as factor

all_wider$pathogen <- factor(all_wider$pathogen,
                             levels = c("sarscov2", "mers", "zika"),
                             labels = c("Pathogen: SARS-CoV-2-like", "Pathogen: MERS-like", "Pathogen: Zika-like"))

all_wider$flight_scenario <- factor(all_wider$flight_scenario,
                                    levels = c("sarscov2", "mers", "zika"),
                                    labels = c("SARS-CoV-2", "MERS", "Zika"))

# Function to produce summary statistics (median and 95% CrI)
data_summary <- function(y) {
  m <- median(y, na.rm = T)
  ymin <- unname(quantile(y, 0.025, na.rm = T))
  ymax <- unname(quantile(y, 0.975, na.rm = T))
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# change colour of jitter points to represent bins of ~10 or 20 countries
all_wider$rank_bins <- factor(plyr::round_any(all_wider$rank, 10, f = ceiling),
                              levels = seq(10, 200, 10),
                              labels = seq(10, 200, 10))

# Put rankings in bins of 20 to reduce visual clutter
all_wider$rank_bins20 <- factor(plyr::round_any(all_wider$rank, 20, f = ceiling),
                                levels = seq(20, 200, 20),
                                labels = seq(20, 200, 20))

all_wider$rank_bins20 <- factor(plyr::round_any(all_wider$rank, 20, f = ceiling),
                                levels = c(seq(20, 200, 20), "All"),
                                labels = c(seq(20, 200, 20), "All"))

p8 <-
  all_wider %>%
  # filter(patch != "Yemen") %>%
  ggplot(aes(x = flight_scenario, y = diff)) +
  geom_jitter(aes(col = rank_bins20), position=position_dodge(0.8), cex=1.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~pathogen, nrow = 1) +
  stat_summary(fun.data=data_summary, color="black") +
  labs(y = "Difference in mean time to epidemic peak (days)",
       x = "Flight disruption scenario",
       col = "Invasion \nrank") +
  theme_classic() +
  theme(legend.title = element_text()) +
  scale_color_brewer(palette = "RdYlBu")
p8


## Create a sub plot of just the SAR-coV-2 flight and pathogen scenarios.

all_wider_repeat <- all_wider %>% 
  filter(pathogen == "Pathogen: SARS-CoV-2-like" & flight_scenario == "SARS-CoV-2")
all_wider_repeat$rank_bins20 <- "All"
all_wider_repeat$rank_bins20 <- factor(all_wider_repeat$rank_bins20,
                                       levels = c(seq(20, 200, 20), "All"),
                                       labels = c(seq(20, 200, 20), "All"))

all_wider_repeat$median <- median(all_wider_repeat$diff, na.rm = T)
all_wider_repeat$ymin <- quantile(all_wider_repeat$diff, 0.025, na.rm = T)
all_wider_repeat$ymax <- quantile(all_wider_repeat$diff, 0.975, na.rm = T)
all_wider_repeat$diff <- NA

plotting_data <- bind_rows(all_wider, all_wider_repeat)

coloured_plot <-
  plotting_data %>%
  filter(pathogen == "Pathogen: SARS-CoV-2-like" & flight_scenario == "SARS-CoV-2") %>%
  ggplot(aes(x = rank_bins20, y = diff)) +
  geom_jitter(aes(col = rank_bins20), position=position_dodge(0.8), cex=1.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_linerange(aes(x = rank_bins20, ymin = ymin, ymax = ymax)) +
  geom_point(aes(x = rank_bins20, y = median)) +
  labs(y = "Difference in mean time to epidemic peak (days)",
       x = "Invasion rank (top n countries)",
       col = "Invasion \nrank") +
  theme_classic() +
  theme(legend.title = element_text()) +
  scale_color_brewer(palette = "RdYlBu")
coloured_plot


black_white_plot <-
  plotting_data %>%
  filter(pathogen == "Pathogen: SARS-CoV-2-like" & flight_scenario == "SARS-CoV-2") %>%
  ggplot(aes(x = rank_bins20, y = diff)) +
  geom_jitter(position=position_dodge(0.8), cex=1.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_linerange(aes(x = rank_bins20, ymin = ymin, ymax = ymax), col = "red") +
  geom_point(aes(x = rank_bins20, y = median), col = "red") +
  labs(y = "Difference in mean epidemic attack rate\n(infections / 1000 people)",
       x = "Invasion rank group",
       col = "Invasion \nrank") +
  theme_classic() +
  theme(legend.title = element_text()) +
  scale_color_brewer(palette = "RdYlBu")
black_white_plot

ggsave("results_fig_attackrate_sarscov2.png", black_white_plot,
       width = 5, height = 5, units = "in",
       dpi = 600)

# black_white_plot <-
plotting_data %>%
  filter(pathogen == "Pathogen: SARS-CoV-2-like" & flight_scenario == "SARS-CoV-2") %>%
  ggplot(aes(x = rank, y = diff)) +
  geom_jitter(position=position_dodge(0.8), cex=1.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_linerange(aes(x = rank_bins20, ymin = ymin, ymax = ymax)) +
  geom_point(aes(x = rank_bins20, y = median)) +
  labs(y = "Difference in mean attack rate (infections / 1000 ppl)",
       x = "Invasion rank (top n countries)",
       col = "Invasion \nrank") +
  theme_classic() +
  theme(legend.title = element_text()) +
  scale_color_brewer(palette = "RdYlBu")
black_white_plot
