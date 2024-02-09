# setwd("Z:/jwardle/flight_spread/src/compare_arrival_times")
# orderly::orderly_develop_start(use_draft = TRUE, parameters = list(init_inf = 100))

arrival_times <- readRDS("arrival_times.rds")

# label which sims are for disrupted years

arrival_times <- arrival_times %>%
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
all_wider <- arrival_times %>%
  select(patch, mean, beta, delta, gamma, init_inf, start_month, pathogen, flight_scenario, disruption) %>%
  pivot_wider(names_from = disruption,
              values_from = mean)

# Find differences in  arrival times for different scenarios

all_wider$diff <- all_wider$disrupted - all_wider$normal

all_wider$metric <- "arrival_times"

# Save dataframe of the differences for any later compilation
saveRDS(all_wider, "arrival_differences.rds")

# Create individual plot of the differences

all_wider$group <- "all"

# For working with earliest start months
all_wider1 <- all_wider %>%
  filter((flight_scenario == "mers" & start_month == "may") | (flight_scenario == "sarscov2" & start_month == "jan") | (flight_scenario == "zika" & start_month == "feb")) %>%
  filter(normal != 1)

# For working with later set of start months
all_wider2 <- all_wider %>%
  filter((flight_scenario == "mers" & start_month == "jun") | (flight_scenario == "sarscov2" & start_month == "feb") | (flight_scenario == "zika" & start_month == "mar")) %>%
  filter(normal != 1)

# Function to produce summary statistics (median and 95% CrI)
data_summary <- function(y) {
  m <- median(y, na.rm = T)
  ymin <- unname(quantile(y, 0.025, na.rm = T))
  ymax <- unname(quantile(y, 0.975, na.rm = T))
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# Function to process and chart wide data input
create_chart <- function(df) {

  # get the ranking of normal year times per param set
  all_wider <- df %>%
    group_by(pathogen, flight_scenario) %>%
    arrange(normal) %>%
    mutate(rank = rank(normal, ties.method = "first"))

  # create stripchart
  # as per http://www.sthda.com/english/wiki/ggplot2-stripchart-jitter-quick-start-guide-r-software-and-data-visualization

  # pathogen and flight_scenario as factor

  all_wider$pathogen <- factor(all_wider$pathogen,
                               levels = c("sarscov2", "mers", "zika"),
                               labels = c("Nat. history: SARS-CoV-2-like", "Nat. history: MERS-like", "Nat. history: Zika-like"))

  all_wider$flight_scenario <- factor(all_wider$flight_scenario,
                                      levels = c("sarscov2", "mers", "zika"),
                                      labels = c("SARS-CoV-2", "MERS", "Zika"))

  # Put rankings in bins of 20 to reduce visual clutter
  all_wider$rank_bins20 <- factor(plyr::round_any(all_wider$rank, 20, f = ceiling),
                                  levels = seq(20, 200, 20),
                                  labels = seq(20, 200, 20))

  # This plot style doesn't use subsetted destinations. But it splits the invasion ranks more clearly

  all_wider %>%
    filter(as.integer(rank_bins20) < 6) %>%
    # filter(patch != "Yemen") %>%
    ggplot(aes(x = flight_scenario, y = diff)) +
    geom_hline(yintercept = 0, linetype = 2,
               color = "grey") +
    stat_summary(fun.data=data_summary, color="black", size = 0.1,
                 shape = 5,
                 position = position_nudge(x = 0.4)
                 ) +
    geom_jitter(aes(col = rank_bins20),
                position=position_dodge(0.7),
                size = 0.4,
                shape = 19) +
    facet_wrap(~pathogen, nrow = 1, labeller = label_wrap_gen(width = 14)) +
    labs(y = "Difference in mean time to\n10 cumulative infections (days)",
         x = "Flight scenario",
         col = "Invasion \nrank") +
    theme_classic() +
    theme(legend.title = element_text(size = 7),
          legend.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 7),
          axis.text = element_text(size=7),
          axis.title = element_text(size=10)
          ) #+
    # scale_color_brewer(palette = "YlOrRd")
    # scale_colour_manual(values = rev(colours))

}


# plot of model using flight data from month after outbreak start
p1 <- create_chart(all_wider2)


# For values in text:
test <- all_wider %>%
  filter(as.integer(rank_bins20) < 6)

data_summary(test$diff[test$pathogen == "Pathogen: MERS-like" &
                         test$flight_scenario == "MERS"])

data_summary(test$diff[test$pathogen == "Pathogen: MERS-like" &
                         test$flight_scenario == "Zika"])


data_summary(test$diff[test$pathogen == "Pathogen: SARS-CoV-2-like" &
                         test$flight_scenario == "SARS-CoV-2"])

data_summary(test$diff[test$pathogen == "Pathogen: MERS-like" &
                         test$flight_scenario == "SARS-CoV-2"])

data_summary(test$diff[test$pathogen == "Pathogen: Zika-like" &
                         test$flight_scenario == "SARS-CoV-2"])

# save figures ----
ggsave("results_fig_time_to_epidemic_arrival_month_set_later.png", p1,
       width = 5, height = 5, units = "in",
       dpi = 600)

ggsave("results_fig_time_to_epidemic_arrival_month_set_later.jpg", p1,
       width = 5, height = 5, units = "in",
       dpi = 600)
