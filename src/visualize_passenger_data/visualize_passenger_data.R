# setwd("Z:/jwardle/flight_spread/src/visualize_passenger_data")
# orderly::orderly_develop_start()

# Import IATA data

iata_all <- readRDS("iata_data.rds")

# Some discrepancies in naming between previously downloaded datasets and new downloads

iata_all <- iata_all %>%
  dplyr::mutate(orig_country = dplyr::case_when(
    orig_country == "Hong Kong (SAR), China" ~ "Hong Kong",
    orig_country == "Chinese Taipei" ~ "Taiwan",
    orig_country == "Macau (SAR), China" ~ "Macau",
    TRUE ~ orig_country
  )) %>%
  dplyr::mutate(dest_country = dplyr::case_when(
    dest_country == "Hong Kong (SAR), China" ~ "Hong Kong",
    dest_country == "Chinese Taipei" ~ "Taiwan",
    dest_country == "Macau (SAR), China" ~ "Macau",
    TRUE ~ dest_country
  ))


### Format and filter the data

iata_selection <- filter(iata_all, year < 2021)
iata_selection$year <- as.numeric(iata_selection$year)
iata_selection$month <- as.numeric(iata_selection$month)
iata_selection$month_sequence <- (iata_selection$year - 2012)*12 + iata_selection$month

iata_selection$month <- as.factor(iata_selection$month)
levels(iata_selection$month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

iata_selection$date <- as.factor(paste0(iata_selection$month, " ", iata_selection$year))
iata_selection$date <- fct_reorder(iata_selection$date, iata_selection$month_sequence, min)

iata_selection$date <- as.Date(paste0("01 ", iata_selection$date), format = "%d %b %Y")


#### SOME PLOT FUNCTIONS ----
my_theme <- theme(axis.text.x = element_text(angle = 45,
                                             # vjust = 0.5,
                                             hjust=1,
                                             element_text(size=7)),
                  axis.text = element_text(size=7),
                  axis.title = element_text(size=10),
                  plot.title = element_text(size = 10, face = "bold"),
                  panel.grid.minor.x = element_line(size = 0.5),
                  panel.grid.major.x = element_line(size = 1.1),
                  panel.grid.minor.y = element_blank(),
                  legend.position="none")


show_top_n <- 10


### TABLE OF THE PASSENGER NUMBERS

# All destinations
iata_table <- iata_selection %>%
  filter(orig_country != dest_country) %>%
  group_by(year, month, date, orig_country) %>%
  summarise(total_departures = sum(passengers))

### FIGURE 1

# Focus on South Korea ----
sk_table <- iata_table %>%
  filter(orig_country == "South Korea")

# Compare departures in each month to the departures in the equivalent month from the year before
sk_table$previous_departures <- lag(sk_table$total_departures, n = 12)
sk_table$rel_change <- 100 * (sk_table$total_departures - sk_table$previous_departures) / sk_table$previous_departures
sk_table$pos_neg <- ifelse(sk_table$rel_change < 0, "negative", "positive")

### Plot the South Korea changes

max_sk_depart <- max(sk_table$total_departures)

ylim.prim <- c(-100, 100)
ylim.sec <- c(0, max_sk_depart + 10000)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

sk_dual <-
  ggplot(sk_table) +
  geom_rect(aes(xmin = as.Date("2015-05-01"), xmax = as.Date("2015-07-01"),
                ymin = -100, ymax = 100), fill = "mistyrose2") +
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-01"),
                ymin = -100, ymax = 100), fill = "darkseagreen1") +
  geom_col(aes(date, rel_change, fill = pos_neg)) +
  geom_line(aes(date, y = a + total_departures*b), color = "black",
  ) +
  scale_y_continuous("% change", sec.axis = sec_axis(~ (. - a)/b, name = "# departing passengers")) +
  xlab("Date") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year",
               date_minor_breaks = "3 months") +
  ggtitle("Monthly departures from South Korea") +
  theme_bw() +
  my_theme

ggsave("sk_departures_dual.png", sk_dual,
       width = 5, height = 2.5, units = "in",
       dpi = 600)



# Focus on Brazil ----

bra_table <- iata_table %>%
  filter(orig_country == "Brazil")

# Compare departures in each month to the departures in the equivalent month from the year before
bra_table$previous_departures <- lag(bra_table$total_departures, n = 12)
bra_table$rel_change <- 100 * (bra_table$total_departures - bra_table$previous_departures) / bra_table$previous_departures
bra_table$pos_neg <- ifelse(bra_table$rel_change < 0, "negative", "positive")

### Plot the Brazil changes

max_bra_depart <- max(bra_table$total_departures)

ylim.prim <- c(-100, 100)
ylim.sec <- c(0, max_bra_depart + 10000)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


bra_dual <-
  ggplot(bra_table) +
  geom_rect(aes(xmin = as.Date("2016-02-01"), xmax = as.Date("2016-11-01"),
                ymin = -100, ymax = 100), fill = "lightgoldenrod1") +
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-01"),
                ymin = -100, ymax = 100), fill = "darkseagreen1") +
  geom_col(aes(date, rel_change, fill = pos_neg)) +
  geom_line(aes(date, y = a + total_departures*b), color = "black",
  ) +
  scale_y_continuous("% change", sec.axis = sec_axis(~ (. - a)/b, name = "# departing passengers")) +
  xlab("Date") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year",
               date_minor_breaks = "3 months") +
  ggtitle("Monthly departures from Brazil") +
  theme_bw() +
  my_theme

ggsave("bra_departures_dual.png", bra_dual,
       width = 5, height = 2.5, units = "in",
       dpi = 600)


# Focus on China ----

chi_table <- iata_table %>%
  filter(orig_country == "China")

# Compare departures in each month to the departures in the equivalent month from the year before
chi_table$previous_departures <- lag(chi_table$total_departures, n = 12)
chi_table$rel_change <- 100 * (chi_table$total_departures - chi_table$previous_departures) / chi_table$previous_departures
chi_table$pos_neg <- ifelse(chi_table$rel_change < 0, "negative", "positive")

### Plot the China changes

max_chi_depart <- max(chi_table$total_departures)

ylim.prim <- c(-100, 100)
ylim.sec <- c(0, max_chi_depart + 10000)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


chi_dual <-
  ggplot(chi_table) +
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-01"),
                ymin = -100, ymax = 100), fill = "darkseagreen1") +
  geom_col(aes(date, rel_change, fill = pos_neg)) +
  geom_line(aes(date, y = a + total_departures*b), color = "black",
  ) +
  scale_y_continuous("% change", sec.axis = sec_axis(~ (. - a)/b, name = "# departing passengers")) +
  xlab("Date") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year",
               date_minor_breaks = "3 months") +
  ggtitle("Monthly departures from China") +
  theme_bw() +
  my_theme

ggsave("chi_departures_dual.png", chi_dual,
       width = 5, height = 2.5, units = "in",
       dpi = 600)


# Focus on all flights ----

global_table <- iata_selection %>%
  filter(orig_country != dest_country) %>%
  group_by(year, month, date) %>%
  summarise(total_departures = sum(passengers))

# Compare departures in each month to the departures in the equivalent month from the year before
global_table$previous_departures <- lag(global_table$total_departures, n = 12)
global_table$rel_change <- 100 * (global_table$total_departures - global_table$previous_departures) / global_table$previous_departures
global_table$pos_neg <- ifelse(global_table$rel_change < 0, "negative", "positive")

### Plot the global changes

max_globe_depart <- max(global_table$total_departures)

ylim.prim <- c(-100, 100)
ylim.sec <- c(0, max_globe_depart + 10000)

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


global_dual <-
  ggplot(global_table) +
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-01"),
                ymin = -100, ymax = 100), fill = "darkseagreen1") +
  geom_col(aes(date, rel_change, fill = pos_neg)) +
  geom_line(aes(date, y = a + total_departures*b), color = "black",
  ) +
  scale_y_continuous("% change", sec.axis = sec_axis(~ (. - a)/b, name = "# departing passengers")) +
  xlab("Date") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year",
               date_minor_breaks = "3 months") +
  ggtitle("Monthly departures (global total)") +
  theme_bw() +
  my_theme

ggsave("global_departures_dual.png", global_dual,
       width = 5, height = 2.5, units = "in",
       dpi = 600)


### DESTINATION BUMP PLOTS (FIGURE 2) ----

iata_selection$patch_3letter <- countrycode::countrycode(iata_selection[[4]],
                                                         origin = "country.name",
                                                         destination = "iso3c")

my_theme <- function() {
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,
                                   # vjust = 0.5,
                                   hjust=1,
                                   element_text(size=7)),
        axis.text = element_text(size=7),
        axis.title = element_text(size=10),
        plot.title = element_text(size = 10, face = "bold"),
        panel.grid.minor.x = element_blank(),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="none")
}

show_top_n <- 10

monthly_departure_dests <- function(country, months_to_plot) {

  plot_data <- filter(iata_selection, orig_country == country & dest_country != country & month %in% months_to_plot)
  plot_rankings <- plot_data %>%
    group_by(month_sequence) %>%
    arrange(month_sequence, desc(passengers), dest_country) %>%
    mutate(ranking = row_number()) %>%
    as.data.frame()

  if (months_to_plot == "Jun") month_title <- "June"
  if (months_to_plot == "Mar") month_title <- "March"
  if (months_to_plot == "Feb") month_title <- "February"

  if(country == "South Korea") {
    outbreaks1 <- geom_rect(aes(xmin = 2015 - 0.5, xmax = 2015 + 0.5,
                               ymin = 10.5, ymax = 0.5), fill = "mistyrose2")
    outbreaks2 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                    ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
  }

  if(country == "Brazil") {
    outbreaks1 <- geom_rect(aes(xmin = 2016 - 0.5, xmax = 2016 + 0.5,
                                ymin = -100, ymax = 100), fill = "lightgoldenrod1")
    outbreaks2 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
  }

  if(country == "China") {
    outbreaks1 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
    outbreaks2 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
  }


  plot <- ggplot(data = plot_rankings, aes(x = year, y = ranking, group = dest_country)) +
    outbreaks1 +
    outbreaks2 +
    geom_line(aes(color = dest_country, alpha = 1), size = 0.8) +
    geom_point(aes(color = dest_country, alpha = 1), size = 2) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse(breaks = show_top_n:1) +
    scale_x_continuous(breaks = 2012:2020, minor_breaks = 2012:2020, expand = c(.2, .2)) +
    geom_text(data = plot_rankings %>% filter(year == min(year)),
              aes(x = year - 1, label = patch_3letter),
              size = 2, hjust = 1) +
    geom_text(data = plot_rankings %>% filter(year == max(year)),
              aes(x = year + 1, label = patch_3letter),
              size = 2, hjust = 0) +
    coord_cartesian(ylim = c(show_top_n,1)) +
    theme(legend.position = "none") +
    labs(x = "Year",
         y = "Rank",
         title = paste0("Destinations from ", country, " in ", month_title)#,
         # subtitle = "Countries ranked by monthly passengers"
    ) +
    my_theme()

  print(plot)

}

sk_dest <-
  monthly_departure_dests("South Korea",
                          "Jun")

ggsave("sk_dests.png", sk_dest,
       width = 5, height = 2.5, units = "in",
       dpi = 600)

bra_dest <-
  monthly_departure_dests("Brazil",
                          "Mar")

ggsave("brazil_dests.png", bra_dest,
       width = 5, height = 2.5, units = "in",
       dpi = 600)

chi_dest <-
  monthly_departure_dests("China",
                          "Feb")

ggsave("chi_dests.png", chi_dest,
       width = 5, height = 2.5, units = "in",
       dpi = 600)

# Alternative plots with additional detail
# variations in point and line style represent countries that feature in all or some of the years

mycolors <-  c(brewer.pal(name="Paired", n = 12), brewer.pal(name="Dark2", n = 8))
mycolors <- mycolors[mycolors != "#FFFF99"]

monthly_departure_dests2 <- function(country, months_to_plot) {

  plot_data <- filter(iata_selection, orig_country == country & dest_country != country & month %in% months_to_plot)
  plot_rankings <- plot_data %>%
    group_by(month_sequence) %>%
    arrange(month_sequence, desc(passengers), dest_country) %>%
    mutate(ranking = row_number()) %>%
    mutate(toptenindic = ifelse(ranking < 11, 1, 0)) %>%
    as.data.frame()

  plot_rankings <- plot_rankings %>%
    ungroup() %>%
    group_by(dest_country) %>%
    mutate(years_present = sum(toptenindic),
           years_present_excl20 = sum(toptenindic[year < 2020]))

  plot_rankings$presence <- ifelse(plot_rankings$years_present == 9, "All",
                                   ifelse(plot_rankings$years_present == 8 & plot_rankings$years_present_excl20 == 8, "All except 2020",
                                          ifelse(plot_rankings$years_present > 0, "Some", NA)
                                   ))

  plot_rankings <- filter(plot_rankings, !(is.na(presence)))

  if (months_to_plot == "Jun") month_title <- "June"
  if (months_to_plot == "Mar") month_title <- "March"
  if (months_to_plot == "Feb") month_title <- "February"

  if(country == "South Korea") {
    outbreaks1 <- geom_rect(aes(xmin = 2015 - 0.5, xmax = 2015 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "mistyrose2")
    outbreaks2 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
  }

  if(country == "Brazil") {
    outbreaks1 <- geom_rect(aes(xmin = 2016 - 0.5, xmax = 2016 + 0.5,
                                ymin = -100, ymax = 100), fill = "lightgoldenrod1")
    outbreaks2 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
  }

  if(country == "China") {
    outbreaks1 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
    outbreaks2 <- geom_rect(aes(xmin = 2020 - 0.5, xmax = 2020 + 0.5,
                                ymin = 10.5, ymax = 0.5), fill = "darkseagreen1")
  }

  ggplot(data = plot_rankings, aes(x = year, y = ranking, group = dest_country)) +
    outbreaks1 +
    outbreaks2 +
    geom_line(aes(color = dest_country, linetype = presence)) +
    geom_point(aes(color = dest_country, shape = presence), size = 2) +
    # geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse(breaks = show_top_n:1) +
    scale_x_continuous(breaks = 2012:2020, minor_breaks = 2012:2020, expand = c(.2, .2)) +
    scale_shape_manual(values = c(19, 1, 4)) +
    scale_color_manual(values = mycolors) +
    scale_linetype_manual(values = c("All" = "solid", "All except 2020" = "dotted", "Some" = "dashed")) +
    geom_text(data = plot_rankings %>% filter(year == min(year)),
              aes(x = year - 1, label = patch_3letter),
              size = 2, hjust = 1) +
    geom_text(data = plot_rankings %>% filter(year == max(year)),
              aes(x = year + 1, label = patch_3letter),
              size = 2, hjust = 0) +
    coord_cartesian(ylim = c(show_top_n,1)) +
    theme(legend.position = "none") +
    labs(x = "Year",
         y = "Rank",
         title = paste0("Destinations from ", country, " in ", month_title, " 2012-2020")#,
         # subtitle = "Countries ranked by monthly passengers"
    ) +
    my_theme()

}

sk_dest <-
  monthly_departure_dests2("South Korea",
                          "Jun")

bra_dest <-
  monthly_departure_dests2("Brazil",
                           "Mar")

chi_dest <-
  monthly_departure_dests2("China",
                           "Feb")

ggsave("sk_dests2.png", sk_dest,
       width = 5, height = 2.5, units = "in",
       dpi = 600)

ggsave("bra_dests2.png", bra_dest,
       width = 5, height = 2.5, units = "in",
       dpi = 600)

ggsave("chi_dests2.png", chi_dest,
       width = 5, height = 2.5, units = "in",
       dpi = 600)
