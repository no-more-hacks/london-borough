


library(tidyverse)
library(lubridate)

#' load in the downloaded CSV file
#' corrections for column names
raw <- read_csv("google_activity_by_London_Borough.csv") %>%
  rename(id=`...1`)

neat_metrics <- c("retail & recreation" = "retail_and_recreation",
  "grocery & pharmacy" = "grocery_and_pharmacy",
  "parks" = "parks",
  "transit stations" = "transit_stations",
  "workplaces" = "workplaces",
  "residential" = "residential")

neat_metrics_colours <- c("retail_and_recreation" = "purple",
                  "grocery_and_pharmacy" = "goldenrod",
                  "parks" = "#880000",
                  "transit_stations" = "blue",
                  "workplaces" = "black",
                  "residential" = "darkgreen")

neat_seasons <- tribble(~month, ~season, 
                        1, "winter",
                        2, "winter",
                        3, "spring",
                        4, "spring",
                        5, "spring",
                        6, "summer",
                        7, "summer",
                        8, "summer",
                        9, "autumn",
                        10, "autumn",
                        11, "autumn",
                        12, "winter")


#' ## check up on the data:
#' ### id is unique?
raw %>% group_by(id) %>% tally() %>% filter(n!=1)

#' ### check area id is unique for area name
raw %>% group_by(area_name) %>% summarise(n_code = n_distinct(area_code)) %>% filter(n_code!=1)


#' # de-pivot the data data
#' this is called "tidy data" in the R idiom
tidy <- raw %>% 
  pivot_longer(cols = matches("percent_change"), names_to = "metric_name", values_to = "metric_value")  %>% 
  mutate(metric_name = str_replace_all(metric_name, pattern = "_percent_change_from_baseline", replacement = "")) %>% 
  #mutate(metric_name = str_replace_all(metric_name, pattern = "_", replacement = " ")) %>%
  #mutate(metric_name = str_replace_all(metric_name, pattern = "and", replacement = "&")) %>% 
  {.}


#' # check the long data is well-structured:
#' ## missing metrics
#' check if all the areas have all the metrics:
check_metric_count <- tidy %>% 
  group_by(area_name, date) %>% 
  filter(!is.na(metric_value), !is_null(metric_value)) %>%
  summarise(n_metric = n_distinct(metric_name))

#' can see that some of the date/areas don't have all the data in them
check_metric_count %>% filter(n_metric != 6)

#' see if there's a time effect:
check_metric_count %>%  ggplot(aes(x = date, y = n_metric, colour = area_name)) +
  geom_line()

#' no time trend at least, so that's good 
#' hard to read that chart, too much noise, so lets summarise again:
check_metric_count %>% 
  group_by(area_name) %>% 
  summarise(n_metric = mean(n_metric)) %>%
  mutate(diff = abs(n_metric-6)) %>%
  # could do something better here, using a standard deviation or something
  filter(diff > 0.1) %>% 
  arrange(diff)

# We see that the City of London and Kingston Upon Thames are unusual in some way

#' ## large values for percentages?
#' check values are all sensible, expect them to all be percentages:
tidy %>% 
  filter(abs(metric_value) > 100)
#' there are some large numbers here, something to look at!


