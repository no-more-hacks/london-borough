


library(tidyverse)
library(lubridate)

# load in the downloaded CSV file
# corrections for column names
raw <- read_csv("c:/source/github/london-borough/google_activity_by_London_Borough.csv") %>%
  rename(id=`...1`)


# check up on the data:
# id is unique?
raw %>% group_by(id) %>% tally() %>% filter(n!=1)

# # area id is unique for area name
raw %>% group_by(area_name) %>% summarise(n_code = n_distinct(area_code)) %>% filter(n_code!=1)


# de-pivot the data data
# this is called "tidy data" in the R idiom
tidy <- raw %>% 
  pivot_longer(cols = matches("percent_change"), names_to = "metric_name", values_to = "metric_value")


# check the data is well-structured:

# check if all the areas have all the metrics:
check_metric_count <- tidy %>% 
  group_by(area_name, date) %>% 
  filter(!is.na(metric_value), !is_null(metric_value)) %>%
  summarise(n_metric = n_distinct(metric_name))

check_metric_count %>% filter(n_metric != 6)


# check values are all sensible:
tidy %>% 
  filter(abs(metric_value) > 100)
# there are some large numbers here, something to look at

