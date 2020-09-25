# TidyTuesday Plants in Danger - 18/08/2020 

threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

library(tidyverse)
library(sunburstR)

# General tidying up
data <- threats %>%
  filter(continent == "Europe" & threatened == 1) %>%
  select(-continent) 

data$year_last_seen <- str_replace_all(data$year_last_seen, "-", "/")


# Reformat data for the sunburstR package
data <- data %>%
  mutate(path = paste(country, group, binomial_name,red_list_category, 
                      threat_type, year_last_seen, sep="-")) %>%
  select(path, threatened)


# Plot
p <- sunburst(data, legend=FALSE)
p
