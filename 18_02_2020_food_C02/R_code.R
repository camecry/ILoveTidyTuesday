# Tidy Tuesday 18/02/2020
# Food Consumption and CO2 Emissions

# Get the Data
library(tidyverse)
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
glimpse(food)
# Only consumption data
food <-  food_consumption %>%
  select(-co2_emmission)

# Create three big food groups
food <-  food %>%  
  mutate(food_category_big = case_when(food_category %in% c("Beef","Fish","Lamb & Goat","Pork","Poultry") ~ "Meat",
                                    food_category %in% c("Eggs","Milk - inc. cheese") ~ "Eggs & Dairy",
                                    food_category %in% c("Nuts inc. Peanut Butter","Rice","Soybeans","Wheat and Wheat Products") ~ "Nuts & Grains"))

# Change category names for food_category variable
food <-food %>% 
  mutate(food_category = case_when(
    food_category == "Beef" ~ "Beef",
    food_category == "Fish" ~ "Fish",
    food_category == "Lamb & Goat" ~ "Lamb&Goat",
    food_category == "Pork" ~ "Pork",
    food_category == "Poultry" ~ "Poultry",
    food_category == "Eggs" ~ "Eggs",
    food_category == "Rice" ~ "Rice",
    food_category == "Soybeans" ~ "Boybeans",
    food_category == "Beef" ~ "Beef",
    food_category == "Milk - inc. cheese" ~ "Milk&Cheese",
    food_category == "Nuts inc. Peanut Butter" ~ "Nuts",
    food_category == "Wheat and Wheat Products" ~ "Wheat products"))
# Circlepacker package
library(circlepackeR)
devtools::install_github("jeromefroe/circlepackeR")
library(data.tree)
food$pathString <- paste("food", food$country, food$food_category_big, food$food_category ,sep = "/")
food_node <- as.Node(food)
p <- circlepackeR(food_node, size = "consumption")
p
##########################################################################################
# Only CO2 emmission data
co2 <-  food_consumption %>%
  select(-consumption)

# Create three big food groups
co2 <-  co2 %>%  
  mutate(food_category_big = case_when(food_category %in% c("Beef","Fish","Lamb & Goat","Pork","Poultry") ~ "Meat",
                                       food_category %in% c("Eggs","Milk - inc. cheese") ~ "Eggs & Dairy",
                                       food_category %in% c("Nuts inc. Peanut Butter","Rice","Soybeans","Wheat and Wheat Products") ~ "Nuts & Grains"))

# Change category names for food_category variable
co2 <-co2 %>% 
  mutate(food_category = case_when(
    food_category == "Beef" ~ "Beef",
    food_category == "Fish" ~ "Fish",
    food_category == "Lamb & Goat" ~ "Lamb&Goat",
    food_category == "Pork" ~ "Pork",
    food_category == "Poultry" ~ "Poultry",
    food_category == "Eggs" ~ "Eggs",
    food_category == "Rice" ~ "Rice",
    food_category == "Soybeans" ~ "Boybeans",
    food_category == "Beef" ~ "Beef",
    food_category == "Milk - inc. cheese" ~ "Milk&Cheese",
    food_category == "Nuts inc. Peanut Butter" ~ "Nuts",
    food_category == "Wheat and Wheat Products" ~ "Wheat products"))

# Circlepacker package
co2$pathString <- paste("co2", co2$country, co2$food_category_big, co2$food_category ,sep = "/")
co2_node <- as.Node(co2)
p1 <- circlepackeR(co2_node, size = "co2_emmission")
p1

# save the widget
library(htmlwidgets)
f<-"path/co2.html"
saveWidget(p1,file.path(normalizePath(dirname(f)),basename(f)))

