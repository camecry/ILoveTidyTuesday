# Tidy Tuesday: 28/01/2020 
# San Francisco trees
library(tidyverse)
library(gganimate)
library(gifski)
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')


# separate species into "scientific name"& "common name"
sf_trees1 <- sf_trees %>%
  separate(col = species, 
           into = c(NA, "common_name"), sep = "::", convert = TRUE)

# select all columns except plot_size
sf_trees1 <- sf_trees1 %>%
  select(-plot_size)


# create variable "tree_non_native"
sf_trees1 <-sf_trees1 %>% 
  mutate(tree_non_native = case_when(
    common_name == " Blackwood Acacia" ~ "Blackwood acacia",
    common_name == " Brisbane Box" ~ "Brisbane box" ,
    common_name == " Sycamore: London Plane"~ "London plane",
    common_name == " Red Flowering Gum"~ "Red flowering gum", 
    common_name == " Southern Magnolia"~ "Southern magnolia", 
    common_name == " New Zeeland Xmas Tree"~ "New Zeeland Xmas tree", 
    common_name == " Swamp Myrtle"~ "Swamp myrtle", 
    common_name == "" ~ "no_info",
    TRUE ~ "less_popular_types"))

# trim white spaces
sf_trees1 <- sf_trees1 %>%
  mutate_if(is.character, str_trim)

# create variable about tree size
sf_trees1 <-sf_trees1 %>% 
  mutate(trees_height_cat = case_when(
    dbh < 25 ~ "small tree",
    TRUE ~ "medium-large tree"))

# reverse order of trees_height_cat
sf_trees1 <- sf_trees1 %>%
  mutate(trees_height_cat = fct_relevel(trees_height_cat, 
                                        "small tree","medium-large tree"))

sf_trees1 <- mutate_at(sf_trees1, vars(tree_non_native), as.factor)


# filtering observation for popular species of trees  
sf_trees1 <- sf_trees1 %>%
  filter(tree_non_native %in% c("Blackwood acacia", "Brisbane box", "London plane", "Red flowering gum",
                                "Southern magnolia", "New Zeeland Xmas tree", "Swamp myrtle")) 


# filtering observation for two types of caretaker 
sf_trees1 <- sf_trees1 %>%
  filter(caretaker %in% c("DPW", "Private")) 
sf_trees1 <- mutate_at(sf_trees1, vars(caretaker), as.factor)

# drop NA
sf_trees1 <- sf_trees1 %>%
  drop_na()

# create variable "year"
sf_trees1 <- sf_trees1 %>%
  separate(col = date, 
           into = c("year", "month", NA), convert = TRUE) 

glimpse(sf_trees1)

# Simple plot 
ggplot(sf_trees1, aes(tree_non_native, caretaker)) + 
  geom_jitter(aes(color=trees_height_cat), width = 0.25) +
  scale_color_manual(values=c("#02FE52", "#01501A")) + 
  labs(colour = "Tree size") +
  labs(title = "Most popular trees in San Francisco, their caretaker and size", x = "Popular non-native trees", y = "Caretaker") +
  labs(caption = "Data source:San Francisco's open data portal (TidyTuesday) | Graphic: Cristina Cametti") +
  theme_bw() + 
  theme(
    panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold")) 

# Plot + animation 
ggplot(sf_trees1, aes(tree_non_native, caretaker)) + 
  geom_jitter(aes(color=trees_height_cat), width = 0.25) +
  scale_color_manual(values=c("#02FE52", "#01501A")) + 
  labs(colour = "Tree size") +
  labs(title = "Year: {frame_time}", x = "Popular non-native trees", y = "Caretaker") +
  labs(subtitle = "Most popular trees in San Francisco, their caretaker and size",
       caption = "Data source:San Francisco's open data portal (TidyTuesday) | Graphic: Cristina Cametti") +
  theme_bw() + 
  theme(
    panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold")) +
  transition_time(year, range = c(2010L, 2020L)) 

image <- animate(plot, width = 950, height = 750)
anim_save(filename ="trees.gif", animation = image)