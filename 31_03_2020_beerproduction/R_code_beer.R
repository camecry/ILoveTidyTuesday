# TidyTuesday 31/03/2020 - Beer production

library(tidyverse)
library(hrbrthemes)
library(ggimage)
library(gganimate)


# Get the Data
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

# Tidying the data
beer <- brewing_materials %>%
  pivot_longer(cols= month_current:ytd_prior_year,
               names_to = "number_type",
               values_to = "number_barrels")

# Filtering for only current month, exclude the total amounts & "other"category 
# for type of material 
data_filter <- beer %>%
  filter(!type %in% c("Total Grain products", 
                               "Total Non-Grain products", 
                               "Total Used", 
                      "Other")) %>%
  filter(number_type == "month_current")


# Creating a new shorter variable for type (of material)
data_filter <- data_filter %>%
  mutate(type_short = case_when(
    type == "Wheat and wheat products" | type == "Barley and barley products" ~ "Wheat & Barley",
    type == "Hops (used as extracts)" | type == "Hops (dry)" ~ "Hops",
    type == "Sugar and syrups" ~ "Sugar products",
    type == "Rice and rice products" ~ "Rice products",
    type == "Malt and malt products" ~ "Malt products",
    type == "Corn and corn products" ~ "Corn products"))

data_filter <- data_filter %>%
  select(-data_type, -type)

# Getting the sum
sum_beer <- data_filter %>%
  group_by(material_type, type_short) %>%
  summarize(total = sum(number_barrels)) %>%
  ungroup()

# Getting the %
beer_perc <- sum_beer %>%
  mutate(percent = total/sum(total) * 100)


# Adding images to our data
d <- c("C:/Users/u0115252/Desktop/Tidy tuesday/31_03_2020_beer/corn.png",
       "C:/Users/u0115252/Desktop/Tidy tuesday/31_03_2020_beer/malt.png",
       "C:/Users/u0115252/Desktop/Tidy tuesday/31_03_2020_beer/rice.png",
       "C:/Users/u0115252/Desktop/Tidy tuesday/31_03_2020_beer/flour.png",
       "C:/Users/u0115252/Desktop/Tidy tuesday/31_03_2020_beer/hop.png",
       "C:/Users/u0115252/Desktop/Tidy tuesday/31_03_2020_beer/sugar.png")

final_data <- beer_perc %>%
  mutate(image = d)


# Lollipop horizontal version with customized images for %
ggplot(final_data, aes(x=type_short, y=percent)) +
  geom_segment(aes(x=type_short, xend=type_short, y=0, yend=percent), color="#f28e1c") +
  geom_image(aes(image=image), size=.06) +
  coord_flip() +
  labs(x="Type of materials used at breweries", y="% of used materials",
       title="Malt, the fundamental ingredient to make beer!",
       subtitle="% of materials used at U.S. breweries for beer production (in 2008-2017)",
       caption="Data source: TTBI & TidyTuesday) | Graphic: @CameCry | Icons:Hop,Corn,Rice,Flour by Freepik,Malt by xnimrodx,Sugar by Smashicons - via flaticon.com") + 
  theme_ft_rc()


# Lollipop horizontal version with customized images for pounds
p <- ggplot(final_data, aes(x=type_short, y=total)) +
  geom_segment(aes(x=type_short, xend=type_short, y=0, yend=total), color="#f28e1c") +
  geom_image(aes(image=image), size=.06) +
  coord_flip() +
  labs(x="Type of materials used at the breweries", y="% of used materials",
       title="Malt, the foundamental ingredient to make beer!",
       subtitle="% of materials used at USA breweries for beer production (in 2008-2017)",
       caption="Data source: TTBI & TidyTuesday) | Graphic: @CameCry | Icons:Hop,Corn,Rice,Flour by Freepik,Malt by xnimrodx,Sugar by Smashicons - via flaticon.com") + 
  theme_ft_rc()
p
