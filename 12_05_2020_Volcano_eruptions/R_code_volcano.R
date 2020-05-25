# # TidyTuesday 12/05/2020 - Volcano Eruptions

library(tidyverse)
library(hrbrthemes)

# eruptions.csv
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv') 

data_eruptions <- eruptions %>%
  mutate_at(vars(eruption_category, evidence_method_dating, vei), as.factor)


data_eruptions <- data_eruptions %>% 
  filter(volcano_name %in% c("Kilauea", "Mauna Kea", "Haleakala"))


# event.csv
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

events <- events %>%
  mutate_if(is.character, as.factor)

events <- events %>%
  filter(volcano_name %in% c("Kilauea", "Mauna Kea", "Haleakala"))

# left join

data_join <- events %>% 
  left_join(eruptions, by = "eruption_number")

glimpse(data_join)
data_fin <- data_join %>%
  select(-event_remarks:-event_date_day, -area_of_activity, -evidence_method_dating, -latitude, 
         -longitude, -eruption_category, -volcano_name.y)

glimpse(data_fin)


data_fin <-data_fin %>%
    mutate(event_type_lump = fct_lump_n(event_type, n=10))
levels(data_fin$event_type_lump)

  

data_fin <- data_fin %>%
  mutate(short_event_type = case_when(
    event_type_lump == "Lava lake" | event_type_lump == "Lava flow(s)" | event_type_lump == "Lava fountains" ~ "Lava",
    event_type_lump == "Earthquakes (undefined)" | event_type_lump == "Seismicity (volcanic)" | event_type_lump == "Volcanic tremor" ~ "General Seismic Activity", 
    event_type_lump == "VEI (Explosivity Index)" ~ "VEI (Explosivity Index)",
    event_type_lump == "Cinder cone formation" | event_type_lump == "Deformation (inflation)" ~ "(De)formations",
    event_type_lump == "Explosion" ~ "Explosion",
    TRUE  ~ "Other"
    )) %>%
  mutate_if(is.character, as.factor)
  
data_fin <- data_fin %>%
  mutate(short_event_type = fct_relevel(short_event_type, 
                                        "Lava","Explosion", "VEI (Explosivity Index)", "(De)formations",
                                        "General Seismic Activity", "Other"))
glimpse(data_fin)



#############################################################################
data_fin %>%
  ggplot(aes(x=eruption_start_year, y=volcano_name.x, color=short_event_type)) +
  geom_point(alpha=0.4, shape=20, size =10)  + 
  scale_color_brewer(type='div', palette="RdYlBu", name="Event type") +
  theme_modern_rc(grid="X") +
  theme() +
  ylab("") +
  xlab("Years") +
  labs(colour = "") +
  labs(title = "Eruptions of my favorite Hawaiian volcanoes",
       subtitle = "Eruption time & type for Mauna Kea, Kilauea and Haleakala",
       caption = "Data source: The Smithsonian Institution (&TidyTuesday) | Graphic: Cristina Cametti - @CameCry") +
  annotate(
    "text",
    x = -1500,
    y = "Mauna Kea",
    hjust = 0,
    size = 4, 
    colour = "white",
    label = "Dormant"
  ) +
  annotate(
    "text",
    x = -7400,
    y = "Kilauea",
    hjust = 0,
    size = 4, 
    colour = "red",
    label = "Active"
  ) +
  annotate(
    "text",
    x = -4400,
    y = "Haleakala",
    hjust = 0,
    size = 4, 
    colour = "#27D5FF",
    label = "Normal"
  )
######################################################



