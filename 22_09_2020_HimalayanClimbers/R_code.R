# Himalayan Climbing Expeditions

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

library(tidyverse)
library(hrbrthemes)
library(naniar)
library(patchwork)

# Sherpas using oxygen 
data_ox <- members %>%
  filter(oxygen_used==TRUE) %>%
  filter(highpoint_metres >= 8000) %>%
  filter(citizenship == "Nepal")

data_ox <- data_ox %>%
  mutate(outcome = case_when(
    died == TRUE ~ "died", 
    injured == TRUE ~ "injured", 
    died == FALSE ~ "safe"))


# Creating a new variable "death_inj_height" about the height in meters when death
# or injuries happened
data_ox <- data_ox %>%
  unite("death_inj_height", death_height_metres, injury_height_metres, na.rm = TRUE) %>%
  replace_with_na(replace = list(death_inj_height = "NA_NA")) 
data_ox$death_inj_height <- str_remove_all(data_ox$death_inj_height, "_NA")
data_ox$death_inj_height <- str_remove_all(data_ox$death_inj_height, "NA_")
data_ox <- data_ox %>%
  mutate(death_inj_height = replace_na(death_inj_height, 0))

data_ox <- data_ox %>%
  mutate_at(vars(peak_name), as.factor) %>%
  mutate_at(vars(outcome), as.factor) %>%
  mutate_at(vars(death_inj_height), as.integer)

glimpse(data_ox)

# Plotting sherpas using oxygen
p1 <- data_ox %>%
  arrange(desc(death_inj_height)) %>%
  ggplot(aes(x=age, y=highpoint_metres,fill=outcome, size=death_inj_height)) +
  geom_point(alpha=0.5, shape=21) +
  scale_x_continuous(n.breaks = 12) +
  scale_size(range = c(.1, 10), breaks = c(0, 3000, 6000, 8000),name="Height at which the person died / \ninjury occurred") +
  scale_fill_manual(values=c("#FF0000","#0000FF","#33FF33")) +
  theme_modern_rc() +
  theme(legend.position="bottom") +
  ylab("Elevation highpoint of the person") +
  xlab("") +
  theme(legend.position = "none") +
  labs(title = "Sherpa climbers' fate in Himalayan expeditions \nabove 8000 meters using oxygen") +
  annotate(
    "text",
    x = 14,
    y = 8670,
    hjust = 0,
    size = 4, 
    colour = "#D4D3D2",
    label = "Youngest climber\n(14 y/o - injured)") +
  annotate(
    "curve",
    x = 14,
    xend = 14,
    y = 8700,
    yend = 8800,
    curvature = -0.25, 
    colour = "#D4D3D2", 
    size = 0.7,
    arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    "text",
    x = 67,
    y = 8760,
    hjust = 0,
    size = 4, 
    colour = "#D4D3D2",
    label = "Oldest climber\n(76 y/o)") +
  annotate(
    "curve",
    x = 75,
    xend = 76,
    y = 8780,
    yend = 8840,
    curvature = 0.25, 
    colour = "#D4D3D2", 
    size = 0.7,
    arrow = arrow(length = unit(2, "mm"))) 

p1
  

###############################
# Sherpas not using oxygen
  data_no <- members %>%
    filter(oxygen_used==FALSE) %>%
    filter(highpoint_metres >= 8000) %>%
    filter(citizenship == "Nepal")
  
  data_no <- data_no %>%
    mutate(outcome = case_when(
      died == TRUE ~ "died", 
      injured == TRUE ~ "injured", 
      died == FALSE ~ "safe"))



# Creating a new variable "death_inj_height" about the height in meters when death
# or injuries happened
data_no <- data_no %>%
  unite("death_inj_height", death_height_metres, injury_height_metres, na.rm = TRUE) %>%
  replace_with_na(replace = list(death_inj_height = "NA_NA")) 
data_no$death_inj_height <- str_remove_all(data_no$death_inj_height, "_NA")
data_no$death_inj_height <- str_remove_all(data_no$death_inj_height, "NA_")
data_no <- data_no %>%
  mutate(death_inj_height = replace_na(death_inj_height, 0))


data_no <- data_no %>%
  mutate_at(vars(peak_name), as.factor) %>%
  mutate_at(vars(outcome), as.factor) %>%
  mutate_at(vars(death_inj_height), as.integer)

glimpse(data_no)

# Plotting sherpas not using oxygen
p2 <- data_no %>%
  arrange(desc(death_inj_height)) %>%
  ggplot(aes(x=age, y=highpoint_metres,fill=outcome, size=death_inj_height)) +
  geom_point(alpha=0.5, shape=21) +
  scale_x_continuous(n.breaks = 12) +
  scale_size(range = c(.1, 10), breaks = c(0, 3000, 6000, 8000),name="Height at which \nthe person died / \ninjury occurred") +
  scale_fill_manual(values=c("#FF0000","#0000FF","#33FF33"), name="Climbers' fate") +
  theme_modern_rc() +
  theme(legend.position="bottom") +
  ylab("") +
  xlab("Age") +
  theme(legend.position = "right") +
    labs(title = "Sherpa climbers' fate in Himalayan expeditions \nabove 8000 meters not using oxygen",
  caption = "Data source: Alex Cookson & TidyTuesday | Graphic: Cristina Cametti - @CameCry")
p2

# Combining two plots 
p1 + p2

