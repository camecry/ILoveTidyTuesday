# TidyTuesday 14/04/2020 - Rap Artists
# Get the Data

library(tidyverse)
library(hrbrthemes)
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

rankings <- rankings %>%
  filter(gender == "female")



final <- rankings %>%
  select(-ID, -title, - gender) %>%
  select(-n:-n5)



# bubble plot
p <- final %>%
  ggplot(aes(x=year, y=artist, size=points, color=points)) +
  geom_point(alpha=0.5, shape=20) +
  scale_x_continuous(n.breaks = 12) + 
  scale_size(range = c(3, 17), name="") +
  scale_color_gradientn(colours = rainbow(7), name="Critic ratings") + 
  theme_ft_rc(grid="X") +
  theme(legend.position="bottom") +
  ylab("Artists") +
  xlab("Years") +
  labs(title = "Women & hip-hop",
       subtitle = "Total points awarded by critics for songs in a given year done by female artists",
       caption = "Data source: BBC Music - Simon Jockers at Datawrapper 
       (&TidyTuesday) | Graphic: Cristina Cametti - @CameCry")+
  annotate(
    "text",
    x = 1985.2,
    y = 4,
    hjust = 0,
    size = 6, 
    colour = "#D4D3D2",
    label = "In 1998\nLauryn Hill made\ntwo great songs"
    ) + annotate(
    "curve",
    x = 1996.5,
    xend = 1989.5,
    y = 7,
    yend = 5.5,
    curvature = 0.25, 
    colour = "#D4D3D2", 
    size = 0.7,
    arrow = arrow(length = unit(2, "mm"))) 
p
