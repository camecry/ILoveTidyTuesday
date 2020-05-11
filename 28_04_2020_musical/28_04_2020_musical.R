# TidyTuesday 28/04/2020 - Broadway musicals

library(tidyverse)
library(lubridate)
library(hrbrthemes)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

grosses <- grosses %>%
  select(-top_ticket_price)

grosses <- grosses %>%  
  mutate(wicked = case_when(
    show == "Wicked" ~ "Wicked",
    TRUE ~ "Other shows"))

grosses <- grosses %>%
  filter(show == "Wicked")

musical <- grosses %>%  
  mutate(production = case_when(
    week_ending >= "2003-10-12" & week_ending < "2005-01-16"~ "orig_prod",
    week_ending > "2015-09-15" & week_ending < "2016-07-30"~ "rachel_tucker",
    TRUE ~ "Other prod"))


musical <- mutate_at(musical, vars(production), as.factor)

musical <- musical %>%
  mutate(production = fct_relevel(production,
                                      "orig_prod", 
                                      "rachel_tucker",
                                      "Other prod"))
musical <- musical %>%
  mutate(Production = fct_recode(production,
                                 "Original production" = "orig_prod",
                              "Notable Elphaba Replacement"= "rachel_tucker", 
                              "Other production" = "Other prod"))
glimpse(musical)

# Plot
musical %>%
  ggplot(aes(x=week_ending, y=weekly_gross, size=pct_capacity, fill= Production)) +
  geom_point(alpha=0.5, shape=21)  +
  scale_size(range = c(1, 4), name="% seats sold") +
  scale_fill_manual(values=c("#33FF01", "#33CC00", "#F2F3F4"))+
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "Weekly gross") +
  labs(title = "Wicked: the best musical ever!",
       subtitle = "Just a personal opionion: the data are not confirming this!",
       caption = "Data source: Alex Cookson (&TidyTuesday) | Graphic: Cristina Cametti - @CameCry")+
  scale_x_date(date_breaks = '2 year', date_labels = '%Y') +
  annotate(
    "text",
    x = as.Date("2005-06-15", "%Y-%m-%d"),
    y = 4e+05,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "Idina Menzel as Elphaba &\n Kristin Chenoweth as Glinda") + annotate(
      "curve",
      x = as.Date("2004-08-15", "%Y-%m-%d"),
      xend = as.Date("2007-01-15", "%Y-%m-%d"),
      y = 10e+05,
      yend = 6e+05,
      curvature = -0.45, 
      colour = "black", 
      size = 1,
      arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    "text",
    x = as.Date("2015-08-15", "%Y-%m-%d"),
    y = 6e+05,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "Rachel Tucker as\n Elphaba") + annotate(
      "curve",
      x = as.Date("2016-04-15", "%Y-%m-%d"),
      xend = as.Date("2017-01-15", "%Y-%m-%d"),
      y = 13e+05,
      yend = 8e+05,
      curvature = -0.30, 
      colour = "black", 
      size = 1,
      arrow = arrow(length = unit(2, "mm"))) 



