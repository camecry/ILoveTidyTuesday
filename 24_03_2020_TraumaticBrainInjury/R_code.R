# Tidy Tuesday 24/03/2020
# Traumatic brain injury

extrafont::loadfonts(device="win")
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(hrbrthemes)


# Get the data
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

# Getting the sum
mildata_sum <- tbi_military %>%
  filter(component == "Active") %>%
  group_by(year, service, severity) %>%
  summarize(total = sum(diagnosed))


# data manipulation
mil_data <- mildata_sum %>% 
  filter(service %in% c("Army", "Marines")) %>%
  group_by(year, service) %>%
  mutate(sum_diagnosed = sum(total))%>%
  select(year, service, sum_diagnosed) %>%
  distinct() %>%
  pivot_wider(names_from = service, values_from = sum_diagnosed)

# Plotting a connected scatterplot

# Select the years to label the chart
tmp_date <- mil_data%>% sample_frac()

plot <- mil_data %>% 
  ggplot(aes(x=Army, y=Marines, label=year)) +
  geom_point(color="#FF3300", size=4) +
  geom_text_repel(data=tmp_date, size = 5, point.padding =1.5, direction = "y") +
  geom_segment(color="#4b5320", 
               aes(
                 xend=c(tail(Army, n=-1), NA), 
                 yend=c(tail(Marines, n=-1), NA)),
               size=1, 
               alpha= 0.7,
               arrow=arrow(length=unit(0.4,"cm"), type = "closed")) +
  labs(x = "DoD numbers for TBI - Army", y = "DoD numbers for TBI - Marines") +
  labs(title = "Traumatic Brain Injury for active U.S. military",
       subtitle = "Connected scatterplot of TBI for active Army and Marines personnel",
       caption = "Data source: Veterans Brain Injury Center (&TidyTuesday) | Graphic: Cristina Cametti - @CameCry")+
  theme_ipsum_rc() +  
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"),
    plot.caption.position =  "plot")

plot
