# TidyTuesday 09/06/2020
# African American Achievements

# get the data
first <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')

# load libraries 
library(tidyverse)
library(ggimage)
library(hrbrthemes)

# Fix error 
first$gender[2] <- "African-American Firsts"


# data manipulation
first <- first %>%
  mutate(gender = case_when(
    gender == "African-American Firsts" ~ "male", 
    gender == "Female African American Firsts" ~ "female"
  ))


first <- first %>%
  mutate_at(vars(category, accomplishment, gender), as.factor)


# adding fist image on specific observations
h_df1 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 1999 & category == "Education & Science")

d <- "~path/fist.png"

h_df1 <- h_df1 %>%
  mutate(image = d)


h_df2 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 1773 & category == "Arts & Entertainment")

h_df2 <- h_df2 %>%
  mutate(image = d)


h_df3 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 1988 & category == "Law")

h_df3 <- h_df3 %>%
  mutate(image = d)

h_df4 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 1985 & category == "Military")

h_df4 <- h_df4 %>%
  mutate(image = d)


h_df5 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 1952 & category == "Politics")

h_df5 <- h_df5 %>%
  mutate(image = d)

h_df6 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 2009 & category == "Religion")

h_df6 <- h_df6 %>%
  mutate(image = d)

h_df7 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 1910 & category == "Social & Jobs")

h_df7 <- h_df7 %>%
  mutate(image = d)


h_df8 <- first %>% 
  filter(gender=="female")%>%
  filter(year == 1917 & category == "Sports")

h_df8 <- h_df8 %>%
  mutate(image = d)





# Plotting

first %>%
  ggplot(aes(x=year, y=category, fill=gender)) +
  geom_point(alpha=0.4, shape=21, size=3.5, color="black") +
  geom_image(data=h_df1, aes(image=image), size=0.04) +
  geom_image(data=h_df2, aes(image=image), size=0.04) +
  geom_image(data=h_df3, aes(image=image), size=0.04) +
  geom_image(data=h_df4, aes(image=image), size=0.04) +
  geom_image(data=h_df5, aes(image=image), size=0.04) +
  geom_image(data=h_df6, aes(image=image), size=0.04) +
  geom_image(data=h_df7, aes(image=image), size=0.04) +
  geom_image(data=h_df8, aes(image=image), size=0.04) +
  scale_color_brewer(type='qual') +
  labs(title = "African-American Achievements",
       subtitle = "Small focus on African-American women achievements",
       caption = "Data source: Wikipedia (&TidyTuesday) | Graphics: @CameCry | Fist by Maxim Kulikov from the Noun Project") +
  theme_ipsum() +
  ylab("") +
  xlab("") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1738,2019,25)) +
  annotate(
    "text",
    x = 1952,
    y = 2.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st African-American woman\nuniversity president") +
  annotate(
    "text",
    x = 1725,
    y = 1.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st known African-American woman to\npublish a book") +
  annotate(
    "text",
    x = 1927,
    y = 3.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st African-American woman elected\nto a U.S. judgeship")+
  annotate(
    "text",
    x = 1930,
    y = 4.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st African-American woman general")+
  annotate(
    "text",
    x = 1900,
    y = 5.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st African-American woman elected to\na U.S. state senate")+
  annotate(
    "text",
    x = 1920,
    y = 6.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st African-American woman rabbi") +
  annotate(
    "text",
    x = 1900,
    y = 7.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st African-American woman millionaire")+
  annotate(
    "text",
    x = 1850,
    y = 8.5,
    hjust = 0,
    size = 4, 
    colour = "black",
    label = "1st African-American woman to win a major sports title")










