# TidyTuesday 16/06/2020 - American Slavery and Juneteenth

library(tidyverse)
library(wordcloud2) 

african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

# Creating a variable as word count (& changing the column names)
final_data <- african_names %>%
  dplyr::count(name, sort = TRUE) %>%
  rename(word = name) %>%
  rename(freq = n)


# plotting with wordcloud2
# Africa Continent by Setyo Ari Wibowo from the Noun Project
p <- wordcloud2(final_data,figPath = "africa.png", 
                color='random-dark', backgroundColor="white")

p




