# TidyTuesday 17/03/2020 
# The Office

library(devtools)
devtools::install_github("lchiffon/wordcloud2")
install.packages("schrute")
library(tidyverse)
library(schrute)
library(tidytext)

# Following "Using schrute" article by Brad Lindblad:
# https://bradlindblad.github.io/schrute/articles/theoffice.html

# Import the data
mydata <- schrute::theoffice

# Tidytext process
token.mydata <- mydata %>%
  tidytext::unnest_tokens(word, text)

stop_words <- tidytext::stop_words

tidy.token.mydata <- token.mydata %>%
  dplyr::anti_join(stop_words, by = "word")


# Most popular online rank of best episode: Dinner Party (Season 4, Episode 13)
final_data <- tidy.token.mydata %>%
  filter(season == '04') %>%
  filter(episode == '13') 

# Creating a variable as word count
final_data <- final_data %>%
  dplyr::count(word, sort = TRUE) 
glimpse(data)

# Rename variable n as freq 
colnames(final_data)[2] <- "freq"

# plotting with wordcloud2
library(wordcloud2) 

p <- wordcloud2(final_data,figPath = "C:/Users/u0115252/Desktop/Tidy tuesday/17_03_2020_theoffice/desk.png", 
                color='random-light', backgroundColor="black")
p    # some bugs in visualizing the image (more info online about wordcloud2 package's bugs)



# install webshot: Problems with exporting the image  
library(webshot)
webshot::install_phantomjs()

# save it in html
library("htmlwidgets")
saveWidget(p,"tmp.html",selfcontained = F)

# and in png or pdf
webshot("tmp.html","desk_plot.png", delay =5, vwidth = 480, vheight=480)
