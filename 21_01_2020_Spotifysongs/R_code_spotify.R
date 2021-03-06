# Spotify songs - Tidy tuesday 21/01/2020
library(tidyverse)
library(forcats)
library(ggrepel)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs <- spotify_songs %>%
  drop_na()


# filtering observation: more popular rock songs 
rock_data <- spotify_songs %>%
  filter(playlist_genre == "rock") %>%
  filter(track_popularity > 70)

# album release decade
rock_data <- rock_data %>%
  separate(col = track_album_release_date, 
           into = c("year_release", "month_release", "day_release"))

rock_data <- rock_data %>%
  mutate(year_release = as.double(year_release))

rock_data_final <- rock_data %>%
  mutate(decades = case_when(
    between(year_release, 1958, 1969) ~ "60's",
    between(year_release, 1970, 1979) ~ "70's",
    between(year_release, 1980,1989) ~ "80's",
    between(year_release,1990,1999) ~ "90's",
    between(year_release, 2000,2009) ~ "2000's",
    TRUE ~ "2010's"))

rock_data_final <- mutate_at(rock_data_final, vars(decades), as.factor)
rock_data_final <- rock_data_final %>%
  mutate(decades = fct_relevel(decades, 
                               "60's","70's","80's", "90's", "2000's","2010's"))

rock_data_final <- mutate_at(rock_data_final, vars(playlist_subgenre), as.factor)

glimpse(rock_data_final)


# plotting loudness vs album release decade (track artist)

ggplot(rock_data_final, aes(decades, loudness)) + 
  geom_boxplot(aes(color = playlist_subgenre), outlier.alpha = 0.9) +
  coord_flip() +
  geom_text_repel(data          = subset(rock_data_final,loudness< -18.700),
                  aes(label = track_artist),
                  nudge_x =     -25 - subset(rock_data_final,loudness< -18.700)$loudness,
                  size          = 3.5,
                  box.padding   = 1.5,
                  point.padding = 1.0,
                  force         = 700,
                  min.segment.length = 1.0,
                  segment.color = "grey50",
                  direction     = "y") +
  labs(colour = "Playlist subgenre") +
  labs(x = "Album release decade", y = "Loudness of a track in decibels") +
  labs(title = "Are rock songs very loud?",
       subtitle = "Plot of more popular rock songs (song popularity > 70) & their relative loudness
       Loudness range: -25 (the least loud) - 0 (the loudest)",
       caption = "Data source: Spotify via spotifyr package | Graphic: Cristina Cametti - @CameCry") +
  theme_bw() + 
  theme(
    panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"))

# plotting loudness vs album release decade (track name)

ggplot(rock_data_final, aes(decades, loudness)) + 
  geom_boxplot(aes(color = playlist_subgenre), outlier.alpha = 0.9) +
  coord_flip() +
  geom_text_repel(data          = subset(rock_data_final,loudness< -18.700),
                  aes(label = track_name),
                  nudge_x =     -25 - subset(rock_data_final,loudness< -18.700)$loudness,
                  size          = 3.5,
                  box.padding   = 1.5,
                  point.padding = 1.0,
                  force         = 700,
                  min.segment.length = 1.0,
                  segment.color = "grey50",
                  direction     = "y") +
  labs(colour = "Playlist subgenre") +
  labs(x = "Album release decade", y = "Loudness of a track in decibels") +
  labs(title = "Are rock songs very loud?",
       subtitle = "Plot of more popular rock songs (song popularity > 70) & their relative loudness
       Loudness range: -25 (the least loud) - 0 (the loudest)",
       caption = "Data source: Spotify via spotifyr package | Graphic: Cristina Cametti - @CameCry") +
  theme_bw() + 
  theme(
    panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold"))