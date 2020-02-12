# Tidy tuesday 4/02/2020 
# NFL attendance 
library(tidyverse)

# Get the data
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

# Data manipulation
df <- standings %>%
  pivot_longer(cols = c("wins", "offensive_ranking", "defensive_ranking"), names_to = "measure", values_to = "value")

# Filter only teams that won the Superbowl
df <- df %>%
  filter(sb_winner == "Won Superbowl")

df <- df %>%  
 unite("team_year", c(team, year))

# Get better team names
df <- df %>%  
  mutate(team_year_specific = case_when(
    team_year == "Tampa Bay_2002" ~ "Buccaneers",
    team_year == "New Orleans_2009" ~ "Saints" ,
    TRUE ~ "Other teams"))
df <- df %>%  
  mutate(team_year_specific = fct_relevel(team_year_specific, 
                                        "Buccaneers","Saints", "Other teams"))
# Variabe measure as factor
df <- df %>%  
  mutate(measure = fct_relevel(measure, 
                               "defensive_ranking", "offensive_ranking", "wins"))
levels(df$measure) <- c('Defense','Offense','Wins')

#Plotting
ggplot(df, aes(measure, value)) + 
  geom_point(aes(group=team_year, color=team_year_specific), size = 2) +
  geom_line(aes(group=team_year, color=team_year_specific), size = 1) +
  scale_colour_manual(values=c("#C70039", "#C29C05", "grey")) +
  labs(colour = "Team") +
  labs(title = "Defense or offense?", x = "", y = "Ranking") +
  labs(subtitle = "Buccaneers & Saints: winning the Superbowl by doing the opposite",
       caption = "Data source: Pro Football reference | Graphic: Cristina Cametti @CameCry") +
  theme_bw() + 
  theme(
    panel.grid = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold")) 



