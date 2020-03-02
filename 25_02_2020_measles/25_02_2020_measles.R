# TidyTuesday 25/02/2020
# Measles

# Get the Data
library(tidyverse)
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

# Filter only year 2018-2019
data <- measles %>%
  filter(year == "2018-19")

# Select only few columns & drop na
dt <- data %>%
  select(index, state, county, city, name, xper) %>%
  drop_na()

# Filter schools here at least 25% of students is exempted from vaccination for personal reasons
dt <- dt %>% 
  arrange(desc(xper)) %>%
  filter(xper > 25.00)

dt <- distinct(dt, index, .keep_all = TRUE)

# Getting shorter names for certain schools
dt[27, "name"] <- "Montessori Education Centre"
dt[35, "name"] <- "Montessori D. P. Lakeside"
dt[50, "name"] <- "Montessori Education Centre"
dt[42, "name"] <- "American Leaders A.-Gilbert"
dt[45, "name"] <- "Elem Sch for Arts &amp; Academics"
dt[5, "name"] <- "Eagleridge E. Program"

# Start seetting the plot -> See https://www.r-graph-gallery.com/circular-barplot.html
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(dt))
colnames(to_add) <- colnames(dt)
dt <- rbind(dt, to_add)
dt$id <- seq(1, nrow(dt))

# Get the name and the y position of each label
label_data <- dt
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# palette
pal <- c("#D4761F","#050AC1")

# Make the plot of students exempted from vaccination for personal reasons
# Big thanks to Jake Lawlor's Tidy tuesday submission on food-related C2 
p <- ggplot(dt, aes(x=as.factor(id), y=xper))+
  geom_bar(stat="identity", aes(fill=state)) +
  scale_fill_manual(values=pal)+
  ylim(-50,90) +
  theme_bw() + 
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4,), "cm")) +
  annotate(geom = "text",
           x=0,y=-50,
           hjust=.5, vjust=1,
           label="Schools where at least 25% of 
students is exempted from 
vaccination for personal reasons",
           size=4.3, lineheight=.8,
           family="Staatliches Regular",
           color="black") +
  annotate(geom = "text",
           x=0,y=200,
           vjust=1,
           hjust=.5,
           label = "Data: The Wallstreet Journal | Graphic: Cristina Cametti - @CameCry",
           size=2.5,
           color="black")+
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=xper+1, label=name, hjust=hjust), color="black", 
            fontface="bold",alpha=0.6, size=2.6, angle=label_data$angle, inherit.aes = FALSE ) 

p



