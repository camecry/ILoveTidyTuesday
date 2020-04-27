# Tidy tuesday 21/04/2020 - GDPR violations
library(tidyverse)

# Get the data
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

# Fix small problem 
gdpr_violations$article_violated[99] <- "Art. 32 GDPR"

# Cleaning strings
gdpr_violations$article_violated <- str_remove_all(gdpr_violations$article_violated, "GDPR")
gdpr_violations$article_violated <- str_remove_all(gdpr_violations$article_violated, "GDRP")
gdpr_violations$article_violated <- str_remove_all(gdpr_violations$article_violated, "Art.")


# Start general data tidying
data <- gdpr_violations %>% 
  separate(article_violated, 
           into = c("first_article_v", "second_article_v", "third_article_v", "fourth_article_v",
                    "fifth_article_v", "sixth_article_v"), 
           sep = "\\|")%>%
  slice(-86:-89) %>%
  slice(-101) 


data <- data %>%
  pivot_longer(cols = first_article_v:sixth_article_v,
               names_to = "type_article_viol", 
               values_to = "num_article_viol") %>%
  drop_na() %>%
  select(-source, -summary) 


# Tidying up string "num_article_viol"
data$num_article_viol <- str_squish(data$num_article_viol)

data$num_article_viol <-  str_remove_all(data$num_article_viol, ",")

data$num_article_viol <- str_replace(data$num_article_viol, '\\ ', ' sub_article') 
data$num_article_viol <- str_replace(data$num_article_viol, '\\(', ' ') 
data$num_article_viol <- str_replace(data$num_article_viol, '\\)', '') 
data$num_article_viol <- str_replace(data$num_article_viol, '\\ ', '_') 
data$num_article_viol <- str_replace(data$num_article_viol, '\\ ', '_') 


# Dropping the extra information after the first sub article letter (no match in gdpr_text)
data$num_article_viol <- str_replace_all(data$num_article_viol, "(\\s).+?", "")
data$num_article_viol <- str_replace(data$num_article_viol, '\\)', '') 

# Now better strings format
data$num_article_viol

# Transform in factors
data <- data %>%
  mutate_at(vars(type_article_viol:num_article_viol), as.factor) 

# Reshaping the data 
data <- data %>%
  group_by(name, fct_explicit_na(num_article_viol)) %>%
  tally() %>%
  rename(num_article_viol = `fct_explicit_na(num_article_viol)`) %>%
  drop_na() %>%
  ungroup()


# Rearrange data
data <- data %>%
  arrange(desc(n))%>%
  filter(n>5)

final_data <- data %>%
  mutate(num_article_viol = fct_recode(num_article_viol,"Art.13" = "13",
                              "Art.32"= "32", 
                              "Art.5" = "5", 
                              "Art.5,sub.1" = "5_sub_article_1",
                              "Art.6" = "6"))



# Plot
ggplot(final_data, aes(y=n, x=num_article_viol, fill=num_article_viol)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = TRUE, option = "A", name = "Article violated") +
  labs(title = "GDPR violations",
       subtitle = "Most common articles violated by EU countries",
       caption = "Data source: GDPR violations - TidyTuesday | Graphic: Cristina Cametti - @CameCry")+
  facet_wrap(~ name, dir = "v" ) +
  theme_ipsum_rc() +
  xlab("") + 
  ylab("Times the article was violated") +
  theme(axis.text.x = element_text(angle=50, hjust=1))




