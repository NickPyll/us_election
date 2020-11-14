library(rvest)
library(tidyverse)
library(janitor)
library(cfeaR)
library(scales)
library(gridExtra)

x.election_tables <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin") %>% 
  html_table(fill = TRUE)

x.colnames <- c(
  "Election Number", "Election Date",
  "Winner", "Winner Party",
  "Electoral College Votes",
  "Electoral College Percent",
  "Popular vote Percent", "Popular vote Margin", "Popular vote count", "Popular vote count margin",
  "Runner-up", "Runner-up party", "Turnout")

election_data <- x.election_tables[[4]] %>%
  setNames(x.colnames) %>%
  clean_names() %>%
  filter(!is.na(election_number)) %>%
  mutate(election_date = as.numeric(if_else(election_number == 1, '1788', election_date)),
         electoral_college_percent = as.numeric(gsub("[%]", "", electoral_college_percent))/100,
         popular_vote_margin = as.numeric(gsub("[^0-9.]", "-", gsub("[%]", "", popular_vote_margin)))/100,
         popular_vote_percent = as.numeric(gsub("[%]", "", popular_vote_percent))/100,
         popular_vote_count = as.numeric(gsub("[,]", "", popular_vote_count))/100,
         popular_vote_count_margin = as.numeric(gsub("[^0-9.]", "-", gsub("[,]", "", popular_vote_count_margin)))/100,
         turnout = as.numeric(gsub("[%]", "", if_else(election_date == '2020', "65.0%", turnout)))/100)

p1 <-
  election_data %>%
  ggplot(aes(x = election_date, y = electoral_college_percent)) +
  geom_line() +
  theme_cfeaR_mod() +
  theme(
    panel.grid.major.y = element_line(colour = "grey98", size = 0.5),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#E3EAEE", colour = NA)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1L)) +
  labs(title = 'Electoral Votes Won by Year',
       x = 'Year',
       y = '% of Electoral Votes\nby Winning Candidate')

p2 <-
  election_data %>%
  ggplot(aes(x = election_date, y = popular_vote_margin)) +
  geom_line() +
  theme_cfeaR_mod() +
  theme(
    panel.grid.major.y = element_line(colour = "grey98", size = 0.5),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#E3EAEE", colour = NA)) +
  scale_y_continuous(limits = c(-.2, 1), labels = scales::percent_format(accuracy = 1L)) +
  labs(title = 'Popular Vote Margin by Year',
       x = 'Year',
       y = '% Margin Popular Vote')

p3 <-
  election_data %>%
  ggplot(aes(x = election_date, y = turnout)) +
  geom_line() +
  theme_cfeaR_mod() +
  theme(
    panel.grid.major.y = element_line(colour = "grey98", size = 0.5),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#E3EAEE", colour = NA)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1L)) +
  labs(title = 'Voter Turnout by Year',
       x = 'Year',
       y = 'Voter Turnout Rate')

grid.arrange(p1, p2, p3, ncol = 1)

