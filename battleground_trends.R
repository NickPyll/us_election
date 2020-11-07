library(tidyverse)
library(tibble)
library(ggplot2)
library(cfeaR)
library(scales)
library(gridExtra)
library(devtools)
library(lubridate)

battleground_state_changes <-
  read_csv(url('https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/battleground-state-changes.csv')) %>%
  mutate(incumbent_lead = if_else(leading_candidate_name == 'Biden', 0 - vote_differential, vote_differential)) %>%
  mutate(georgia_overestimate_remain = 
           if_else(
             timestamp > as_datetime('2020-11-04 23:14:44') & 
               timestamp < as_datetime('2020-11-05 16:37:48'), 1, 0))

georgia_clean <-
  battleground_state_changes %>%
  filter(state == 'Georgia (EV: 16)',
         georgia_overestimate_remain == 0)

p1 <-
  battleground_state_changes %>%
  filter(state == 'Georgia (EV: 16)') %>%
  ggplot() +
  geom_point(aes(x = incumbent_lead, y = votes_remaining, color = leading_candidate_name, shape = factor(georgia_overestimate_remain))) +
  scale_color_manual(values = c('blue', 'red')) +
  geom_smooth(data = georgia_clean, aes(x = incumbent_lead, y = votes_remaining), color = 'grey70', fill = 'grey90', method = 'lm', fullrange = TRUE) +
  theme_cfeaR() +
  theme(legend.position = 'none') +
  scale_x_continuous(labels = comma, limits = c(-10000, 120000)) +
  scale_y_continuous(labels = comma, limits = c(0, 400000)) +
  labs(title = 'Georgia',
       x = 'Incumbent Lead',
       y = 'Estimated Votes Remaining')

p2 <-
  battleground_state_changes %>%
  filter(state == 'Pennsylvania (EV: 20)') %>%
  ggplot() +
  geom_point(aes(x = incumbent_lead, y = votes_remaining, color = leading_candidate_name)) +
  scale_color_manual(values = c('blue', 'red')) +
  geom_smooth(aes(x = incumbent_lead, y = votes_remaining), color = 'grey70', fill = 'grey90', method = 'lm', fullrange = TRUE) +
  theme_cfeaR() +
  theme(legend.position = 'none') +
  scale_x_continuous(labels = comma, limits = c(-10000, 500000)) +
  scale_y_continuous(labels = comma, limits = c(0, 1200000)) +
  labs(title = 'Pennsylvania',
       x = 'Incumbent Lead',
       y = 'Estimated Votes Remaining')

p3 <-
  ggplot(battleground_state_changes %>%
           filter(state == 'Arizona (EV: 11)'),
         aes(x = vote_differential, y = votes_remaining)) +
  geom_point(color = 'blue') +
  geom_smooth(color = 'grey70', fill = 'grey90', method = 'lm', fullrange = TRUE) +
  theme_cfeaR() +
  scale_x_continuous(labels = comma, limits = c(-10000, 150000)) +
  scale_y_continuous(labels = comma, limits = c(0, 1200000)) +
  labs(title = 'Arizona',
       x = 'Challenger Lead',
       y = 'Estimated Votes Remaining')

p4 <-
  ggplot(battleground_state_changes %>%
         filter(state == 'Nevada (EV: 6)'),
       aes(x = vote_differential, y = votes_remaining)) +
  geom_point(color = 'blue') +
  geom_smooth(color = 'grey70', fill = 'grey90', method = 'lm', fullrange = TRUE) +
  theme_cfeaR() +
  scale_x_continuous(labels = comma, limits = c(0, 25000)) +
  scale_y_continuous(labels = comma, limits = c(0, 250000)) +
  labs(title = 'Nevada',
       x = 'Challenger Lead',
       y = 'Estimated Votes Remaining')

grid.arrange(p1, p2, p3, p4, ncol = 2)

