library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(lubridate)
library(purrr)
library(ggplot2)

source('functions/query_prep.R')
source('functions/results_prep.R')
source('functions/visual_prep.R')

q1_text <- '(test OR testing)'
q2_text <- '("testing some more")'

query_list <- list(list(prep_api(prep_query(q1_text)), 'q1_label'), 
                   list(prep_api(prep_query(q2_text)), 'q2_label'))

test <- purrr::map_dfr(query_list, prep_results)

all_test <- dplyr::left_join(test, covid_results(), by = c('Date', 'Series')) %>%
  dplyr::mutate(prop_covid = (Value / all_covid) * 100,
                week = lubridate::floor_date(as.Date(Date), 'week'))

weekly_test <- all_test %>%
  dplyr::group_by(Series, week) %>%
  dplyr::summarise(Value = mean(Value, na.rm = T),
                   all_covid = mean(all_covid, na.rm = T),
                   prop_covid = mean(prop_covid, na.rm = T)) %>%
  dplyr::ungroup() 

uk_plot <- weekly_test %>% 
  dplyr::filter(Series == 'United Kingdom') %>%
  dplyr::mutate(Series = toupper(Series)) %>%
  ggplot(aes(week, prop_covid, col = query)) + 
  geom_line(size=0.8, alpha = 0.8) +
  facet_wrap(~ Series) + 
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +
  scale_y_continuous(limits = c(0, max(weekly_test$prop_covid)))+
  labs(title = 'Percentage of COVID Press Coverage of Selected Issues',
       x = '', y = '% of national COVID press coverage', 
       col = '') +
  guides(colour = guide_legend(nrow = 1)) +
  thm
uk_plot

intl_plot <- weekly_test %>% 
  dplyr::filter(Series != 'United Kingdom') %>%
  dplyr::mutate(Series = toupper(Series)) %>%
  ggplot(aes(week, prop_covid, col = query)) + 
  geom_line(size=0.8, alpha = 0.8) +
  facet_wrap(~ Series, ncol = 2, scales = 'free_x') + 
  scale_y_continuous(limits = c(0, max(weekly_test$prop_covid)))+
  scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +
  labs(x = NULL, y = '% of national COVID press coverage', col = '',
       caption = "[Source: GDELT. Lines are a composite index of sub-themes. Each country's figures are calculated as a percentage of COVID coverage within that country.]") +
  thm+
  theme(legend.position = 'none')
intl_plot
