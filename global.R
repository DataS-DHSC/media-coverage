library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(purrr)
library(ggplot2)

source('functions/query_prep.R')
source('functions/results_prep.R')

q1_text <- '(test OR testing)'
q2_text <- '("testing some more")'

query_list <- list(list(prep_api(prep_query(q1_text)), 'q1_label'), 
                   list(prep_api(prep_query(q2_text)), 'q2_label'))

test <- purrr::map_dfr(query_list, prep_results)

all_test <- dplyr::left_join(test, covid_results(), by = c('Date', 'Series')) %>%
  dplyr::mutate(prop_covid = (Value / all_covid) * 100)
