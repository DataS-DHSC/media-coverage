library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(lubridate)
library(readr)
library(purrr)
library(ggplot2)

library(shiny)
library(shinythemes)
library(rsconnect)

source('functions/query_prep.R')
source('functions/results_prep.R')
source('functions/visual_prep.R')

countries_list <- readr::read_tsv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', 
                                  col_names = c('cc','country'))
covid_query <- '(coronavirus OR virus OR pandemic OR epidemic OR covid OR corona)'
