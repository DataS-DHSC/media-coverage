prep_query <- function(q){
    stringr::str_replace_all(q, ' ', '%20') %>% stringr::str_replace_all('"', '%22') %>% stringr::str_replace_all("'", "%22")
}

prep_api <- function(q){
  paste0('https://api.gdeltproject.org/api/v2/doc/doc?query=', q, '&mode=TimelineSourceCountry&format=csv&timespan=4m')
}
