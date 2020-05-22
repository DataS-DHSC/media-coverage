prep_query <- function(q){
  u <- paste0(q, ' ', covid_query)
  u_rep <- stringr::str_replace_all(u, ' ', '%20') %>% stringr::str_replace_all('"', '%22') %>% stringr::str_replace_all("'", "%22")
  return(u_rep)
}

prep_api <- function(q){
  paste0('https://api.gdeltproject.org/api/v2/doc/doc?query=', q, '&mode=TimelineSourceCountry&format=csv&timespan=4m')
}
