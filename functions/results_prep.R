prep_country <- function(country){
  gsub(' Volume Intensity', '', country)
}

prep_results <- function(q_list){
  out <- read.csv(q_list[[1]], stringsAsFactors = FALSE)
  if(nrow(out) > 0){
    out$query <- q_list[[2]]
    out$Series <- prep_country(out$Series)
    return(out)
  }
}

covid_results <- function(){
  query <- prep_api(prep_query(covid_query))
  covid_only <- prep_results(list(query, 'all_covid')) %>%
    dplyr::transmute(Date, 
                     Series = prep_country(Series),
                     all_covid = Value)
  return(covid_only)
}
