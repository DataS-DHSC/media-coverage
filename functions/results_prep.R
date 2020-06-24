prep_country <- function(country){
  gsub(' Volume Intensity', '', country)
}

prep_results <- function(q_list){
  out <- read.csv(q_list[[1]], stringsAsFactors = FALSE)
  if(nrow(out) > 0){
    out$Date <- out[, 1]
    out$query <- q_list[[2]]
    out$Series <- prep_country(out$Series)
    return(out)
  }
}

covid_results <- function(){
  query <- prep_api(prep_query(covid_query))
  covid_only <- prep_results(list(query, 'all_covid')) 
  covid_out <- data.frame(Date = covid_only[,1],
                          Series = prep_country(covid_only$Series),
                          all_covid = covid_only$Value)
  return(covid_out)
}
