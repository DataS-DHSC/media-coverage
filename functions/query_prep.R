prep_query <- function(q){
  u <- paste0(q, ' ', covid_query)
  u_rep <- stringr::str_replace_all(u, ' ', '%20') %>% 
    stringr::str_replace_all('"', '%22') %>% 
    stringr::str_replace_all("'", "%22") %>% 
    stringr::str_replace_all('\\(', '%28') %>% 
    stringr::str_replace_all('\\)', '%29')
  return(u_rep)
}

prep_api <- function(q){
  paste0('https://api.gdeltproject.org/api/v2/doc/doc?query=', q, '&mode=TimelineSourceCountry&format=json&timespan=4m')
}

read_gdelt_json = function(u){
  message(u)
  u = str_replace_all(u, ' ', '%20') %>% str_replace_all('"', '%22')
  if(!str_detect(u, fixed('json',ignore_case=TRUE))){
    u = paste0(u, '&format=json')
    message('*** "json" not detected in URL. Are you sure format argument is correct? ***')
    message('.. trying with "&format=json" added to URL..')
  }
  g = tryCatch({ jsonlite::fromJSON(u) }, error = function(e) { 
    message('*** alternative parse ***')
    fileIn = file(u, open="rb", encoding="UTF-8")
    lines = readLines(fileIn, warn = FALSE)
    clean_lines = gsub("[\001-\026]*", "", lines) # remove problematic chars
    jsonlite::fromJSON(clean_lines)
  })
  if('features' %in% names(g)){
    geoms = g$features$geometry %>% as_tibble() %>% 
      mutate(coordinates = map(coordinates, ~ tibble(lon = .x[1], lat = .x[2]))) %>% 
      unnest(coordinates)
    props = g$features$properties %>% as_tibble()
    if('urlpubtimedate' %in% names(props)) props = props %>% mutate(urlpubtimedate = as.POSIXct(urlpubtimedate, format = '%Y-%m-%dT%H:%M:%SZ'))
    out = bind_cols(geoms, props)
  }
  if('articles' %in% names(g)){
    out = g$articles %>% as_tibble()
  }
  if('timeline' %in% names(g)){
    out = map2_df(g$timeline$series, g$timeline$data, { ~ as_tibble(.y) %>% mutate(series = .x) %>% 
        mutate(date = as.POSIXct(date, format='%Y%m%dT%H%M%SZ')) 
    }) %>% mutate(series = str_remove(series, ' Volume Intensity$'))
    if('toparts' %in% names(out)) out = out %>% unnest(toparts)
  }
  colnames(out) <- c('Date', 'Value', 'Series')
  return(out)
}