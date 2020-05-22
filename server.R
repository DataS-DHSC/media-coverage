server <- function(input, output) {
  source('global.R')
  
  values <- reactiveValues(query_list = list())
  
  #### DATA PREPARATION ####
  # Prepare query list
  observeEvent(input$get_queries, {
    values$query_list <- list(list(prep_api(prep_query(input$q1_text)), input$q1_label), 
           list(prep_api(prep_query(input$q2_text)), input$q2_label), 
           list(prep_api(prep_query(input$q3_text)), input$q3_label), 
           list(prep_api(prep_query(input$q4_text)), input$q4_label), 
           list(prep_api(prep_query(input$q5_text)), input$q5_label), 
           list(prep_api(prep_query(input$q6_text)), input$q6_label))
  })
  
  # Run queries against GDELT v2
  results <- reactive({
    req(length(values$query_list) > 0)
    # query_list <- list(list(prep_api(prep_query('test')), 'test')) # Only uncomment for testing
    withProgress(message = 'Crunching data', value = 0, {
      results <- purrr::map_dfr(values$query_list, prep_results)
      })
    return(results)
  })
  
  # Prepare data for visualisation
  weekly_data <- reactive({
    req(length(values$query_list) > 0)
    withProgress(message = 'Calculating', value = 0, {
    weekly_data <- dplyr::left_join(results(), covid_results(), 
                                 by = c('Date', 'Series')) %>%
      dplyr::mutate(prop_covid = (Value / all_covid) * 100,
                    week = lubridate::floor_date(as.Date(Date), 'week')) %>%
      dplyr::group_by(Series, week, query) %>%
      dplyr::summarise(Value = mean(Value, na.rm = T),
                       all_covid = mean(all_covid, na.rm = T),
                       prop_covid = mean(prop_covid, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Series %in% c(input$countries, 'United Kingdom')) %>%
      dplyr::filter(nchar(query) > 2)
    })
    return(weekly_data)
  })
  
  # Calculate the max y-value
  y_lim <- reactive({
    req(length(values$query_list) > 0)
    weekly_data() %>% 
      dplyr::summarise(max_prop = max(prop_covid, na.rm = T))
  })
  
  #### VISUALISATIONS ####
  output$uk_plot <- renderPlot({
    req(length(values$query_list) > 0)
    weekly_data() %>% 
      dplyr::filter(Series == 'United Kingdom') %>%
      dplyr::mutate(Series = toupper(Series)) %>%
      ggplot(aes(week, prop_covid, col = query)) + 
      geom_line(size=0.8, alpha = 0.8) +
      facet_wrap(~ Series) + 
      scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +
      scale_y_continuous(limits = c(0, y_lim()$max_prop))+
      labs(title = 'Percentage of COVID Press Coverage of Selected Issues',
           x = '', y = '% of national COVID press coverage', 
           col = '') +
      guides(colour = guide_legend(nrow = 1)) +
      thm
  })
  
  output$intl_plot <- renderPlot({
    req(length(values$query_list) > 0)
    weekly_data() %>% 
      dplyr::filter(Series %in% input$countries) %>%
      dplyr::mutate(Series = toupper(Series)) %>%
      ggplot(aes(week, prop_covid, col = query)) + 
      geom_line(size=0.8, alpha = 0.8) +
      facet_wrap(~ Series, ncol = 2, scales = 'free_x') + 
      scale_y_continuous(limits = c(0, y_lim()$max_prop))+
      scale_x_date(date_breaks = "2 week", date_labels =  "%d %b") +
      labs(x = NULL, y = '% of national COVID press coverage', col = '',
           caption = "[Source: GDELT. Lines are a composite index of sub-themes. Each country's figures are calculated as a percentage of COVID coverage within that country.]") +
      thm+
      theme(legend.position = 'none')
  })
  
  #### KEY POINTS ####
  # Calculate changes week-on-week
  differences <- reactive({
    differences <- weekly_data() %>% 
      dplyr::filter((week == max(week) | week == max(week) - 7) & Series == 'United Kingdom') %>%
      dplyr::group_by(query, Series) %>%
      dplyr::arrange(week) %>%
      dplyr::mutate(difference = prop_covid - lag(prop_covid),
                    pct_change = difference / lag(prop_covid)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(difference))
    return(differences)
  })
  
  # Highest coverage in the UK
  highest <- reactive({
    weekly_data() %>% 
      dplyr::filter(Series == 'United Kingdom') %>% 
      dplyr::group_by(query) %>% 
      dplyr::summarise(prop_covid = sum(prop_covid, na.rm = T))
  }) 
  
  # Most recent value in the UK
  latest <- reactive({
    weekly_data() %>% 
      dplyr::filter(Series == 'United Kingdom' & week == max(week))
  })
  
  # Highest coverage across countries
  top_intl <- reactive({
    weekly_data() %>% 
      dplyr::filter(Value == max(Value, na.rm = T))
  })
  
  # Phrasing absolute and relative increases / decreases in coverage
  incr <- reactive({
    abs_change <- differences() %>% 
      dplyr::filter(abs(difference) == max(abs(difference)))
    pct_change <- differences() %>% 
      dplyr::filter(abs(pct_change) == max(abs(pct_change)))
    abs_dir <- ifelse(abs_change$difference > 0, 'increase', 'decrease')
    dir_word <- ifelse(abs_change$difference > 0, 'more', 'less')
    pct_dir <- ifelse(pct_change$pct_change > 0, 'increase', 'decrease')
    incr <- ifelse(abs_change$query == pct_change$query, 
                   paste0(abs_change$query, ' saw the greatest ', abs_dir, ' in UK coverage: ', round(abs(abs_change$difference), 1), '% ', dir_word, ' than last week. '),
                   paste0(abs_change$query, ' saw the greatest absolute ', abs_dir, ' in UK coverage: ', round(abs(abs_change$difference), 1), '% ', dir_word, ' than last week. ',
                          pct_change$query, ' saw the greatest relative ', pct_dir, ' in UK coverage: ', round(abs(pct_change$difference), 1), '% ', dir_word, ' than last week. '))
    return(incr)
  })
  
  # Put key points together
  comments <- reactive({
    c(paste0('Of the media topics monitored, ', highest()$query[which(highest()$prop_covid == max(highest()$prop_covid, na.rm = T))], ' has had the most coverage. '),
                  paste0('Currently, ', latest()$query[which(latest()$prop_covid == max(latest()$prop_covid, na.rm = T))], ' is having the most coverage. '),
                  paste0('The highest proportion of COVID media coverage in the past 4 months was on the topic of ', top_intl()$query,' on ',format(top_intl()$week, '%d %B'), '. '),
                  incr())
  })
  
  output$key_points <- renderUI({
    HTML(vectorBulletList(comments()))
  })
  
}