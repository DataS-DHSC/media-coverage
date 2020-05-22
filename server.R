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
    purrr::map_dfr(values$query_list, prep_results)
  })
  
  # Prepare data for visualisation
  weekly_data <- reactive({
    all_test <- dplyr::left_join(results(), covid_results(), 
                                 by = c('Date', 'Series')) %>%
      dplyr::mutate(prop_covid = (Value / all_covid) * 100,
                    week = lubridate::floor_date(as.Date(Date), 'week')) %>%
      dplyr::group_by(Series, week, query) %>%
      dplyr::summarise(Value = mean(Value, na.rm = T),
                       all_covid = mean(all_covid, na.rm = T),
                       prop_covid = mean(prop_covid, na.rm = T)) %>%
      dplyr::ungroup() 
  })
  
  #### VISUALISATIONS ####
  output$uk_plot <- renderPlot({
    weekly_data() %>% 
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
  })
  
  output$intl_plot <- renderPlot({
    weekly_data() %>% 
      dplyr::filter(Series %in% input$countries) %>%
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
  })
  
  #### KEY POINTS ####
  differences <- reactive({
    weekly_data() %>% 
      dplyr::filter((week == max(week) | week == max(week) - 7) & Series == 'United Kingdom') %>%
      dplyr::group_by(query, Series) %>%
      dplyr::arrange(week) %>%
      dplyr::mutate(difference = Value - lag(Value),
                    pct_change = difference / lag(Value)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(difference))
  })
  
  highest <- reactive({
    weekly_data() %>% 
      dplyr::filter(Series == 'United Kingdom') %>% 
      dplyr::group_by(query) %>% 
      dplyr::summarise(Value = sum(Value))
  }) 
  
  latest <- reactive({
    weekly_data() %>% 
      dplyr::filter(Series == 'United Kingdom' & week == max(week))
  })
  
  top_intl <- reactive({
    weekly_data() %>% 
      dplyr::filter(Value == max(Value))
  })
  
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
  
  comments <- reactive({
    c(paste0('Of the media topics monitored, ', highest()$query[which(highest()$Value == max(highest()$Value))], ' has had the most coverage. '),
                  paste0('Currently, ', latest()$query[which(latest()$Value == max(latest()$Value))], ' is having the most coverage. '),
                  paste0('The highest proportion of COVID media coverage in the past 4 months was on the topic of ', top_intl()$query,' on ',format(top_intl()$week, '%d %B'), '. '),
                  incr())
  })
  
  output$key_points <- renderUI({
    HTML(vectorBulletList(comments()))
  })
  
}