ui <- fluidPage(
  titlePanel("COVID media coverage tool"),
  sidebarLayout(
    sidebarPanel(
      actionButton('get_queries', 'Run queries'),
      hr(),
      textInput('q1_text', 
                label = 'Query 1'),
      textInput('q1_label', 
                label = 'Name of Query 1'),
      textInput('q2_text', 
                label = 'Query 2'),
      textInput('q2_label', 
                label = 'Name of Query 2'),
      textInput('q3_text', 
                label = 'Query 3'),
      textInput('q3_label', 
                label = 'Name of Query 3'),
      textInput('q4_text', 
                label = 'Query 4'),
      textInput('q4_label', 
                label = 'Name of Query 4'),
      textInput('q5_text', 
                label = 'Query 5'),
      textInput('q5_label', 
                label = 'Name of Query 5'),
      textInput('q6_text', 
                label = 'Query 6'),
      textInput('q6_label', 
                label = 'Name of Query 6')
    ),
    mainPanel(
      selectizeInput('countries', 
                     label = 'Choose countries to compare to the UK', 
                     choices = countries_list$country, multiple = TRUE),
      h3('Key points'),
      htmlOutput(outputId = 'key_points'),
      hr(),
      h3('Plots'),
      plotOutput(outputId = "uk_plot"),
      plotOutput(outputId = "intl_plot")
    )
  )
)