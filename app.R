library(shiny)
library(echarts4r)
library(nycflights13)
library(dplyr)
library(DT)

# We use a subset of the flights data
data <- flights %>% 
  mutate(date = as.Date(time_hour)) %>% 
  filter(date <= as.Date('2013-01-31')) %>% 
  select(date, time_hour, carrier, tailnum, origin, dest) %>% 
  arrange_all()

ui <- fluidPage(
  tags$h1('Demo for echarts4r callbacks'),
  tags$p('A simple example of chart interactions with callbacks. Data is from the nycflights13 dataset.'),
  tags$p(
    'Read more about echarts4r callbacks at ',
    tags$a(
      'https://echarts4r.john-coene.com/articles/shiny.html',
      href = 'https://echarts4r.john-coene.com/articles/shiny.html'
    )
  ),
  tags$hr(),
  fluidRow(
    column(8, echarts4rOutput('chart')),
    column(4, 
     tags$h4('Callbacks'),
     textOutput('brush'),
     textOutput('legend_change'),
     textOutput('legend_selected'),
     textOutput('clicked_data'),
     textOutput('clicked_data_value'),
     textOutput('clicked_row'),
     textOutput('clicked_serie'),
     textOutput('mouseover_data'),
     textOutput('mouseover_data_value'),
     textOutput('mouseover_row'),
     textOutput('mouseover_serie')   
    )
  ),
  tags$p('Click on a data point to filter table by date'),
  actionButton(
    inputId = 'reset_filter', 
    label = 'Reset filter',
    icon = icon('redo')
  ),
  tags$h3(textOutput('filter_title')),
  DTOutput('dt')
)

server <- function(input, output, session) {
  
  # Track the filter date in a reactive value so it can be
  # update independently of the chart
  filter_date <- reactiveVal(value = NULL)
  
  # Update the filter date on click on data point
  observeEvent(input$chart_clicked_data, {
    filter_date(input$chart_clicked_data$value[1])
  })
  
  # Reset the filter date
  observeEvent(input$reset_filter, {
    filter_date(NULL)
  })
  
  # Display the filter date
  output$filter_title <- renderText({
    if (is.null(filter_date())) return('Showing all flights')
    paste('Showing flights on', filter_date())
  })
  
  # Create an interactive chart
  output$chart <- renderEcharts4r({
    data %>% 
      count(date) %>% 
      e_charts(date) %>% 
      e_line(
        serie = n, 
        name = 'Number of flights',
        symbolSize = 12
      ) %>% 
      e_tooltip(axisPointer = list(type = 'cross')) %>% 
      e_axis_labels(y = 'Number of flights')
  })
  
  # Create a table with detailed information
  output$dt <- renderDT({
    if (!is.null(filter_date())) {
      data <- data %>% filter(date == filter_date())
    }
    data <- data %>% select(time_hour, carrier, tailnum, origin, dest) 
    datatable(
      data, 
      selection = 'none', 
      rownames = FALSE,
      options = list(dom = 'tip') # select table elements to show
    )
  })
  
  # Show the input values of the callbacks
  output$brush <- renderText(paste('brush:', input$chart_brush))
  output$legend_change <- renderText(paste('legend_change:', input$chart_legend_change))
  output$legend_selected <- renderText(paste('legend_selected:', input$chart_legend_selected))
  output$clicked_data <- renderText(paste('clicked_data:', input$chart_clicked_data))
  output$clicked_data_value <- renderText(paste('clicked_data_value:', input$chart_clicked_data_value))
  output$clicked_row <- renderText(paste('clicked_row:', input$chart_clicked_row))
  output$clicked_serie <- renderText(paste('clicked_serie:', input$chart_clicked_serie))
  output$mouseover_data <- renderText(paste('mouseover_data:', input$chart_mouseover_data))
  output$mouseover_data_value <- renderText(paste('mouseover_data_value:', input$chart_mouseover_data_value))
  output$mouseover_row <- renderText(paste('mouseover_row:', input$chart_mouseover_row))
  output$mouseover_serie <- renderText(paste('mouseover_serie:', input$chart_mouseover_serie))
}

shinyApp(ui, server)