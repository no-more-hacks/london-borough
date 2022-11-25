#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if (!shiny::serverInfo()$shinyServer) {
  source(file = "001_explore_data.R")
} else {

}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("London Borough post-COVID mobility data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( 
          actionButton("minimal_boroughs", label = "set minimal"),
          checkboxGroupInput(inputId = "metrics",
                       label = "Metric to plot",
                       choices = neat_metrics, 
                       selected = neat_metrics
                       ),
          checkboxGroupInput(inputId = "boroughs",
                             label = "Boroughs to plot",
                             choices = raw %>% pull(area_name) %>% unique(), 
                             selected = raw %>% pull(area_name) %>% unique(),
          ), width = 1
        ),

        # Show a plot of the generated distribution
        mainPanel(  width = 9,
                    tabsetPanel(
                      tabPanel("Timeline plot", 
                              checkboxInput(inputId = "smooth", label = "Smooth data over time?", value = FALSE),
                              plotOutput("timePlot", height = 1200)),
                      tabPanel("Histogram", "caution: scales vary with selected data", plotOutput("histogram", height = 1200)),
                      tabPanel("Pairs plot", plotOutput("pairs_plot", height = 1200), height=1200),
                      tabPanel("Table", tableOutput("table"))
                    )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  
  filtered_data <- reactive({
    filtered_data <- tidy %>% 
      filter(area_name %in% input$boroughs) %>% 
      filter(metric_name %in% input$metrics)
  })

  output$timePlot <- renderPlot({
    if(nrow(filtered_data()) !=0){
      
      if(input$smooth) {
        time_data <- filtered_data() %>% 
          group_by(area_name, metric_name) %>% 
          mutate(metric_value = zoo::rollmean(metric_value, k = 10, fill = c(NA, NA, NA)))
      } else {
        time_data <- filtered_data() 
      }
      
      time_data %>% 
        ggplot(aes(x = date, y = metric_value, colour = metric_name)) +
          geom_line() +
          facet_wrap(~area_name) + 
          theme(legend.position = "top") +
        scale_fill_manual(values =  neat_metrics_colours, 
                          breaks = names(neat_metrics_colours), 
                          labels = names(neat_metrics))
    } else {
      print("no data selected")
    }
    
  })
  
  observeEvent(input$minimal_boroughs, { 
    updateCheckboxGroupInput(session, inputId = "boroughs", 
                             selected = c(
                               # two residential
                               "Lewisham", "Ealing",
                               # two work places
                                "City of London", "Westminster"))
  })

  
  output$histogram <- renderPlot({
    if(nrow(filtered_data()) !=0){
      filtered_data() %>% 
        ggplot(aes(x = metric_value, colour = metric_name)) +
        geom_density() +
        facet_wrap(~area_name, scales = "free") + 
        theme(legend.position = "top")  +
        scale_fill_manual( neat_metrics_colours)
    } else {
      print("no data selected")
    }
    
  })
  
  output$pairs_plot <- renderPlot({
    if(nrow(filtered_data()) !=0){
      xx <- filtered_data()  %>% 
        pivot_wider(names_from = metric_name, 
                    values_from = metric_value)
      
      GGally::ggpairs(xx %>% 
                        select(area_name, matches("percent")), 
                      columns = colnames(
                        xx %>% 
                          select(matches("percent"))), ggplot2::aes(alpha = 0.1, colour = area_name))
    } else {
      print("no data selected")
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
