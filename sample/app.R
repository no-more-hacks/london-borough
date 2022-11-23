#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source(file = "001_explore_data.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("London Borough post-COVID mobility data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( 
          
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
        scale_fill_manual(values =  neat_metrics_colours)
    } else {
      print("no data selected")
    }
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)
