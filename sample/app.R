#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source(file = "../001_explore_data.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("London Borough post-COVID mobility data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(inputId = "metric",
                       label = "Metric to plot",
                       choices = neat_metrics
                       ),
          checkboxGroupInput(inputId = "stations",
                             label = "Stations to plot",
                             choices = raw %>% pull(area_name) %>% unique()
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          "hello", 
           plotOutput("timePlot", height = 1200)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timePlot <- renderPlot({
      
        
      tidy %>% 
        ggplot(aes(x = date, y = metric_value, colour = metric_name)) +
        geom_line() +
        facet_wrap(~area_name)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
