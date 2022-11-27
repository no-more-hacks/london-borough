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
          actionButton("minimal_boroughs", label = "set\nminimal\nboroughs"),
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
                      tabPanel("Summary Data", 
                               shiny::sliderInput(inputId = "date_select_min",
                                                  min = min(tidy$date),
                                                  max = max(tidy$date), 
                                                  value =min(tidy$date), 
                                                  label = "choose date min:"),
                               shiny::sliderInput(inputId = "date_select_max",
                                                  min = min(tidy$date),
                                                  max = max(tidy$date), 
                                                  value =max(tidy$date), 
                                                  label = "choose date max:"),
                               actionButton("first_lockdown", label = "First Lockdown up to 'Rule Of Six' (May 2020)"),
                               actionButton("second_lockdown", label = "Second Lockdown (Oct 2020)"),
                               
                               actionButton("third_lockdown", label = "Third Lockdown (Jan 2021)"),
                               actionButton("post_lockdown", label = "After school reopening (March 2021)"),
                               tableOutput("table"), 
                               plotOutput("summary_stats", height = 600), height=1200)
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
  
  observeEvent(input$first_lockdown, { 
    
    updateSliderInput(session, inputId = "date_select_min", value = ymd("2020-03-23"))
    updateSliderInput(session, inputId = "date_select_max", value = ymd("2020-05-28"))
  })
  
  
  observeEvent(input$second_lockdown, { 
    
    updateSliderInput(session, inputId = "date_select_min", value = ymd("2020-10-31"))
    updateSliderInput(session, inputId = "date_select_max", value = ymd("2020-11-05"))
  })
  
  
  observeEvent(input$third_lockdown, { 
    updateSliderInput(session, inputId = "date_select_min", value = ymd("2021-01-04"))
    updateSliderInput(session, inputId = "date_select_max", value = ymd("2021-03-08"))
  })
  
  
  
  observeEvent(input$post_lockdown, { 
    updateSliderInput(session, inputId = "date_select_min", value = ymd("2021-05-17"))
    updateSliderInput(session, inputId = "date_select_max", value = max(filtered_data()$date))
  })
  
  
  

  
  output$summary_stats <- renderPlot({
    max_date <- input$date_select_max
    min_date <- input$date_select_min
    
    if(nrow(filtered_data()) !=0){
      cropped <- filtered_data() %>% 
        filter(date <= max_date, date >= min_date) %>% 
        group_by(metric_name, area_name) %>% 
        summarise(median = median(metric_value, na.rm= T),
                  mean = mean(metric_value, na.rm= T),
                  upper_quintile = quantile(metric_value,probs =  0.8, na.rm = T),
                  lower_quintile = quantile(metric_value,probs =  0.2, na.rm = T),
                  max = max(metric_value, na.rm= T),
                  min = min(metric_value, na.rm= T)
                  ) %>%
        # this is a bug / hack, can't get the scales to flip properly so cropping to 100%
        mutate(across(.cols = where(is_numeric), .fns = function(x){case_when(abs(x)>=100 ~ sign(x)*100, TRUE ~ x)}))
  
      
      cropped %>% 
        ggplot(mapping = aes(x = area_name)) + 
        geom_point(aes(y = mean, colour = metric_name, size = 3)) + 
        geom_point(aes(y = median), colour = "black", alpha = 0.4)  + 
        geom_errorbar(mapping = aes(y = median, ymin = lower_quintile, ymax = upper_quintile), alpha = 0.4, size = 2) + 
        geom_linerange(mapping = aes(y = median, ymin = min, ymax = max), alpha = 0.3) + 
        facet_wrap(~metric_name) + 
        scale_color_manual(values =  neat_metrics_colours, breaks = names(neat_metrics_colours), labels = names(neat_metrics)) +
        theme(legend.position = "none") +
        scale_y_continuous(limits  = c(-100,100), name = "% change") +
        theme_minimal() +
        theme(strip.text = element_text(size = 18), 
              axis.text.x = element_text(size = 18),
              axis.text.y = element_text(size = 18)) +
      coord_flip() 
      
    } else {
      print("no data selected")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
