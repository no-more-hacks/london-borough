# command to deploy shiny app


rsconnect::deployApp('sample', account = "sam-gardiner")


rmarkdown::render("001_explore_data.R", output_options = list(warning=FALSE)) 
