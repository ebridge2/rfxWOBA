#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("rfxwOBA 2017-2019"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("player.name",
                 label="Input Player Name",
                 value="Mike Trout"),
       selectInput("plotToDisplay",
                   label="Select Plot",
                   choices=c("2017 rfxwOBA",
                             "2018 rfxwOBA",
                             "2019 rfxwOBA",
                             "Rolling Values"),
                   selected="2017 rfxwOBA"),
       actionButton("randomPlayer",
                    label="Get Random Player")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("rfxwOBA.Plots"),
       textOutput("description1"),
       textOutput("description2"),
       textOutput("description3"),
       textOutput("description4")
    )
  )
))
