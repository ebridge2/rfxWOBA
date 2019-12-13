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
       actionButton("randomPlayer",
                    label="Get Random Player")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("rfxwOBA.Plots")
    )
  )
))
