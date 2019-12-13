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
  titlePanel("Predict a batted ball!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("batterStand",
                   label="Select Batter Handedness",
                   choices=c("Right",
                             "Left"),
                   selected="Right"
                   ),
       numericInput("launchSpeed",
                    label="Launch Speed",
                    value=90,
                    min=0,
                    max=150),
       numericInput("launchAngle",
                    label="Launch Angle",
                    value=0,
                    min=-90,
                    max=90),
       numericInput("sprayAngle",
                    label="Spray Angle",
                    value=0,
                    min=-80,
                    max=80),
       numericInput("sprintSpeed",
                    label="Sprint Speed",
                    value=27,
                    min=15,
                    max=40),
       checkboxInput("realPlay",
                     label="Random Real Play",
                     value=FALSE),
       actionButton("plot",
                    label="Predict!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("ballPlot"),
       textOutput("description")
    )
  )
))
