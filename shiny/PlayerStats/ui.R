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
  titlePanel("Mouse Allergen and Asthma Data Visualizer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("outcome",
                   label="Select Outcome of Interest",
                   choices=c("sxsgeneral",
                             "sxsslowed",
                             "sxsspeech",
                             "sxsruncnt",
                             "sxscoughcnt",
                             "sxsmaxday"),
                   selected="sxsmaxday"
                   ),
       selectInput("covariate",
                   label="Select Input of Interest",
                   choices=c("preFEVpp",
                             "preFEVFVC",
                             "dmouseb",
                             "dmouseblog2",
                             "dmousef",
                             "dmouseflog2",
                             "airmouse",
                             "airmouselog2"),
                   selected="dmouseb"
                   ),
       checkboxInput("color",
                     label="Add Color",
                     value=FALSE),
       selectInput("colorBy",
                   label="Color By",
                   choices=c("group",
                             "VisitNum",
                             "gender"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("symptomPlot")
    )
  )
))
