#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
dat <- read_csv("https://rdpeng.github.io/MAAIT/maait.csv")
dat <- dat %>%
  mutate(VisitNum=factor(VisitNum,
                         levels=c(0,1,2,3,4), 
                         labels=c("Baseline",
                                  "3 Months",
                                  "6 Months",
                                  "9 Months",
                                  "12 Months")))
dat <- dat %>%
  mutate(gender=factor(gender,
                       levels=c(1,2),
                       labels=c("Male",
                                "Female")))
dat <- dat %>%
  mutate(group=as.factor(group))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$symptomPlot <- renderPlot({
    
    if (input$color) {
      dat %>% ggplot(aes_string(x=input$covariate,
                                y=input$outcome,
                                color=input$colorBy)) +
        geom_point(alpha=0.3) +
        geom_smooth(method="gam", formula=y~s(x), alpha=0.1) +
        theme_bw() 
      
    } else {
      dat %>% ggplot(aes_string(x=input$covariate,
                                y=input$outcome)) +
        geom_point(alpha=0.3) +
        geom_smooth(method="gam", formula=y~s(x), alpha=0.1) +
        theme_bw()
    }
    
  })
  
})
