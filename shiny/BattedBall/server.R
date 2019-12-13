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
library(gridExtra)
library(rerf)
library(data.table)
library(mltools)
library(kableExtra)
library(parallel)


fit <- read_rds("../../data/rf/fit_model.rds")

X <- read_rds("../../data/shiny/model_dat.rds")

descriptions <- read_rds("../../data/shiny/descriptions.rds")

idx.cc <- which(complete.cases(X))


out <- c("Out", "Single", "Double", "Triple", "Home run")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
    
  
  output$ballPlot <- renderPlot({
    
    input$plot
    
    isolate(
    
    if (input$realPlay) {
      idx <- sample(idx.cc, size=1)
      dat <- X[idx,]
      batterStand <- ifelse(dat$batterRighty, "Right", "Left")
      updateSelectInput(session, "batterStand", selected=batterStand)
      updateNumericInput(session, "launchSpeed", value=dat$launch_speed)
      updateNumericInput(session, "launchAngle", value=dat$launch_angle)
      updateNumericInput(session, "sprayAngle", value=dat$spray_angle)
      updateNumericInput(session, "sprintSpeed", value=dat$sprint_speed)
      plt1 <- dat %>% ggplot(aes(x=-(launch_angle+90), y=launch_speed)) +
        geom_point() +
        coord_polar(theta="x") +
        scale_x_continuous(limits = c(-180, 180),
                           breaks = c(-180, -135, -90, -45, 0),
                           labels = c(90, 45, 0, -45, -90)) +
        scale_y_continuous(limits = c(0, 125)) +
        labs(x="Launch Angle",
             y="Launch Speed") 
      foul.lines <- data.frame(lf=c(315,315),
                               rf=c(45,45),
                               launch_speed=c(0,120))
      dat.plt <- dat %>% mutate(spray_angle2=ifelse(spray_angle<0, 360+spray_angle, spray_angle))
      plt2 <- dat.plt %>% ggplot(aes(x=spray_angle2, y=launch_speed)) +
        geom_line(data=foul.lines, aes(x=lf, y=launch_speed), color="black") +
        geom_line(data=foul.lines, aes(x=rf, y=launch_speed), color="black") +
        geom_point() + 
        scale_x_continuous(limits = c(0, 360), 
                           breaks=c(0, 45, 315), 
                           labels=c(0, 45, -45)) +
        scale_y_continuous(limits = c(0, 125)) +
        coord_polar(theta="x") +
        labs(title="",
             x="Spray Angle",
             y="Launch Speed")
      p <- Predict(dat, fit, num.cores=1L, output.scores = TRUE)
      tbl1 <- data.frame(Outcome=out, Probability=t(p)) %>%
        tableGrob()
      tbl2 <- data.frame(Prediction=out[which.max(p)]) %>%
        tableGrob()
      grid.arrange(plt1, plt2, tbl1, tbl2, ncol=2)
      output$description <- renderText({
        paste(descriptions$des[idx])
      })
      
      
    } else {
      
       idx <- sample(idx.cc, size=1)
       dat <- X[idx,]
       dat <- dat %>%
         mutate(launch_speed=input$launchSpeed,
                launch_angle=input$launchAngle,
                spray_angle=input$sprayAngle)
       plt1 <- dat %>% ggplot(aes(x=-(launch_angle+90), y=launch_speed)) +
         geom_point() +
         coord_polar(theta="x") +
         scale_x_continuous(limits = c(-180, 180),
                            breaks = c(-180, -135, -90, -45, 0),
                            labels = c(90, 45, 0, -45, -90)) +
         scale_y_continuous(limits = c(0, 125)) +
         labs(x="Launch Angle",
              y="Launch Speed") 
       foul.lines <- data.frame(lf=c(315,315),
                                rf=c(45,45),
                                launch_speed=c(0,120))
       dat.plt <- dat %>% mutate(spray_angle2=ifelse(spray_angle<0, 360+spray_angle, spray_angle))
       plt2 <- dat.plt %>% ggplot(aes(x=spray_angle2, y=launch_speed)) +
         geom_line(data=foul.lines, aes(x=lf, y=launch_speed), color="black") +
         geom_line(data=foul.lines, aes(x=rf, y=launch_speed), color="black") +
         geom_point() + 
         scale_x_continuous(limits = c(0, 360), 
                            breaks=c(0, 45, 315), 
                            labels=c(0, 45, -45)) +
         scale_y_continuous(limits = c(0, 125)) +
         coord_polar(theta="x") +
         labs(x="Spray Angle",
              y="Launch Speed")
       p <- Predict(dat, fit, num.cores=1L, output.scores = TRUE)
       tbl1 <- data.frame(Outcome=out, Probability=t(p)) %>%
         tableGrob()
       tbl2 <- data.frame(Prediction=out[which.max(p)]) %>%
         tableGrob()
       
       grid.arrange(plt1, plt2, tbl1, tbl2, ncol=2)
       output$description <- renderText({
         paste("")
       })
    }
    
    )
    
  })
  
})
