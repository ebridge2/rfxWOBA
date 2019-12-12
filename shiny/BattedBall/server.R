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


dat.full <- read_csv("../../data/cleaned/model_data.csv")
descriptions <- read_csv("../../data/raw/baseball_data.csv") %>%
  select(des)


idx.cc <- which(complete.cases(dat))


out <- c("Out", "Single", "Double", "Triple", "Home run")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$ballPlot <- renderPlot({
    
    if (input$realPlay) {
      idx <- sample(idx.cc, size=1)
      dat <- dat.full[idx,]
      batterStand <- ifelse(dat$batterRighty, "Right", "Left")
      updateSelectInput(session, "batterStand", selected=batterStand)
      updateNumericInput(session, "launchSpeed", value=dat$launch_speed)
      updateNumericInput(session, "launchAngle", value=dat$launch_angle)
      updateNumericInput(session, "sprayAngle", value=dat$lspray_angle)
      updateNumericInput(session, "sprintSpeed", value=dat$sprint_speed)
      plt1 <- dat %>% ggplot(aes(x=-(launch_angle+90), y=launch_speed)) +
        geom_point() +
        coord_polar(theta="x") +
        scale_x_continuous(limits = c(-180, 180),
                           breaks = c(-180, -135, -90, -45, 0),
                           labels = c(90, 45, 0, -45, -90)) +
        scale_y_continuous(limits = c(0, 125)) +
        labs(title=descriptions$des[idx],
             x="Launch Angle",
             y="Launch Speed") 
      foul.lines <- data.frame(lf=c(315,315),
                               rf=c(45,45),
                               launch_speed=c(0,120))
      dat <- dat %>% mutate(spray_angle2=ifelse(spray_angle<0, 360+spray_angle, spray_angle))
      plt2 <- dat %>% ggplot(aes(x=spray_angle2, y=launch_speed)) +
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
     
      tbl1 <- data.frame(Outcome=out,
                         Probability=p) %>%
        kable() %>%
        kable_styling("striped")
      tbl2 <- data.frame(Prediction=out[which.max(p)]) %>%
        kable() %>%
        kable_styling("striped")
        
      grid.arrange(plt1, plt2, ncol=2)
      
      
    } else {
      
       idx <- sample(idx.cc, size=1)
       dat <- dat.full[idx,]
       dat <- data.frame(launch_speed=input$launchSpeed,
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
       dat <- dat %>% mutate(spray_angle2=ifelse(spray_angle<0, 360+spray_angle, spray_angle))
       plt2 <- dat %>% ggplot(aes(x=spray_angle2, y=launch_speed)) +
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
       
       tbl1 <- data.frame(Outcome=out,
                          Probability=p) %>%
         kable() %>%
         kable_styling("striped")
       tbl2 <- data.frame(Prediction=out[which.max(p)]) %>%
         kable() %>%
         kable_styling("striped")
       
       grid.arrange(plt1, plt2, ncol=2)
    }
    
  })
  
})
