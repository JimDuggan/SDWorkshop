library(shiny)
library(aimsir17)
library(ggplot2)
library(glue)
library(dplyr)
library(tidyr)

# Load the model function
source("Model.R")

ui <- fluidPage(
  titlePanel("Simulating the SIRH Model"),
  sliderInput("con", "Contacts:",
              min = 2, max = 20, value = 6
  ),
  sliderInput("vf", "Vaccination Fraction:",
              min = 0, max = 0.15, value = .02
  ),
  plotOutput("sim_output")
)

server <- function(input, output, session){
  message("\nStarting the server...")
  output$sim_output <- renderPlot({
    message("\nreacting to input control change...")
    sim <- run_sirh(contacts = input$con,
                    VF = input$vf) 
    sim <- sim %>%
             select(time,I,H) %>%
             pivot_longer(cols = -time,
                          names_to = "Variable",
                          values_to = "Value")
    
    ggplot(sim,aes(x=time,y=Value,colour=Variable))+
      geom_point()+geom_line()+
      facet_wrap(~Variable,nrow = 2,scales="free")
  })
}

shinyApp(ui, server)
