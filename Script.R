#Install and load all required packages
install.packages("ggplot")
install.packages("dplyr")
install.packages("shiny")
install.packages("plotly")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("forcats")

library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(tidyverse)
library(hrbrthemes)
library(forcats)


#Load dataset from github

surv  <- 
  read.csv("https://raw.githubusercontent.com/skylarnguyen1/Nguyen-Analysis-Project/refs/heads/main/data/mrsa_surv.csv")

mlst <- read.csv("mrsa_mlst.csv")

#Transform data to include desired values

variable <- c("Age", "Epi class", "Race", "Sex", "PFGE type: All cases",
              "Syndromes")

case_rate <-  surv |>
  filter(ViewBy == "All cases", Topic == "Case rates")

death_rate <-  surv |>
  filter(ViewBy == "All cases", Topic == "Death rates")

mrsa_age <- surv |>
  filter(ViewBy == "Age", Series != "< 1 year", Series != "> 65 years",
         Series != "All cases")

mrsa_class <- surv |>
  filter(ViewBy == "Epi class", Series != "All cases", Topic != "Death rates")

#Remove all unnecessary rows 

surv_sort <- surv |>
  filter(Series != "All cases", Topic != "Death rates", ViewBy != "Dialysis",
         ViewBy != "PFGE type: Community-associated",
         ViewBy != "PFGE type: Healthcare-associated community-onset",
         ViewBy != "PFGE type: Hospital-onset")


#Preliminary plots, making sure the data set works with ggplot

ggplot(data=case_rate, aes(x=YearName, y=Value)) +
  geom_line(color="blue", size=1) +
  geom_point(shape=21, color="blue", fill="blue", size=2) +
  labs(title = "Number of MRSA cases by from 2005-2020",
       x="\nYear", y="Cases per 100,000") + theme_bw()

ggplot(data=death_rate, aes(x=YearName, y=Value)) +
  geom_line(color="blue", size=1) +
  geom_point(shape=21, color="blue", fill="blue", size=2) + 
  labs(title = "Number of MRSA-related deaths cases by from 2005-2020",
       x="\nYear", y="Cases per 100,000")

ggplot(data=mlst, aes(x=clonal_complex..MLST.)) + geom_bar()

library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(tidyverse)
library(hrbrthemes)
library(forcats)

#ShinyApp User Interface

ui <- fluidPage(
  sliderInput("time", "Year Selected:",
              min = 2005, max = 2020,
              value = 0),
  selectInput(inputId = "variable", "Choose variable to analyze",
              choices = variable, selected = NULL),
  plotOutput("plot")
)

#ShinyApp code

server <- function(input, output, session){
  my_data <- reactive(surv_sort %>% filter(YearName == input$time,
                                           ViewBy == input$variable))
  
  output$plot <- renderPlot({
    my_data() |>
      ggplot(aes(x= fct_inorder(Series), y= Value, fill= Series)) +
      geom_col() +
      theme_bw()+ 
      labs(title = "Number of MRSA-related Cases", y = "Number of Cases",
           x = input$variable)
  })
}

shinyApp(ui,server)
  