# Mini shiny app for customizing year problem
library(shiny)
library(tidyverse)
library(readxl)

mlbsalary_df <- read_xlsx(path = "data/MLBPlayerSalaries.xlsx")



ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    dateRangeInput(inputId = "ChooseYear",
                   label = "Choose a Year Range",
                   format = "yyyy",
                   startview = "year",
                   min = "1988-01-01",
                   max = "2011-01-31",
                   start = "1988-01-01",
                   end = "2011-01-31"),
    selectizeInput("TeamChoiceCust",
                   label = "Choose a MLB Team",
                   choices = levels(
                     factor(mlbsalary_df$Team)),
                   selected = "Los Angeles Angels")),
  mainPanel(
    plotOutput("colgraph_cust")))
  
)

server <- function(input, output, session) {
  mlbsalary_cust_sub <- reactive({
    mlbsalary_df %>% filter(Team == input$TeamChoiceCust,
                            Year == input$ChooseYear) %>%
      slice(1:15)
  })
  
  col_plot_cust <- reactive({
    ggplot(data = mlbsalary_cust_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() +
      ylab("Salary per Million")
  })
  
  output$colgraph_cust <- renderPlot({
    col_plot_cust()
  })
  
}

shinyApp(ui, server)

