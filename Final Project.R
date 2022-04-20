## New Version of Final Project

# Load the necessary library packages
library(tidyverse)
library(readxl)
library(shiny)
library(RColorBrewer)


mlb_df <- read_xlsx(path = "data/MLBPlayerSalaries.xlsx")

ui <- fluidPage(h1(strong("MLB Dataset")),
  sidebarLayout(
    sidebarPanel(
                 selectizeInput(inputId = "TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlb_df$Team)),
                                selected = "Los Angeles Angels",
                                multiple = TRUE),
                 selectizeInput(inputId = "YearChoice",
                             label = "Choose a Year",
                             choices = levels(factor(mlb_df$Year)),
                             selected = "1988"),
                 sliderInput(inputId = "TopSlice",
                             label = "Choose the Top Slice for Salary Plot",
                             min = 1,
                             max = 30,
                             value = 1),
                  sliderInput(inputId = "BottomSlice",
                              label = "Choose the Bottom Slice for Salary Plot",
                              min = 1,
                              max = 30,
                              value = 20)),
                 #selectizeInput(inputID = "ThemeChoice",
                                #label = "Choose a Theme",
                                #choices = c(theme_bw(), theme_classic(),
                                            #theme_dark(), theme_gray(),
                                            #theme_linedraw(), theme_light(),
                                            #theme_minimal()),
                                #selected = theme_bw())),
    mainPanel(plotOutput("colgraph"),
              plotOutput("graphposition"))
  
))

server <- function(input, output, session) {
  mlbsalary_sub <- reactive({
    mlb_df %>% filter(.,
                      Year == input$YearChoice,
                      Team == input$TeamChoice) %>%
      mutate(.,
             Player = fct_reorder(Player, Salary),
             Salary = Salary/1000000) %>%
      slice(input$TopSlice:input$BottomSlice)
  })
  
  mlbpositions_sub <- reactive({
    mlb_df %>% filter(.,
                      Year == input$YearChoice,
                      Team == input$TeamChoice) %>%
      group_by(Team, Position) %>%
      summarize(.,
                nPlayers = n())
  })
  
  
  col_plot <- reactive({
    ggplot(data = mlbsalary_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(aes(fill = Team), color = "black") + 
      geom_hline(aes(yintercept = mean(Salary)), color = "black", linetype = 2, size = 1.25) + coord_flip() + 
      ylab("Salary per Million") + theme_bw() + scale_fill_brewer(palette = "Set2")
  })
  
  graph_plot <- reactive({
    ggplot(data = mlbpositions_sub(),
           aes(x = fct_reorder(Position, nPlayers), y = nPlayers)) + geom_bar(aes(fill = Team), color = "black", stat = "identity", position = "dodge") + 
      coord_flip() + ylab("Number of Players") + xlab("Positions") + theme_bw() + scale_fill_brewer(palette = "Set2")
   
  })
  
  
  output$colgraph <- renderPlot({
    col_plot()
  })
  
  output$graphposition <- renderPlot({
    graph_plot()
  })
  
}

shinyApp(ui, server)