## New Version of Final Project

# Load the necessary library packages
library(tidyverse)
library(readxl)
library(shiny)
library(RColorBrewer)
library(bslib)
thematic:: thematic_shiny(font = "auto")


mlb_df <- read_xlsx(path = "data/MLBPlayerSalaries.xlsx")

ui <- fluidPage(theme = bs_theme(), 
                h1(strong("Major League Baseball App")),
  sidebarLayout(
    sidebarPanel(HTML("<p>This data presents an MLB dataset from 1988 to 2011,
                      with the top graph displaying the top players' salaries
                      from the team(s) chosen, the dotted vertical line
                      representing the players shown mean salary, and the
                      bottom graph displaying the number of players in each
                      position from the team(s) chosen.</p>"),
                 selectizeInput(inputId = "TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlb_df$Team)),
                                selected = "Los Angeles Angels",
                                multiple = TRUE,
                                options = list(maxItems = 4)),
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
    mainPanel(plotOutput("colgraph"),
              plotOutput("graphposition"))))

server <- function(input, output, session) {
  bs_themer()
  
  mlbsalary_sub <- reactive({
    mlb_df %>% filter(.,
                      Year %in% input$YearChoice,
                      Team %in% input$TeamChoice) %>%
      mutate(.,
             Player = fct_reorder(Player, Salary),
             Salary = Salary/1000000) %>%
      arrange(desc(Salary)) %>%
      slice(input$TopSlice:input$BottomSlice)
  })
  
  mlbpositions_sub <- reactive({
    mlb_df %>% filter(.,
                      Year %in% input$YearChoice,
                      Team %in% input$TeamChoice) %>%
      group_by(Team, Position) %>%
      summarize(.,
                nPlayers = n())
  })
  
  
  col_plot <- reactive({
    ggplot(data = mlbsalary_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(aes(fill = Team), color = "black") + 
      geom_hline(aes(yintercept = mean(Salary)), color = "black", linetype = 2, size = 1.25) + coord_flip() + 
      labs(title = "Top Salaries for Select Year and Teams", 
           y = "Salary (per Million Dollars)",
           fill = "Team(s) Chosen") + theme_bw() + scale_fill_brewer(palette = "Set2") + theme(text = element_text(size = 20),
                                                                                                  axis.text.x = element_text(size = 18),
                                                                                                  axis.text.y = element_text(size = 18)) 
  })
  
  graph_plot <- reactive({
    ggplot(data = mlbpositions_sub(),
           aes(x = fct_reorder(Position, nPlayers), y = nPlayers)) + geom_bar(aes(fill = Team), color = "black", stat = "identity", position = "dodge") + 
      coord_flip() + labs(x = "Positions",
                          y = "Number of Players",
                          title = "Number of Players in MLB Positions",
                          fill = "Team(s) Chosen") + theme_bw() + scale_fill_brewer(palette = "Set2") + theme(text = element_text(size = 20),
                                                                                                                              axis.text.x = element_text(size = 18),
                                                                                                                              axis.text.y = element_text(size = 18)) +
      scale_y_continuous(breaks = seq(0, 16, 2))
                                                                                                  
   
  })
  
  
  output$colgraph <- renderPlot({
    col_plot()
  })
  
  output$graphposition <- renderPlot({
    graph_plot()
  })
  
}

shinyApp(ui, server)