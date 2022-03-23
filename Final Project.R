# Final Project for Data Visualization Class

# Load the necessary library packages
library(tidyverse)
library(readxl)
library(shiny)


mlbsalary_df <- read_xlsx(path = "data/MLBPlayerSalaries.xlsx")

mlbsalary_late80s <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1988" | Year == "1989") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_early90s <-
  mlbsalary_df %>% 
  filter(.,
         Year == "1990" | Year == "1991" | Year == "1992" | Year == "1993" |
           Year == "1994") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_late90s <-
  mlbsalary_df %>% 
  filter(.,
         Year == "1995" | Year == "1996" | Year == "1997" | Year == "1998" |
           Year == "1999")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_early00s <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2000" | Year == "2001" | Year == "2002" | Year == "2003" |
           Year == "2004")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_late00s <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2005" | Year == "2006" | Year == "2007" | Year == "2008" |
           Year == "2009")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_early10s <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2010" | Year == "2011")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)



# Create the shiny app using mlbsalary_df

ui <- fluidPage(
  tabsetPanel(header = "MLB Data Set",
    tabPanel("1988-1989", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_late80s$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_late80s")))),
    tabPanel("1990-1994",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_early90s$Team)),
                                selected = "Detroit Tigers")),
               mainPanel(
                 plotOutput("colgraph_early90s")))),
    tabPanel("1995-1999",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_late90s$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_late90s")))),
    tabPanel("2000-2004",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_early00s$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_early00s")))),
    tabPanel("2005-2009",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_late00s$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_late00s")))),
    tabPanel("2010-2011",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_early10s$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_early10s"))))))

server <- function(input, output, session) {
  
  mlbsalary_late80s_sub <- reactive({
    mlbsalary_late80s %>% filter(Team == input$TeamChoice)
  })
  
  mlbsalary_early90s_sub <- reactive({
    mlbsalary_early90s %>% filter(Team == input$TeamChoice)
  })
  
  mlbsalary_late90s_sub <- reactive({
    mlbsalary_late90s %>% filter(Team == input$TeamChoice)
  })
  
  mlbsalary_early00s_sub <- reactive({
    mlbsalary_early00s %>% filter(Team == input$TeamChoice)
  })
  
  mlbsalary_late00s_sub <- reactive({
    mlbsalary_late00s %>% filter(Team == input$TeamChoice)
  })
  
  mlbsalary_early10s_sub <- reactive({
    mlbsalary_early10s %>% filter(Team == input$TeamChoice)
  })
  
  
  
  col_plot_late80s <- reactive({
    ggplot(data = mlbsalary_late80s_sub(),
           aes(x = Player,
               y = Salary)) + geom_col() + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_early90s <- reactive({
    ggplot(data = mlbsalary_early90s_sub(),
           aes(x = Player,
               y = Salary)) + geom_col() + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_late90s <- reactive({
    ggplot(data = mlbsalary_late90s_sub(),
           aes(x = Player,
               y = Salary)) + geom_col() + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_early00s <- reactive({
    ggplot(data = mlbsalary_early00s_sub(),
           aes(x = Player,
               y = Salary)) + geom_col() + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_late00s <- reactive({
    ggplot(data = mlbsalary_late00s_sub(),
           aes(x = Player,
               y = Salary)) + geom_col() + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_early10s <- reactive({
    ggplot(data = mlbsalary_early10s_sub(),
           aes(x = Player,
               y = Salary)) + geom_col() + coord_flip() + 
      ylab("Salary per Million")
  })
  
  
  
  output$colgraph_late80s <- renderPlot({
    col_plot_late80s()
  })
  
  output$colgraph_early90s <- renderPlot({
    col_plot_early90s()
  })
  
  output$colgraph_late90s <- renderPlot({
    col_plot_late90s()
  })
  
  output$colgraph_early00s <- renderPlot({
    col_plot_early00s()
  })
  
  output$colgraph_late00s <- renderPlot({
    col_plot_late00s()
  })
  
  output$colgraph_early10s <- renderPlot({
    col_plot_early10s()
  })
  
}

shinyApp(ui, server)


