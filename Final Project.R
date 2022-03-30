# Final Project for Data Visualization Class

# Load the necessary library packages
library(tidyverse)
library(readxl)
library(shiny)


mlbsalary_df <- read_xlsx(path = "data/MLBPlayerSalaries.xlsx")

mlbsalary_88 <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1988") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_89 <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1989") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_90 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "1990") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_91 <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1991") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_92 <-
  mlbsalary_df %>%
  filter(.,
         Year == "1992") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_93 <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1993") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_94 <-
  mlbsalary_df %>%
  filter(.,
         Year == "1994") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_95 <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1995") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_96 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "1996")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_97 <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1997") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_98 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "1998")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_99 <- 
  mlbsalary_df %>%
  filter(.,
         Year == "1999") %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)
  
mlbsalary_00 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2000")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_01 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2001")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_02 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2002")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_03 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2003")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_04 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2004")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_05 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2005")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_06 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2006")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_07 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2007")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_08 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2008")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_09 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2009")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_10 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2010")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)

mlbsalary_11 <-
  mlbsalary_df %>% 
  filter(.,
         Year == "2011")  %>%
  mutate(.,
         Player = fct_reorder(Player, Salary),
         Salary = Salary/1000000)


# Create the shiny app using mlbsalary_df

ui <- fluidPage(h1("MLB Data Set"),
  tabsetPanel(
    tabPanel("1988", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice88",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_88$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_88"),
                 plotOutput("graphposition_88")))),
    tabPanel("1989", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice89",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_89$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_89"),
                 plotOutput("graphposition_89")))),
    tabPanel("1990",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice90",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_90$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_90"),
                 plotOutput("graphposition_90")))),
    tabPanel("1991",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice91",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_91$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_91"),
                 plotOutput("graphposition_91")))),
    tabPanel("1992",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice92",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_92$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_92"),
                 plotOutput("graphposition_92")))),
    tabPanel("1993",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice93",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_93$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_93"),
                 plotOutput("graphposition_93")))),
    tabPanel("1994",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice94",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_94$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_94"),
                 plotOutput("graphposition_94")))),
    tabPanel("1995",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice95",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_95$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_95"),
                 plotOutput("graphposition_95")))),
    tabPanel("1996",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice96",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_96$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_96"),
                 plotOutput("graphposition_96")))),
    tabPanel("1997",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice97",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_97$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_97"),
                 plotOutput("graphposition_97")))),
    tabPanel("1998",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice98",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_98$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_98"),
                 plotOutput("graphposition_98")))),
    tabPanel("1999",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice99",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_99$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_99"),
                 plotOutput("graphposition_99")))),
    tabPanel("2000",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice00",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_00$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_00"),
                 plotOutput("graphposition_00")))),
    tabPanel("2001",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice01",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_01$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_01"),
                 plotOutput("graphposition_01")))),
    tabPanel("2002",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice02",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_02$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_02"),
                 plotOutput("graphposition_02")))),
    tabPanel("2003",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice03",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_03$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_03"),
                 plotOutput("graphposition_03")))),
    tabPanel("2004",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice04",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_04$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_04"),
                 plotOutput("graphposition_04")))),
    tabPanel("2005",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice05",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_05$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_05"),
                 plotOutput("graphposition_05")))),
    tabPanel("2006",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice06",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_06$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_06"),
                 plotOutput("graphposition_06")))),
    tabPanel("2007",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice07",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_07$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_07"),
                 plotOutput("graphposition_07")))),
    tabPanel("2008",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice08",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_08$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_08"),
                 plotOutput("graphposition_08")))),
    tabPanel("2009",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice09",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_09$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_09"),
                 plotOutput("graphposition_09")))),
    tabPanel("2010",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice10",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_10$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_10"),
                 plotOutput("graphposition_10")))),
    tabPanel("2011",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("TeamChoice11",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_11$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_11"),
                 plotOutput("graphposition_11")))),
    tabPanel("Customize Year",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(inputId = "ChooseYear",
                                label = "Choose a Year Range",
                                format = "yyyy",
                                startview = "year",
                                min = "1988-01-01",
                                max = "2011-01-01",
                                start = "1988-01-01",
                                end = "2011-01-01"),
                 selectizeInput("TeamChoiceCust",
                                label = "Choose a MLB Team",
                                choices = levels(
                                  factor(mlbsalary_df$Team)),
                                selected = "Los Angeles Angels")),
               mainPanel(
                 plotOutput("colgraph_cust"),
                 plotOutput("graphposition_cust"))))))

server <- function(input, output, session) {
  
  mlbsalary_88_sub <- reactive({
    mlbsalary_88 %>% filter(Team == input$TeamChoice88)
  })
  
  mlbsalary_89_sub <- reactive({
    mlbsalary_89 %>% filter(Team == input$TeamChoice89)
  })
  
  mlbsalary_90_sub <- reactive({
    mlbsalary_90 %>% filter(Team == input$TeamChoice90)
  })
  
  mlbsalary_91_sub <- reactive({
    mlbsalary_91 %>% filter(Team == input$TeamChoice91)
  })
  
  mlbsalary_92_sub <- reactive({
    mlbsalary_92 %>% filter(Team == input$TeamChoice92)
  })
  
  mlbsalary_93_sub <- reactive({
    mlbsalary_93 %>% filter(Team == input$TeamChoice93)
  })
  
  mlbsalary_94_sub <- reactive({
    mlbsalary_94 %>% filter(Team == input$TeamChoice94)
  })
  
  mlbsalary_95_sub <- reactive({
    mlbsalary_95 %>% filter(Team == input$TeamChoice95)
  })
  
  mlbsalary_96_sub <- reactive({
    mlbsalary_96 %>% filter(Team == input$TeamChoice96)
  })
  
  mlbsalary_97_sub <- reactive({
    mlbsalary_97 %>% filter(Team == input$TeamChoice97)
  })
  
  mlbsalary_98_sub <- reactive({
    mlbsalary_98 %>% filter(Team == input$TeamChoice98)
  })
  
  mlbsalary_99_sub <- reactive({
    mlbsalary_99 %>% filter(Team == input$TeamChoice99)
  })
  
  mlbsalary_00_sub <- reactive({
    mlbsalary_00 %>% filter(Team == input$TeamChoice00)
  })
  
  mlbsalary_01_sub <- reactive({
    mlbsalary_01 %>% filter(Team == input$TeamChoice01)
  })
  
  mlbsalary_02_sub <- reactive({
    mlbsalary_02 %>% filter(Team == input$TeamChoice02)
  })
  
  mlbsalary_03_sub <- reactive({
    mlbsalary_03 %>% filter(Team == input$TeamChoice03)
  })
  
  mlbsalary_04_sub <- reactive({
    mlbsalary_04 %>% filter(Team == input$TeamChoice04)
  })
  
  mlbsalary_05_sub <- reactive({
    mlbsalary_05 %>% filter(Team == input$TeamChoice05)
  })
  
  mlbsalary_06_sub <- reactive({
    mlbsalary_06 %>% filter(Team == input$TeamChoice06)
  })
  
  mlbsalary_07_sub <- reactive({
    mlbsalary_07 %>% filter(Team == input$TeamChoice07)
  })
  
  mlbsalary_08_sub <- reactive({
    mlbsalary_08 %>% filter(Team == input$TeamChoice08)
  })
  
  mlbsalary_09_sub <- reactive({
    mlbsalary_09 %>% filter(Team == input$TeamChoice09)
  })
  
  mlbsalary_10_sub <- reactive({
    mlbsalary_10 %>% filter(Team == input$TeamChoice10)
  })
  
  mlbsalary_11_sub <- reactive({
    mlbsalary_11 %>% filter(Team == input$TeamChoice11)
  })
  
  mlbsalary_cust_sub <- reactive({
    mlbsalary_df %>% filter(Team == input$TeamChoiceCust,
                            Year == input$ChooseYear) %>%
      slice(1:25)
  })
  

  
  col_plot_88 <- reactive({
    ggplot(data = mlbsalary_88_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_89 <- reactive({
    ggplot(data = mlbsalary_89_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_90 <- reactive({
    ggplot(data = mlbsalary_90_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_91 <- reactive({
    ggplot(data = mlbsalary_91_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_92 <- reactive({
    ggplot(data = mlbsalary_92_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_93 <- reactive({
    ggplot(data = mlbsalary_93_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_94 <- reactive({
    ggplot(data = mlbsalary_94_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_95 <- reactive({
    ggplot(data = mlbsalary_95_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_96 <- reactive({
    ggplot(data = mlbsalary_96_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_97 <- reactive({
    ggplot(data = mlbsalary_97_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_98 <- reactive({
    ggplot(data = mlbsalary_98_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_99 <- reactive({
    ggplot(data = mlbsalary_99_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_00 <- reactive({
    ggplot(data = mlbsalary_00_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_01 <- reactive({
    ggplot(data = mlbsalary_01_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_02 <- reactive({
    ggplot(data = mlbsalary_02_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_03 <- reactive({
    ggplot(data = mlbsalary_03_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_04 <- reactive({
    ggplot(data = mlbsalary_04_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_05 <- reactive({
    ggplot(data = mlbsalary_05_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_06 <- reactive({
    ggplot(data = mlbsalary_06_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_07 <- reactive({
    ggplot(data = mlbsalary_07_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_08 <- reactive({
    ggplot(data = mlbsalary_08_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_09 <- reactive({
    ggplot(data = mlbsalary_09_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_10 <- reactive({
    ggplot(data = mlbsalary_10_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_11 <- reactive({
    ggplot(data = mlbsalary_11_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() + 
      ylab("Salary per Million")
  })
  
  col_plot_cust <- reactive({
    ggplot(data = mlbsalary_cust_sub(),
           aes(x = Player,
               y = Salary)) + geom_col(color = "black", fill = "white") + coord_flip() +
      ylab("Salary per Million")
  })
  
  graph_plot_88 <- reactive({
    ggplot(data = mlbsalary_88_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_89 <- reactive({
    ggplot(data = mlbsalary_89_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_90 <- reactive({
    ggplot(data = mlbsalary_90_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_91 <- reactive({
    ggplot(data = mlbsalary_91_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_92 <- reactive({
    ggplot(data = mlbsalary_92_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_93 <- reactive({
    ggplot(data = mlbsalary_93_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_94 <- reactive({
    ggplot(data = mlbsalary_94_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_95 <- reactive({
    ggplot(data = mlbsalary_95_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_96 <- reactive({
    ggplot(data = mlbsalary_96_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_97 <- reactive({
    ggplot(data = mlbsalary_97_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_98 <- reactive({
    ggplot(data = mlbsalary_98_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_99 <- reactive({
    ggplot(data = mlbsalary_99_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_00 <- reactive({
    ggplot(data = mlbsalary_00_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_01 <- reactive({
    ggplot(data = mlbsalary_01_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_02 <- reactive({
    ggplot(data = mlbsalary_02_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_03 <- reactive({
    ggplot(data = mlbsalary_03_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_04 <- reactive({
    ggplot(data = mlbsalary_04_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_05 <- reactive({
    ggplot(data = mlbsalary_05_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_06 <- reactive({
    ggplot(data = mlbsalary_06_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_07 <- reactive({
    ggplot(data = mlbsalary_07_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_08 <- reactive({
    ggplot(data = mlbsalary_08_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_09 <- reactive({
    ggplot(data = mlbsalary_09_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_10 <- reactive({
    ggplot(data = mlbsalary_10_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })
  
  graph_plot_11 <- reactive({
    ggplot(data = mlbsalary_11_sub(),
           aes(x = Position)) + geom_histogram(color = "black", fill = "white") + 
      coord_flip() + ylab("Number of Players")
  })

  
  output$colgraph_88 <- renderPlot({
    col_plot_88()
  })
  
  output$colgraph_89 <- renderPlot({
    col_plot_89()
  })
  
  output$colgraph_90 <- renderPlot({
    col_plot_90()
  })
  
  output$colgraph_91 <- renderPlot({
    col_plot_91()
  })
  
  output$colgraph_92 <- renderPlot({
    col_plot_92()
  })
  
  output$colgraph_93 <- renderPlot({
    col_plot_93()
  })
  
  output$colgraph_94 <- renderPlot({
    col_plot_94()
  })
  
  output$colgraph_95 <- renderPlot({
    col_plot_95()
  })
  
  output$colgraph_96 <- renderPlot({
    col_plot_96()
  })
  
  output$colgraph_97 <- renderPlot({
    col_plot_97()
  })
  
  output$colgraph_98 <- renderPlot({
    col_plot_98()
  })
  
  output$colgraph_99 <- renderPlot({
    col_plot_99()
  })
  
  output$colgraph_00 <- renderPlot({
    col_plot_00()
  })
  
  output$colgraph_01 <- renderPlot({
    col_plot_01()
  })
  
  output$colgraph_02 <- renderPlot({
    col_plot_02()
  })
  
  output$colgraph_03 <- renderPlot({
    col_plot_03()
  })
  
  output$colgraph_04 <- renderPlot({
    col_plot_04()
  })
  
  output$colgraph_05 <- renderPlot({
    col_plot_05()
  })
  
  output$colgraph_06 <- renderPlot({
    col_plot_06()
  })
  
  output$colgraph_07 <- renderPlot({
    col_plot_07()
  })
  
  output$colgraph_08 <- renderPlot({
    col_plot_08()
  })
  
  output$colgraph_09 <- renderPlot({
    col_plot_09()
  })
  
  output$colgraph_10 <- renderPlot({
    col_plot_10()
  })
  
  output$colgraph_11 <- renderPlot({
    col_plot_11()
  })
  
  output$colgraph_cust <- renderPlot({
    col_plot_cust()
  })
  
  
  output$graphposition_88 <- renderPlot({
    graph_plot_88
  })
  
  output$graphposition_89 <- renderPlot({
    graph_plot_89
  })
  
  output$graphposition_90 <- renderPlot({
    graph_plot_90
  })
  
  output$graphposition_91 <- renderPlot({
    graph_plot_91
  })
  
  output$graphposition_92 <- renderPlot({
    graph_plot_92
  })
  
  output$graphposition_93 <- renderPlot({
    graph_plot_93
  })
  
  output$graphposition_94 <- renderPlot({
    graph_plot_94
  })
  
  output$graphposition_95 <- renderPlot({
    graph_plot_95
  })
  
  output$graphposition_96 <- renderPlot({
    graph_plot_96
  })
  
  output$graphposition_97 <- renderPlot({
    graph_plot_97
  })
  
  output$graphposition_98 <- renderPlot({
    graph_plot_98
  })
  
  output$graphposition_99 <- renderPlot({
    graph_plot_99
  })
  
  output$graphposition_00 <- renderPlot({
    graph_plot_00
  })
  
  output$graphposition_01 <- renderPlot({
    graph_plot_01
  })
  
  output$graphposition_02 <- renderPlot({
    graph_plot_02
  })
  
  output$graphposition_03 <- renderPlot({
    graph_plot_03
  })
  
  output$graphposition_04 <- renderPlot({
    graph_plot_04
  })
  
  output$graphposition_05 <- renderPlot({
    graph_plot_05
  })
  
  output$graphposition_06 <- renderPlot({
    graph_plot_06
  })
  
  output$graphposition_07 <- renderPlot({
    graph_plot_07
  })
  
  output$graphposition_08 <- renderPlot({
    graph_plot_08
  })
  
  output$graphposition_09 <- renderPlot({
    graph_plot_09
  })
  
  output$graphposition_10 <- renderPlot({
    graph_plot_10
  })

  
}

shinyApp(ui, server)


