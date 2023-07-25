library(tidyverse)
library(ggplot2)
library(readr)
library(knitr)
library(kableExtra)
library(shiny)
library(gridExtra)
setwd("C:/Users/hdawg/OneDrive/Documents/Personal")
opts_chunk$set(echo = TRUE)

#start of creation of dataframes
fantasy_performance <- read_csv("fantasy_performance.csv") %>% 
  select(c(-'2022', -bye, -injury, -'ffl-team', -'avg-3wk'))
rushing <- read_csv("rushing.csv")
recievingAdvanced <- read_csv("recievingAdvanced.csv")
recieving <- read_csv("recieving.csv") %>% 
  mutate('drop%' = recievingAdvanced$'Drop%', YACpR = recievingAdvanced$`YAC/R`)
adp <- read_csv("adp.csv") %>% 
  select(-c(4:8))
adp$ADP <- round(adp$ADP, 1)
recieving$Pos <- toupper(recieving$Pos)
rushing$Pos <- toupper(rushing$Pos)

stats <- full_join(rushing, recieving, by = c("Player", "Pos", "Tm")) %>% 
  filter(!is.na(Pos)) %>% 
  filter(Pos %in% c("WR", "RB", "TE")) %>% 
  select(-c(Rk.x, Rk.y)) %>% 
  full_join(adp, by = c("Player" = "Player Name"))

final <- data.frame(full_join(stats, fantasy_performance, 
                   by = c("Player" = "name")) %>% 
  select(-c('nfl-team', pos, Rank, Team)) %>% 
  filter(Pos %in% c("WR", "RB", "TE")) %>% 
  mutate(StD = 0, Att = ifelse(is.na(Att), 0, Att), FPpTch = pts/(Att+Rec)))

for(i in 1:nrow(final)){
  if(!is.na(final$wk12[i])){
    final$StD[i] <- sd(final[i,24:41])
  }
}

final$`Ctch.` <- round(as.double(gsub("%$", "", final$`Ctch.`)), 1)

kickers <- read_csv("kickers.csv") %>% 
  inner_join(fantasy_performance, by = c("Tm" = "nfl-team")) %>% 
  filter(pos == "K") %>% 
  select(-c(Lng, name, pos)) %>% 
  mutate(Std = 0) %>% 
  rename(FGPer = 'FG%')

for(i in 1:nrow(kickers)){
  if(!is.na(kickers$wk12[i])){
    kickers$Std[i] <- sd(kickers[i,19:36])
  }
}
kickers <- kickers[1:23,]
kickers$FGPer <- round(as.double((gsub("%$", "", kickers$FGPer))), 1)
kickers$`XP%` <- round(as.double((gsub("%$", "", kickers$`XP%`))), 1)

nflteams <- read_csv("nfl_teams.csv")

defense <- read_csv("defense.csv")
for(i in 1:nrow(defense)){
  defense$Tm[i] <- as.character((nflteams %>% filter(Name == defense$Tm[i]))[1,3])
}
defense <- defense %>% 
  inner_join(fantasy_performance, by = c("Tm" = "nfl-team")) %>% 
  filter(pos == "D/ST") %>% 
  select(-c(name, pos, Rk, "...18")) %>% 
  mutate(Std = 0) %>% 
  rename(PTD = 'TD...10', RTD = "TD...14", TOPer = 'TO%')

for(i in 1:nrow(defense)){
  if(!is.na(defense$wk12[i])){
    defense$Std[i] <- sd(defense[i,19:36])
  }
}
#end of creation of dataframes
#final - skill players df
#defense, kickers - dfs
#nflteams- df with nfl teams abbreviations
#selected - selected teams or players

#start of shinyApp

ui <- fluidPage(
  tags$style(
    HTML("
      body {
        background-color: rgba(180, 180, 255, 0.2);
      }
      h1 {
        color: rgba(55, 55, 55, 0.9);
      }
      /* Add more CSS rules as needed */
    ")
  ),
  titlePanel("Fantasy Football Draft 2023"),
  fluidRow(
    column(
      width = 6,
      sidebarPanel(
        selectInput("input_variable", label = "Select an option:", choices = c("Skill Position", "D/ST or Kicker")),
        uiOutput("dynamic_choices")
      )
    ),
    column(
      width = 12,
      mainPanel(
        plotOutput("plot"),
        textOutput("output_text"),
        tableOutput("table")
      )
    )
  )
)

server <- function(input, output) {
  # Define a reactive expression to determine the dynamic choices
  dynamic_choices <- reactive({
    if (input$input_variable == "Skill Position") {
      final$Player
    } else if (input$input_variable == "D/ST or Kicker") {
      nflteams$Abbreviation
    }
  })
  
  # Render the dynamic choices in the UI
  output$dynamic_choices <- renderUI({
    selectInput("input_choice", "Select Choice:", choices = dynamic_choices(), multiple = TRUE)
  })
  
  selected_plot <- reactive({
    if (input$input_variable == "Skill Position") {
      selected_data <- subset(final, Player %in% as.character(input$input_choice))
      plot1 <- ggplot(data = final %>% filter(!is.na(ADP) & !is.na(pts))) +
        geom_point(mapping = aes(x = ADP, y = pts)) +
        geom_point(data = selected_data,
                   mapping = aes(x = ADP, y = pts, color = Player), size = 10) +
        geom_smooth(mapping = aes(x = ADP, y = pts), method = "lm") +
        ggtitle("Average Draft Position versus Fantasy Points")
      plot2 <- ggplot(data = final %>% filter(!is.na(drop.) & !is.na(YACpR) & drop.>0 &YACpR<20)) +
        geom_point(mapping = aes(x = drop., y = YACpR)) +
        geom_point(data = selected_data,
                   mapping = aes(x = drop., y = YACpR, color = Player), size = 10) +
        geom_smooth(mapping = aes(x = drop., y = YACpR), method = "lm") +
        ggtitle("Drop percentage versus YAC per reception")
      plot3 <- ggplot(data = final %>% filter(!is.na(RuY.A) & !is.na(RuTD) & RuY.A<20)) +
        geom_point(mapping = aes(x = RuY.A, y = jitter(RuTD))) +
        geom_point(data = selected_data,
                   mapping = aes(x = RuY.A, y = jitter(RuTD), color = Player), size = 10) +
        geom_smooth(mapping = aes(x = RuY.A, y = RuTD), method = "lm") +
        ggtitle("Rushing Yards per attempt versus Rushing TDs") +
        abline(y=4)
      grid.arrange(plot1, plot2, plot3, ncol = 3)
    } else if (input$input_variable == "D/ST or Kicker") {
      selected_data_kickers <- kickers %>% filter(Tm %in% as.character(input$input_choice))
      plot1 <- ggplot(data = kickers) +
        geom_point(mapping = aes(x = FGPer, y = pts)) +
        geom_point(data = selected_data_kickers,
                   mapping = aes(x = FGPer, y = pts, color = Tm), size = 10) +
        geom_smooth(mapping = aes(x = FGPer, y = pts), method = "lm") +
        ggtitle("Field Goal Percentage versus Fantasy Points")
      selected_data_defense <- defense %>% filter(Tm %in% as.character(input$input_choice))
      plot2 <-  ggplot(data = defense) +
        geom_point(mapping = aes(x = TOPer, y = pts)) +
        geom_point(data = selected_data_defense, 
                   mapping = aes(x = TOPer, y = pts, color = Tm, size = 10)) +
        geom_smooth(mapping = aes(x = TOPer, y = pts), method = "lm") + 
        ggtitle("Turnover Percentage versus Fantasy Points")
      grid.arrange(plot1, plot2, ncol = 2)
    }
  })
  
  selected_table <- reactive({
    if (input$input_variable == "Skill Position") {
      filtered_data <- final %>% filter(Player %in% input$input_choice) %>% 
        select(-c(24:41))
      return(filtered_data)
    } else if (input$input_variable == "D/ST or Kicker") {
      filtered_data <- kickers %>% filter(Tm %in% input$input_choice) %>% 
        select(-c(18:35))
      filtered_data2 <- defense %>% filter(Tm %in% input$input_choice) %>% 
        select(-c(19:36))
      return(c(filtered_data, filtered_data2))
    }
  })

  output$table <- renderUI({
    if (!is.null(input$input_choice)) {
      if (input$input_variable == "Skill Position") {
        table_html <- renderDataTable({
          selected_table()
        })
        table_html
      } else if (input$input_variable == "D/ST or Kicker") {
        filtered_data_kickers <- kickers %>% filter(Tm %in% input$input_choice) %>% 
          select(-c(18:35))
        filtered_data_defense <- defense %>% filter(Tm %in% input$input_choice) %>% 
          select(-c(19:36))
        
        table_kickers <- renderDataTable({
          filtered_data_kickers
        })
        
        table_defense <- renderDataTable({
          filtered_data_defense
        })
        
        tabsetPanel(
          tabPanel("Kickers", table_kickers),
          tabPanel("Defense/Special Teams", table_defense)
        )
      }
    }
  })
  
  
  
  output$plot <- renderPlot({
    selected_plot()
  })
  
  # Use the selected option and choice in the output
  output$output_text <- renderText({
    paste("Selected Type:", input$input_variable)
  })
}

shinyApp(ui, server)
