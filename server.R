source("FantasyFootball2023.R")
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
      plot1 <- ggplot(data = final %>% filter(!is.na(pts))) +
        geom_point(mapping = aes(x = adp, y = pts)) +
        geom_point(data = selected_data,
                   mapping = aes(x = adp, y = pts, color = Player), size = 10) +
        geom_smooth(mapping = aes(x = adp, y = pts), method = "lm") +
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
