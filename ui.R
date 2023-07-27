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