

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Movie Recommendation Tool", titleWidth = 450),
  dashboardSidebar(),
  dashboardBody(
    actionButton("randUserRec", "Get Movie Recomendation for a Random User"),
    br(),br(),
    fluidRow(
      box(width = 6, title = strong("User's Top Rated Movies"), tableOutput('UserRatedMovies')),
      box(width = 6, title = strong("User's Top Rated Movies"), tableOutput("recomendedMovies"))
    ),
    fluidRow(
      box(
        title = strong("Model Performance Analysis"),
        width = 12,
        textOutput("MSE"),
        br(),strong("Binary Classification Analysis"),
        tableOutput("confusionMatrix"),
        textOutput("Accuracy")
      )
    )
  )
)

server <- function(input, output){
  observeEvent(input$randUserRec, {
    randUser <- floor(runif(1, min = 1, max = 278))
    UsersTopRatedMovies <- GetTopMovies(movie_data, UserRatings_t, randUser)
    recomendedMovies <- GetRecommendations(movie_data, UserRatings, PredictedRatings_t, randUser)
    output$UserRatedMovies <- renderTable({UsersTopRatedMovies}, caption = paste("Users Top Rated Movies. User ID:", randUser) )
    output$recomendedMovies <- renderTable({recomendedMovies}, caption = paste("Recomended Movies. User ID: ", randUser) )
    output$MSE <- renderText(paste("Mean Squared Error:", PredictionMSE))
    matrix <- ConfusionAnalysis[1]
    output$confusionMatrix <- renderTable(matrix, rownames = TRUE, colnames = TRUE)
    acc <- ConfusionAnalysis[2]
    output$Accuracy <- renderText(paste("Accuracy: ", acc))
  })
}

shinyApp(ui, server)