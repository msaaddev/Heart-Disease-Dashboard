library(shiny)
library(shinydashboard)

ui <- fluidPage(
  titlePanel("Heart Disease Data Visualization"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("var",
                  label = "Choose a technique",
                  choices = c("Decision Tree ID3", "Decision Tree C4.5", "Random Forest"), 
                  selected = "Select"
      )
    ),
    
    mainPanel(
      renderTable(data)
    )
    
  )
)




shinyApp(ui, server)
