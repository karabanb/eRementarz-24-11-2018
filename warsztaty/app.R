library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Wycena mieszkań"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "rok_budowy",
                  label = "Podaj zakres lat budowy",
                  min = 1750,
                  max = 2018, 
                  value = c(1988, 2000), 
                  sep = "")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput(outputId = "pred_table")
    )
  )
)

server <- function(input, output) {
  
  output[["pred_table"]] <- renderTable({
    data.frame(rok = input[["rok_budowy"]][1]:input[["rok_budowy"]][2])
  })
}

shinyApp(ui = ui, server = server)

