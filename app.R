library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)

raw.data <- read.table("housing_prices_ge19.txt", 
                       header = TRUE, sep = "\t", dec = ".")

# CHOOSE COLUMNS
data <- raw.data[, c("Price", "Lot.Size", "Age",
                     "Land.Value", "Living.Area")]


# UI
ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(
    title = "NY Housing Prices",
    titleWidth = 200
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    box(
      title = "Our Model", 
      status = "info",
      solidHeader = TRUE, 
      width = 50,
      collapsible = TRUE,
      withMathJax(helpText("$$\\textrm{Price}=31814.094+86.264\\times\\textrm{Living.Area}+0.975\\times\\textrm{Land.Value}-299.948\\times\\textrm{Age}+6680.308\\times\\textrm{Lot.Size}$$"))
    ),
    
    box(
      
      title = "Inputs",
      status = "primary",
      solidHeader = TRUE,
      
      numericInput("lot.size",
                   "Lot Size",
                   100
      ),
      
      numericInput("age",
                   "Age",
                   40
      ),
      
      numericInput("land.value",
                   "Land Value",
                   10000
      ),
      
      numericInput("living.area",
                   "Living Area",
                   200
      ),
      
      div(style = "display: inline-block; vetical-align: top; width:100px",
          actionButton("go", "Predict")),
      div(style="display: inline-block;vertical-align:top; width: 100px;",
          actionButton("reset", "Clear", style='padding:6px;width:80px'))
    ),
    
    box(
      
      title = "Prediction",
      status = "primary",
      solidHeader = TRUE,
      
      h2("Predicted value for house price: "),
      h4(verbatimTextOutput("pred", placeholder = T)),
      tags$head(tags$style("#pred{color: black;
                                   font-size: 20px;
                                   font-family: Source Sans Pro
                                   }")),
      h3("Confidence intervals (at a 95% confidence): "),
      h4("Lower CI"),
      h4(verbatimTextOutput("lci", placeholder = T)),
      h4("Upper CI"),
      h4(verbatimTextOutput("uci", placeholder = T)),
      tags$head(tags$style("#uci{color: black;
                                   font-size: 20px;
                                   font-family: Source Sans Pro
                                   }")),
      tags$head(tags$style("#lci{color: black;
                                   font-size: 20px;
                                   font-family: Source Sans Pro
                                   }")),
      
    )
    
  )
  
)

# SERVER
server <- function(input, output, session) {
  
  #reset
  observeEvent(input$reset, {
    updateNumericInput(session, 'lot.size', value = 100)
    updateNumericInput(session, 'age', value = 40)
    updateNumericInput(session, "land.value", value = 10000)
    updateNumericInput(session, "living.area", value = 200)
  })
  
  fit = lm(Price ~ ., data = data)
  
  pred = eventReactive(input$go, {
    
    newdata = tibble(Lot.Size = input$lot.size,
                     Age = input$age,
                     Land.Value = input$land.value,
                     Living.Area= input$living.area) |>
      mutate_all(as.numeric)
    
    vals = predict(fit, newdata = newdata, interval = "confidence")
    pred = vals
    pred = round(pred, 4)
    paste0(pred)
    
  })
  
  output$pred = renderText(pred()[1])
  output$lci = renderText(pred()[2])
  output$uci = renderText(pred()[3])
  
}


shinyApp(ui = ui, server = server)