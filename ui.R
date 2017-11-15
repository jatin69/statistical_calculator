#' @author : Jatin Rohilla
#' @tool   : R studio v1.0
#' 
#' @ Statistical Calculator
#' @title : ui.R

library(shiny)

# Define UI 
ui <- fluidPage(
  
  # Title 
  titlePanel("Statistical Calculator"),
  hr(),
  
  # 3 colums of equal width
  
  fluidRow(
    column(4,
           # Choosing module
          uiOutput("module"),
          hr(),
          # choosing submoule
          uiOutput("sub_module")
    ),
    column(4,
           # choosing all the inputs required for that submodule
           uiOutput("sub_module_inputs")
    ),
    column(4,
           # processed result
           uiOutput("sub_module_result")
    )
  )
)