#' @author : Jatin Rohilla
#' @tool   : R studio v1.0
#' 
#' @ Statistical Calculator
#' @title : server.R


library(shiny)

source("module_includes.R")



# Server Logic

server <- function(input, output) {
  
  # Making input outputs global
  # sacrificing security for Modularity
  input<<-input
  output<<-output
  
  
  ### LEFT MOST PANEL ###
  
  ## renders module list ###
  output$module<-renderUI({
      
    # ask for module input
    selectInput(
        inputId = "module_menu", label = "Choose a module", 
        choices = module_list, selected = "-- Choose a module --"
      )
    
    })
  
  ## Renders sub module list ###
  output$sub_module<-renderUI({
    
    # The warning pops up for EXPR should be 1 because, module_one_list is a list of len >1
    
    sub_module_choices= switch(
    
        input$module_menu, # changes based on module choice
      
        "-- Choose a module --"= module_zero_list,
        "Descriptive Analysis"= module_one_list,
        "Predictive Analysis"= module_two_list
        # similarly add all 9 modules and corresponding sub_modules
    )
    
    # ask for sub_module input
    selectInput(
      inputId = "sub_module_menu", label = "Choose a Sub module", 
      choices = sub_module_choices
    )
  })
  
  
  ### MIDDLE PANEL ### - Most difficult
  ## Renders required input - arbitrary length - unstable ###
  ## also figure out - how to pass it as argument to my function and how to receive output ##
  
  output$sub_module_inputs<-renderUI({
    
    sub_module_inputs= switch(
      
      input$sub_module_menu,
      "-- First select a module --"= my_info_input,
      "Mean"= my_mean_input,
      "Median"= my_median_input,
      "Mode"= my_mode_input
      # similarly add all all utilities from all 9 modules
    )
    
    # Takes all the required inouts
    sub_module_inputs()
    
  })
  
  #### RIGHT MOST PART ####### - Equally difficult
  
  output$sub_module_result<-renderUI({
    
    sub_module_outputs= switch(
      input$sub_module_menu,
      "-- First select a module --"= my_info_output,
      "Mean"= my_mean_output,
      "Median"= my_median_output,
      "Mode"= my_mode_output
      # similarly add all all utilities from all 9 modules
    )
    
    sub_module_outputs()
  })
  
}
# server logic ends
