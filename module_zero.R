module_list<-c(
  
  "-- Choose a module --", # module 0 option - About
  
  "Descriptive Analysis",  # Module 1
  "Predictive Analysis",   # Module 2
  "Probability Analysis",  # Module 3

  "Discrete Distribution Functions",      # Module 4
  "Continuous Distribution Functions",    # Module 5
  
  "Sample Distribution Test Statistic",   # Module 6
  "Interval Estimation",                 # Module 7
  
  "Non-Parametric Analysis",    # Module 8
  "Visualizations"              # Module 9
)

# =========== About us ====================

module_zero_list<-c("-- First select a module --")

my_info_input<-function(){
  tagList(
    withTags({
      div(class="header", checked=NA,
          h1("SKULL"),
          h2("The Statistical Calculator"),
          br()
          
      )
    }),
    renderImage({
      image_file <- paste("skull.jpeg")
      return(list(
        src = image_file,
        filetype = "image/jpeg"
      ))
    }, deleteFile = FALSE)
   
  )
}

my_info_output<-function(){
  tagList(
    
    withTags({
      div(class="header", checked=NA,
          h2("About Us"),
          h3("We are students at DUCS."),
          p("This is a statistical calculator."),
          br()
          
      )
    }),
    
    renderPrint({
      cat("Copyright 2017\n")
      cat("\nDeveloper     : Jatin Rohilla\nMathematician : Shoaib Rayeen
          ")
      
    })
  )
  
}


## =================== IMPLEMENTATION STATUS ==========================

#=========== Steps to add a new utility ================

# 1. add core function in module_<no>_core.R, that returns correct result
# 2. Test it thoroughly
# 
# 3. go to module_<no>.R and generate appropriate input/ouput.
# Be careful, this is a high priority task
# All required inputs to be name as - my_mean_input_dataOne
# 
# 4. go to server.R and make input/output entry of that utility in server logic.
# 5. Test on app
# 6. Add entry in module_zero.R


# ============ GUI ============

# Module 1
# - mean
# - median
# - Mode
# - Range-Min-Max

# =========== CORE ============

# Module 1
# - mean
# - mode
# - median
# - range
# - min
# - max
