

median_input<-function(){
  tagList(
  textInput('median_input_data1', 'Enter a numeric data', "0,1,2"),
  selectInput(inputId = "utility",
              label = "Choose a Utility:",
              choices = c("lol","okokokokok"))
  )
}