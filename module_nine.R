# Including the core functionality
source("module_nine_core.R")


## ======================= UTILITY LIST ==========================

module_nine_list<-c( 	"Histograms",
                      "Line Graph",
                      "Bar Graph",
                      "Pie Chart",
                      "Scatter plot",
                      "Box-plot",
                      "q-q plot",
                      "Stem-leaf plot",
                      "Pareto Chart"
)

## ========================= I/O FUNCTIONS ==============================


## ========================= Histogram & Bar plot  ==============================

my_hist_input<-function(){
  tagList(
    textInput('my_hist_input_dataOne', 'Enter data set to be plotted ( comma delimited )', "1,2,3,4,5,1,2,3,3,3,5")
  )
}

my_hist_output<-function(){
  tagList(
    renderPlot({
      mydata<- as.numeric(unlist(strsplit(input$my_hist_input_dataOne,",")))
      my_hist(mydata)
    })
  )
}


my_barplot_input<-function(){
  tagList(
    textInput('my_barplot_input_dataOne', 'Enter data set to be plotted ( comma delimited )', "1,2,3,4,5,1,2,3,3,3,5")
  )
}

my_barplot_output<-function(){
  tagList(
    renderPlot({
      mydata<- as.numeric(unlist(strsplit(input$my_barplot_input_dataOne,",")))
      my_barplot(mydata)
    })
  )
}

## ========================= Line Chart  ==============================

my_linegraph_input<-function(){
  tagList(
    textInput('my_linegraph_input_dataOne', 'Enter data set 1 ', "5,6,1,1,2,3"),
    textInput('my_linegraph_input_dataTwo', 'Enter data set 2 (if any)', "2,3"),
    textInput('my_linegraph_input_dataThree', 'Enter data set 3 (if any)', "0")
  )
}

my_linegraph_output<-function(){
  tagList(
    renderPlot({
      mydata1<- as.numeric(unlist(strsplit(input$my_linegraph_input_dataOne,",")))
      mydata2<- as.numeric(unlist(strsplit(input$my_linegraph_input_dataTwo,",")))
      mydata3<- as.numeric(unlist(strsplit(input$my_linegraph_input_dataThree,",")))
      my_linegraph(mydata1,mydata2,mydata3)
    })
  )
}


## ========================= Pie Chart  ==============================

my_pie_input<-function(){
  tagList(
    textInput('my_pie_input_dataOne', 'Enter data set to be plotted ( comma delimited )', "1,2,3,4,5,1,2,3,3,3,5")
  )
}

my_pie_output<-function(){
  tagList(
    renderPlot({
      mydata<- as.numeric(unlist(strsplit(input$my_pie_input_dataOne,",")))
      my_pie(mydata)
    })
  )
}


## ========================= Scatter, BOX, qq plot  ==============================


my_scatter_input<-function(){
  tagList(
    textInput('my_scatter_input_dataOne', 'Enter data set to be plotted across x axis', "1,2,3,4,5,6,7,8,9,10,11"),
    textInput('my_scatter_input_dataTwo', 'Enter data set to be plotted across y axis', "3,4,7,9,10,14,15,18,19,11,13")
  )
}

my_scatter_output<-function(){
  tagList(
    renderPlot({
      x<- as.numeric(unlist(strsplit(input$my_scatter_input_dataOne,",")))
      y<- as.numeric(unlist(strsplit(input$my_scatter_input_dataTwo,",")))
      my_scatter(x,y)
    })
  )
}


my_boxplot_input<-function(){
  tagList(
    textInput('my_boxplot_input_dataOne', 'Enter data set to be plotted across x axis', "1,2,3,4,5,6,7,8,9,10,11"),
    textInput('my_boxplot_input_dataTwo', 'Enter data set to be plotted across y axis', "3,4,7,9,10,14,15,18,19,11,13")
  )
}

my_boxplot_output<-function(){
  tagList(
    renderPlot({
      x<- as.numeric(unlist(strsplit(input$my_boxplot_input_dataOne,",")))
      y<- as.numeric(unlist(strsplit(input$my_boxplot_input_dataTwo,",")))
      my_boxplot(x,y)
    })
  )
}

my_qplot_input<-function(){
  tagList(
    textInput('my_qplot_input_dataOne', 'Enter data set to be plotted across x axis', "1,2,3,4,5,6,7,8,9,10,11"),
    textInput('my_qplot_input_dataTwo', 'Enter data set to be plotted across y axis', "3,4,7,9,10,14,15,18,19,11,13")
  )
}

my_qplot_output<-function(){
  tagList(
    renderPlot({
      x<- as.numeric(unlist(strsplit(input$my_qplot_input_dataOne,",")))
      y<- as.numeric(unlist(strsplit(input$my_qplot_input_dataTwo,",")))
      my_qplot(x,y)
    })
  )
}


## ========================= Stem Leaf  ==============================

my_stem_input<-function(){
  tagList(
    textInput('my_stem_input_dataOne', 'Enter data set to be plotted ( comma delimited )', "1,2,3,5,5,4,1,1,1")
  )
}

my_stem_output<-function(){
  tagList(
    renderPrint({
      mydata<- as.numeric(unlist(strsplit(input$my_stem_input_dataOne,",")))
      cat(my_stem(mydata))
    })
  )
}


## ========================= Pareto Chart  ==============================

my_pareto_chart_input<-function(){
  tagList(
    textInput('my_pareto_chart_input_dataOne', 'Enter data set to be plotted ( comma delimited )', "5,6,1,1,2,3")
  )
}

my_pareto_chart_output<-function(){
  tagList(
    renderPlot({
      mydata<- as.numeric(unlist(strsplit(input$my_pareto_chart_input_dataOne,",")))
      my_pareto_chart(mydata)
    })
  )
}

