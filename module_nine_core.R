# ================ CORE FUNCTIONS FOR MODULE NINE =================

library(qcc)


# mydata<-head(iris$Sepal.Length,10)
# mydata1<-tail(iris$Sepal.Length,10)
# mydata2<-c(1:10)

my_hist <- function(mydata) {
  hist( mydata , xlab = "x",col = "green",border = "black",main = "Histogram")
}

my_linegraph <- function( mydata , mydata1 = 0 , mydata2 = 0) {
  plot(mydata,type = "o", col = "red", xlab = "x", ylab = "y",main = "Line Graph")
  lines(mydata1, type = "o", col = "blue")
  lines(mydata2, type = "o", col = "green")
}

my_barplot <- function(mydata) {
  barplot(mydata, col = "red", xlab = "x", ylab = "y",main = "Bar Graph")
}

my_pie <- function(mydata) {
  pie(mydata,  main = "Pie chart")
}


my_scatter <- function(mydata,mydata1) {
  plot(x = mydata,y = mydata1, xlab = "x", ylab = "y", main = "Scatter Diagram")
  abline(lm(mydata ~ mydata1))
}

my_boxplot <- function(mydata,mydata1) {
  boxplot(mydata~mydata1, xlab = "x" , ylab = "y", main = "Box Plot")
}

my_qplot <- function(mydata,mydata1) {
  qqplot(x= mydata,y=mydata1,xlab = "xBeer", ylab = "y")
  qqnorm(mydata1)
  qqline(mydata1, col = "red")
}

my_stem <- function(mydata) {
  return(stem(mydata))
}

my_pareto_chart<-function(data){

  library(qcc)

  # defect <- c(80, 27, 66, 94, 33)
  # names(defect) <- c("price code", "schedule date", "supplier code", "contact num.", "part num.")
  # pareto.chart(defect, ylab = "Error frequency")
  # pareto.chart(defect, ylab = "Error frequency", xlab = "Error causes", las=1)
  # pareto.chart(defect, ylab = "Error frequency", col=rainbow(length(defect)))
  # pareto.chart(defect, cumperc = seq(0, 100, by = 5), ylab2 = "A finer tickmarks grid")
  
  pareto.chart(data, ylab = "Frequency")
}

