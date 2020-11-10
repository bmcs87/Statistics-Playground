# EDIT LOCATION FILE SAVED HERE (example: "C:/Users/resmi/Documents")
setwd("C:/Users/resmi/Documents")

# Call your dataset
dataset <- read.delim("dataset.txt", quote = "")
attach(dataset)

#UPDATE THE X & Y AXIS NAMES
x <- dataset$Date
y <- dataset$Clicks

colnames(dataset) <- c("x", "y")

#Install and call packages
pkgs <- c("tidyverse", "data.table", "outliers", "janitor", "rlang")
install.packages(pkgs, repos = "http://cran.us.r-project.org")
library(tidyverse)
library(data.table)
library(outliers)
library(janitor)
library(rlang)

#Clean names which gives names are unique and consist only of the _ character, numbers, and letters.
data<-janitor::clean_names(dataset)

#Outlier function 
outlier_function<-function(data_frame, column, outlier_label_column) {
              column<-enquo(column)
              outlier_label_column <- enquo(outlier_label_column)
              data_frame %>% 
              mutate(!!outlier_label_column:= scores(!!column,type="z", prob=0.95))
}

######X-Axis
#Outliers on X-Axis; Outlier label column creates a new column with the outliers labeled
dataset <-outlier_function(data_frame = dataset, column = x, 
                 outlier_label_column = x.outlier)

#Output of X-Outlier
x.outlier <- (filter(dataset, x.outlier==TRUE))

#Name X-Outlier
x.id.outlier <- which(dataset$x.outlier == TRUE, arr.ind=TRUE)
print(x.id.outlier)

#Remove X-Outlier
dataset <- dataset[-c(x.id.outlier),]
print(dataset)


######Y-Axis
#Outliers on Y-Axis; Outlier label column creates a new column with the outliers labeled
dataset <-outlier_function(data_frame = dataset, column = y, 
                 outlier_label_column = y.outlier)

#Output of Y-Outlier
y.outlier <- (filter(dataset, y.outlier==TRUE))

#Name Y-Outlier
y.id.outlier <- which(dataset$y.outlier == TRUE, arr.ind=TRUE)
print(y.id.outlier)

#Remove Y-Outlier
dataset <- dataset[-c(y.id.outlier),]
print(dataset)


#New Plot with Outliers Removed
plot(dataset$y ~ dataset$x)
