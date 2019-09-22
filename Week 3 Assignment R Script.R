## MSDS 662 - Assignment 3

#Load & explore the Iris data
data(iris)
iris

colnames(iris)

dim(iris)

str(iris)

summary(iris)

#Load the R libraries
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
install.packages("fields")
library(fields)
install.packages("nlme")
library(nlme)
install.packages("mgcv")
library(mgcv)
library(dplyr)

#The table() instruction
table(iris$Species) %>%
barplot(col=iris$Species)

#Table of Iris Species
counts=table(iris$Species)
counts

#Bar chart of Species
barplot(counts, col="red")

#Line Charts
head(iris)
with(iris, {plot(Sepal.Length~Sepal.Width)
  lines(lowess(Sepal.Length~Sepal.Width),col="red")})

with(iris, {plot(Petal.Length~Petal.Width)
  lines(lowess(Petal.Length~Petal.Width),col="red")})

with(iris, {plot(Petal.Length~Sepal.Length)
  lines(lowess(Petal.Length~Sepal.Length), col="red")})

with(iris, {plot(Petal.Width~Sepal.Width)
  lines(lowess(Petal.Width~Sepal.Width), col="red")})

#Histogram
install.packages("ggplot2")
library(ggplot2)

#Petal width
hist(iris$Petal.Width, breaks=9)

#Petal Length
hist(iris$Petal.Length, breaks=9)

#Sepal Width
hist(iris$Sepal.Width, breaks=9)

#Sepal Length
hist(iris$Sepal.Length, breaks=9)


