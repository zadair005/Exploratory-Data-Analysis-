##Week 4 Assignment

#Load the Data & add the data packages
data(iris)

install.packages("ggplot2")
library(ggplot2)

#Initial Exploration
dim(iris)
colnames(iris)
str(iris)
summary(iris)
head(iris)

#Explore the ggplot2 package
boxplot(iris$Sepal.Length~iris$Species)
#Box plots are great for showing the data points which are deemed as outliers
#Also is a good for finding the median, the differences in the classification, and the quantile range.

boxplot(iris$Sepal.Length~iris$Species, ylab="Sepal Length",
        xlab="Species", main="Sepal Length vs Species", cex.lab=1.5)

boxplot(iris$Sepal.Length~iris$Species, ylab = "Sepal Length",
        col=rep(c(1,2,3)), xlab="Species", main="Sepal Length vs Species",
        cex.lab=1.5, cex.axis=1.5, cex.main=2)

#Now load the faithful dataset
data(faithful)
head(faithful)
dim(faithful)
colnames(faithful)
str(faithful)
summary(faithful)

#Plot faithful data
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")

#Load the Airquality dataset
data("airquality")
?airquality

#Load Cars Data
data(cars)

#Explore the cars dataset
head(cars)
dim(cars)
cars$speed

#Simple plot
plot(cars$speed, cars$dist)

#Simple plot with Colored data points
plot(cars$speed, cars$dist, pch=19, col=4, main="Stopping Distance vs. Speed",
     xlab= "Speed", ylab="Stopping Distance")

#Simple plot with different colors 
plot(cars$speed,cars$dist, pch=19, col=rep(1:5,each=10), main="Stopping Distance vs. Speed",
     xlab= "Speed", ylab="Stopping Distance")

##Tables
#Create the table with the speed column from the cars dataset
speed.tab <- table(cars$speed)
plot(speed.tab)
barplot(speed.tab)
hist(cars$speed)

#More elaborate histogram
hist(cars$speed, main="Histogram of Car Speeds", xlab="Speed", ylab="Frequency", breaks = 10)

#Now with some color
hist(cars$speed, main= "histogram of car speeds", xlab = "speed", ylab="Frequency", breaks = 10, col = "purple")

#As we can see, the y axis stops at 8 meters, so make a change to make it from 0 to 12 meters:
hist(cars$speed, main = "Histogram of Car Speeds", xlab="Speed", ylab= "Frequency", breaks = 10, col= "blue", ylim= c(0,12))

##Regression Plots
plot(cars$speed, cars$dist, type="l")

#Now with the data plot more distinct
plot(cars$speed, cars$dist, type = "o")

#Now with a dashed line
plot(cars$speed,cars$dist, type="l",lty="dashed")

plot(cars$speed,cars$dist, type="l", col=2)

#Change the line thickness
plot(cars$speed,cars$dist,type="l",col=2,lwd=3)

#Add some labels
plot(cars$speed,cars$dist, type="l",col=2,lwd=3, xlab="Car Speed", ylab= "Stopping Distance",
     main="Data on Stopping Distance of Cars")

#Fitting the data, if you want to clear the previous plot and start fresh:
plot.new()
#Now I'll redraw the plot above and fit a regression line upon it.
plot(cars$speed, cars$dist, type="l", col=2, lwd=3, 
     xlab="Car Speed", ylab="Stopping Distance", main = "Data on Stopping Distance of Cars")
#regression line
fit1=lm(dist~speed,data=cars)

summary(fit1)

#Now add the line
abline(fit1,lty="dashed")

#Add some text
text(x=10, y=100, labels="average distance = \n3.9324 meters per mile of speed")
#The x = 10, y = 100 command locates the origin of the text. The \n command tells the text to create a new line.

##Scatterplots
#Go back to using the Iris dataset again
data(iris)
head(iris)
pairs(iris)

#Color the pairs by their species
pairs(iris, col=iris$Species)

plot(iris$Sepal.Length, iris$Petal.Length, col=iris$Species)
#We can see a real disparity in the species and their petal and sepal lengths.

#Change the points to solid squares
plot(iris$Sepal.Length, iris$Petal.Length, col=iris$Species, pch=15)

plot(iris$Sepal.Length, iris$Petal.Length, col=iris$Species,pch=16)
#Change the data points to solid circles

#Now let's make the circles bigger
plot(iris$Sepal.Length, iris$Petal.Length, col=iris$Species, pch=16, cex=2)

#Levels of the species
levels(iris$Species)

#R gives setosa the first level, versicolor the 2nd and virginica the 3rd.
legend(x=4.5, y=7,legend=levels(iris$Species),col=c(1:3), pch=16)

legend(x=4.5, y=7,legend=c("setosa","versicolor","virginica"),col=c("magenta","cyan","yellow"),pch=16)

#Let's make the legend too big!
legend(x=4.5,y=7,legend=c("setosa","versicolor","virginica"),col=c("magenta","cyan","yellow"),pch=16,cex=3)
