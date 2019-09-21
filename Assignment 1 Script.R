## From the Expert Week 1 MSDS 662

#Load the iris data set
data(iris)

iris

head(iris)

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width", "Species")

head(iris)

#Description of the Database
str(iris)
dim(iris)

names(iris)

#Gain insights on the data set
summary(iris)

## Another Example
From_the_Expert_Data_Example <- read_excel("Data Science School Documents/MSDS 662 Exploratory Data Analysis/Week 1/From the Expert Data Example.xlsx", 
                                           +     sheet = "women data")
View(From_the_Expert_Data_Example)

women <- From_the_Expert_Data_Example

women[12,2] #row 12, column 2; note: sqauare brackets
women[8,]
women[1:5,]
women[,2]
women[c(1,3,7,13),]
women[c(1,3,7,13),1]

#Remember, these index numbers don't necessarily correspond to the #'s you
# see printed out w/ the data frame. This can be confusing at 1st, & it is
# something you need to keep in mind.