##Week 2 From the Expert Exercise

#Read in the file
library(dplyr)

ozone <- read_csv("Data Science School Documents/MSDS 662 Exploratory Data Analysis/Week 2/hourly_44201_2014.csv")

#Examine the rows and columns
str(ozone)

#Show only Latitude and Longitude
head(select(ozone, Latitude:Longitude), 15)

#Create subset of the data with Latitude, Longitude and State Name
subset <- select(ozone, Latitude, Longitude, State.Name) 
head(subset,20)

#From the Expert Commands
ranking = group_by(ozone, State.Name,
                   County.Name) %>% summarize(ozone=mean(Sample.Measurement))%>%as.data.frame%>%
  arrange(desc(ozone))

ozone = read_csv("Data Science School Documents/MSDS 662 Exploratory Data Analysis/Week 2/hourly_44201_2014.csv")
head(ozone)

#Select only 3 of the variables of ozone
ozone1 <- select(ozone, State.Name, County.Name, Sample.Measurement)
head(ozone1)

#The frame ozone1 contains all the measurements from every county of every state, and
# remember that in every county there are 100's of measurements cause they were taken over
# a period of time.

#Group the data of ozone1 by State, County:
ozone2 <- group_by(ozone1, State.Name, County.Name)

ozone3 <- summarise(ozone2, count= n(), meanozon=mean(Sample.Measurement))
head(ozone3)

#The instruction used to break the grouping of this data was the data.frame one. It remakes the data into a single
# dataframe w/out groupings. Thus, all counties and their mean pollution information is pooled together irrespective
# of the State they're in.

ozone4 <- as_data_frame(ozone3)

#Now, I'll arrange my ozone4 to descending ozone pollution (meanozone).
ozone5 <- arrange(ozone4,desc(meanozon))

#Check results
head(ozone5)
#The results of the top 5 counties show the state of Colorado and the state of California dominating the top 6 with all but 
# one county in there. Wyoming's Albany county is 5th on the list on the list of highest ozone levels.


###From the Textbook - EDA Checklist
## 1) Formulate your Question
# Formulating a question can be a useful way to guide the EDA process and to limit the exponential number
# of paths that can be taken w/ any sizeable datset.
# Our Question: Which counties in the USA have the highest level of ambient ozone pollution?

## 2) Read in your data
#Now that the question is formed its time to load that data into R
library(readr)
ozone <- read_csv("Data Science School Documents/MSDS 662 Exploratory Data Analysis/Week 2/hourly_44201_2014.csv",
                  col_types = "ccccinnccccccncnncccccc")
#The character string provided to the col_types argument specifies the class of each column in the dataset.
#Each letter represents the class of a column: "c" for character, "n" for numeric, and "i" for integer

#Just as a convenience for later, we can rewrite the names of the columns to remove any spaces.
names(ozone) <- make.names(names(ozone))

## 3) Check the Packaging
#Assuming you don't get any warnings or errors when reading in the dataset, you should now have an object
#in your workspace named ozone. Ex., you can check the number of rows & columns
nrow(ozone)
ncol(ozone)
#There are a total of 9,096,553 records with 23 variables

## 4) Run str()
#This is a safe operation in the sense that even w/ a large dataset.
str(ozone)
#The output for str() duplicates some info that we know about the # of rows & columns 
#You can also examine the classes of each column to make sure they are correctly specified.

## 5) Look at the top and bottom or your data
#It is useful to look at the beginning & end of a dataset. This lets us know if the data were 
# properly read, formatted and that everything is there. 
# This can be done with the head() and tail() function.
head(ozone[, c(6:7, 10)])
tail(ozone[, c(6:7, 10)])
#tail() command is useful because there will be some problems reading the end of a dataset,
# if you don't check you won't know. Make sure to check all columns & verify that all of the data in each column 
# looks the way it's supposed to.

## 6) Check your "n"s
#In general, counting things is usually a good way to figure out if anything is wrong.
#Ex. use the dataset purportedly contains hourly data for the entire country, these will be our 2 landmarks
# Using the column, Time Local, we can monitor to see what time measurements are recorded as being taken.
table(ozone$Time.Local)
#We notice while almost all measurments in the dataset are recorded as being taken by hour, som are taken at different times
#Such a small number of readings are taken at these off times that we might not want to care.
library(dplyr)
#In the example, the author was seeing Time.Local records of 13:14, but I'm not seeing that in my ozone data,
# so I will make note of it but not run it. 
filter(ozone, Time.Local == "13:14") %>% 
           select(State.Name, County.Name, Date.Local, 
                                   Time.Local, Sample.Measurement)

#This next one will filter out that data found in the command filter above.
filter(ozone, State.Code == "36" 
               & County.Code == "033" 
               & Date.Local == "2014-09-30") %>%
           select(Date.Local, Time.Local, 
                   +                Sample.Measurement) %>% 
           as.data.frame
#EPA monitors pollution across the country, there should be a good representation of states. 
select(ozone, State.Name) %>% unique %>% nrow 

#There are 53 states, in a country with only 50 states
#We can take a look at the unique elements of the State.Name variable to see what's going on.
unique(ozone$State.Name)
#In my data, the 3 extra states listed appear to be Washington, D.C.,  Puerto Rico, and the Country of Mexico
#Next we will validate with an external data source.

## 7) Validate with at least one external data source
#Making sure your data matches something outside the dataset is very important. Allows to ensure that the measurements
# are roughly in line with what they should be & it serves as a check on what other things might be wrong in your dataset.
#External validation can often be as simple as checking your data vs a single number like what we do here.
#Let's take a look at the hourly measurements of ozone:
summary(ozone$Sample.Measurement)
#From the summary we can see that the max hourly concentration is quite high but in general, the bulk of 
# distribution is far below 0.075.
quantile(ozone$Sample.Measurement, seq(0,1,0.1))
#Knowing the national standard for ozone is 0.075, we can see from the data: 
#The data are at least of the right order of magnitude 
#The range of the distribution is roughly what we'd expect, given the regulation around ambient pollution levels
#Some hourly levels (less than 10%) are above 0.075 but this may be reasonable given the wording of the standard & the averaging involved.

##8) Try the easy solution first
#We want to know which counties have the highest levels, it seems we need a list of counties that are ordered
# from highest to lowest w/ respect to their levels of ozone. 
#To identify each county we will use a combo of the State.Name and the County.Name variables.
ranking <- group_by(ozone, State.Name, County.Name) %>%
  summarize(ozone = mean(Sample.Measurement)) %>%
  as.data.frame %>%
  arrange(desc(ozone))
#We can look at the rank of top 10 counties
head(ranking, 10)

#Seems interesting that all of these counties are in western U.S., with 4 in California.
#Now the 10 lowest are:
tail(ranking, 10)
#Let's take a look at one of the highest level counties, Mariposa County, California and see how many observations there are for this county in the dataset

filter(ozone, State.Name == "California" & County.Name == "Mariposa") %>% nrow
#We can take a look at how ozone varies through the year in this county by looking at monthly averages.
#First we'll need to convert the date variable into a Date class:
ozone <- mutate(ozone, Date.Local = as.Date(Date.Local))
#Then we split the data by month to look at the average hourly levels.

filter(ozone, State.Name == "California" & County.Name == "Mariposa") %>%
  mutate(month = factor(months(Date.Local), levels = month.name)) %>%
  group_by(month) %>%
  summarize(ozone = mean(Sample.Measurement))
#Ozone appears to be higher in the summer months and lower in the winter months. 
# Let's look at one of the lowest level counties, Caddo County, Oklahoma
filter(ozone, State.Name == "Oklahoma" & County.Name == "Caddo") %>% nrow
#Here we see that there are perhaps fewer observations than we would expecgt for a monitor that was measuring 24 hr/day/all year.

filter(ozone, State.Name == "Oklahoma" & County.Name == "Caddo") %>%
  mutate(month = factor(months(Date.Local), levels = month.name)) %>%
  group_by(month) %>%
  summarize(ozone = mean(Sample.Measurement))

#Some of the monthly averages are below the typical method detection limit of the measurement technology, meaning that those values are highly 
# uncertain and likely not distinguishable from zero.

##9) Challenge your solution
#After finding the easy solution, it is only right to challenge the results, especially if those results comport w/ your prior expectation.
#To challenge the solution, let's start with setting a random number generator and resample the indices of the rows of the data frame with replacement.
set.seed(10234)
N <- nrow(ozone)
idx <- sample(N, N, replace = TRUE)
ozone2 <- ozone[idx, ]
#Now we can reconstruct our rankings of the counties based on this resampled data.
ranking2 <- group_by(ozone2, State.Name, County.Name) %>%
  summarize(ozone = mean(Sample.Measurement)) %>%
  as.data.frame %>%
  arrange(desc(ozone))
#We can compare the top 10 counties from our original ranking & the top 10 counties from ranking based on the resampled data.
cbind(head(ranking, 10),
      head(ranking2, 10))

#We can see that the rankings based on the resampled data are very close to the original, w/ the first 7 being identical.
# Numbers 8 & 9 get flipped in the resampled rankings but that's about it. This might suggest that the original rankings are somewhat stable.
#We can also look at the bottom of the list if there were major changes.
cbind(tail(ranking, 10),
      tail(ranking2, 10))
#Here we can see that the bottom 7 counties are identical in both rankings, but after that things shuffle a bit. 
#We're less concerned w/ the counties at the bottom of the list, but this suggests there is also stability.

##10) Follow up questions
#Several follow-up questions:
#Do we have the right data?
#Do you need other data?
#Do you have the right question?
#The goal of EDA is to get you thinking about your data and reasoning about your question. At this point, we can refine our question
# or collect new data, all in an iterative process to get at the truth.