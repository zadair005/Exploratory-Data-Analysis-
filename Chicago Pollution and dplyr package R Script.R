##Chicago Dataset Exploration

#Load the dplyr package and the Chicago pollution data
install.packages("dplyr")
library(dplyr)

chicago <- readRDS("C:/Users/zadai/Downloads/chicago_data (1)/chicago.rds")

#Look at the basic characteristics of the data
dim(chicago)

str(chicago)

#Create a subset of the data
names(chicago)[1:3]
subset <- select(chicago, city:dptp)
head(subset)

#Omit variables using the select() function by using (-)
select(chicago, -(city:dptp))
#This indicates that we should include every variable except the variables
# city through dptp. The equivalent code in base R would be

i <- match("city", names(chicago))
j <- match("dptp", names(chicago))
head(chicago[, -(i:j)])

#The select() function also allows a special syntax that allows you to specify
#variable names based on patterns. An example, if you wanted to keep every variable
# that ends with a "2", we could do:
subset <- select(chicago, ends_with("2"))
str(subset)

#Or to keep every variable that starts w/ a "d", we could do 
subset <- select(chicago, starts_with("d"))
str(subset)
#You can also use more general regular expressions if necessary. 

#Now let's play around with the filter() function
#The filter() function is used to extract subsets of rows from a data frame.
#This function is similar to the existing subset() function in R

#Suppose we wanted to extract rows of the chicago data frame where the levels of PM2.5 are greater than 30:
chic.f <- filter(chicago, pm25tmean2 > 30)
str(chic.f)
#Now we can see that there are only 194 rows in the data frame and the distribution 
#of the pm25tmean2 value is:
summary(chic.f$pm25tmean2)

#We can place an arbitraily complex logical sequence inside filter(),
#so we could for example extract rows where PM2.5 is greater than 30 &
#temperature is > 80 degrees
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
select(chic.f, date, tmpd, pm25tmean2)
#Now there are 17 observations where both of those conditions are met

## Arrange Function
#The arrange() function is used to reorder rows of a data frame according
#to one of the variables/columns. Reordering rows of a data frame (while
# preserving corresponding order of other columns) is normally a pain to
# do in R. The arrange() function simplifies the process quite a bit. 
# Here we can order rows of the data frame by date, so that the first row 
#is the earliest (oldest) observation and the last row is the latest observation

chicago <- arrange(chicago, date)

head(select(chicago, date, pm25tmean2), 3)
tail(select(chicago, date, pm25tmean2), 3)

chicago <- arrange(chicago, desc(date))

head(select(chicago, date, pm25tmean2), 3)
tail(select(chicago, date, pm25tmean2), 3)

## Rename Function
# Renaming a variable in a data frame in R is surprisingly hard to do!
# The rename() function is designed to make this process easeier
# Here you can see the names of the 1st 5 variables in the chicago df
head(chicago[, 1:5], 3)
#The dptp column is supposed to represent the dew point temp. and the
# pm25tmean2 column provides the PM2.5 data, but, these names are pretty 
# obscure or awkward and probably be renamed to something more sensible
chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
head(chicago[, 1:5], 3)

## Mutate Function
#Mutate() function exists to compute transformations of variables in a data frame.
#Ex., with air pollution data, we often want to detrend the data by subtracting the
# the mean from the data. We can look at whether a given day's air pollution level is
# higher than or < average 
# Let's create a pm25detrend variable that subtracts the mean from the pm25 variable
chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
head(chicago)
# There is also the related transmute() function, which does the same thing as mutate()
# but then drops all non-transformed variables.
#Here we detrend the PM10 and ozone (O3) variables.
head(transmute(chicago,
               pm10detrend = pm10tmean2 - mean(pm10tmean2, na.rm = TRUE),
               o3detrend = o3tmean2 - mean(o3tmean2, na.rm = TRUE)))

## Group By Function
# The group_by() function is used to generate summary statistics from the data frame w/in
#strata defined by a variable. For example, in this air pollution dataset, might want to know
# the average annual level of PM2.5 is. 
# The general operation here is a combo of splitting a data frame into separate pieces defined 
# by a variable or group of variables (group_by()), and then applying a summary function
# accross those subsets (summarize())
# Start by creating a year variable using as.POSIXlt().
chicago <- mutate(chicago, year = as.POSIXlt(date)$year +1900)
# Now we can create a separate data frame that splits the original data frame by year.
years <- group_by(chicago, year)
# Finally, compute summary stats for each year in the data frame w/ the summarize function
summarize(years, pm25 = mean(pm25, na.rm = TRUE),
          o3 = max(o3tmean2, na.rm = TRUE),
          no2 = median(no2tmean2, na.rm = TRUE))
qq <- quantile(chicago$pm25, seq(0, 1, 0.2), na.rm = TRUE)
chicago <- mutate(chicago, pm25.quint = cut(pm25, qq))
quint <- group_by(chicago, pm25.quint)
summarize(quint, o3 = mean(o3tmean2, na.rm = TRUE),
          no2 = mean(no2tmean2, na.rm = TRUE))

## %>%
# The pipeline operator %>% is very handy for stringing togehter multiple dplyr functions
# in a sequence of operations. Notice above that every time we wanted to apply more than one function, 
# the sequence gets buried in a sequence of nested function calls that is difficult to read.
mutate(chicago, pm25.quint = cut(pm25, qq)) %>%
  group_by(pm25.quint) %>%
  summarize(o3 = mean(o3tmean2, na.rm = TRUE),
            no2 = mean(no2tmean2, na.rm = TRUE))
# This way we don't have to create a set of temporary variables along the way or crate a massive
# nested sequence of function calls.
# Notice in the above code that I pass the chicago data frame to the first call to mutate(), but 
# then afterwards I don't have to pass the 1st argument to group_by() or summarize().
# Once you travel down the pipeline w/ %>%, the 1st argument is taken to be the output of the
# previous element in the pipeline.
#Another exa., might be computing the average pollutant level by month. This could be useful to see 
# if there are any seasonal trends in the data. 
mutate(chicago, month = as.POSIXlt(date)$mon + 1) %>%
  group_by(month) %>%
  summarize(pm25 = mean(pm25, na.rm = TRUE),
            o3 = max(o3tmean2, na.rm = TRUE),
            no2 = median(no2tmean2, na.rm = TRUE))

