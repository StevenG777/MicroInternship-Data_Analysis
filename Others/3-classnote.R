library(dplyr)
library(tidyverse)
library(nycflights13)
df <- data.frame(x=c(1L,2L,5L,9L),
                 y=c('a','b','c','d'),
                 'z !'=c(1.11,2.22,3.33,4.0),
                 row.names = c("Jo","Ha","Q","Final"),
                 check.names = FALSE,
                 stringsAsFactors = FALSE)
#How to retrieve a column
#By name or index
df$x
df[["y"]]
df$'z !'
df[,2:3]
df[,c("y","z !")]

#How to retrieve a row
df[1:3,]
df[c("Jo","Ha"),]

#Know the class of the things you retrieve
class(df$x)
class(df[1])


df[,'z !',drop=FALSE]
data.frame(df[,'z !'])


1!=2
#How to retrieve a row & column at the same time
df[1:2,c("y","x")]


#After this, move to ppt and scroll to the dplyr

# Dplyr verb
df1 <- mutate(flights,x=distance+1)
df2 <- filter(flights,month==10)
df3 <- select(flights,arr_time)
df4 <- summarise(flights,x=median(distance))

#Comparison Exercise
#Comparison will return only two value, True or False
df$x<=2
#When it comes to select stuff, you could use the True and False to filter the right value you want
df[df$x<=2,]

#Standard comparison
1==2
1!=2
1>2

a=1
a %in% c(1,3,4)
is.na(c(1,2,NA))
is.na(c(1,2,"NA"))

#Question5:Get only flights from Alaska Airlines or Hawaiian Airlines
# In base R:

# In dplyr:
filter(flights,carrier=="AS" | carrier=="HA")
filter(flights,carrier %in% c("AS","HA"))


# In dplyr:


#Question5b:Get only flights from Alaska Airlines or Hawaiian Airlines and the flights arrives earlier


# #Question6: Get only flights between Honolulu 
  #and JFK. You're going to use column "origin", "dest" 
  #and variables called "JFK" & "HNL"
filter(flights, 
       (origin=="JFK" & dest=="HNL") |
       (origin=="HNL" & dest=="JFK"))

# Move to the ppt and talk about arrange

#Summary statistic 
#Why na.rm=True is important?
summarise(flights,
          mean_dep_delay = mean(dep_delay, na.rm=TRUE), 
          mean_arr_delay = mean(arr_delay, na.rm=TRUE))
summarise(flights,
          mean_dep_delay = mean(dep_delay), 
          mean_arr_delay = mean(arr_delay))



#Find the total observation in your data
summarise(df,
          count=n(),
          count_distinct=n_distinct(x))

#Find the total observation in your data
df <- data.frame(x=c(1L,2L,5L,9L),
                 y=c('a','b','c','d'),
                 'z !'=c(1.11,2.22,3.33,4.0),
                 row.names = c("Jo","Ha","Q","Final"),
                 check.names = FALSE,
                 stringsAsFactors = FALSE)
summarise(df,count=n(),count_distinct=n_distinct(x))

# Calculate the proportion of flights delayed more than 2 hours on arrival.
summarise(flights, mean(arr_delay > 120, na.rm=TRUE)) 
# Calculate the number of unique airline carriers.
summarise(flights, n_distinct(carrier))
# Calculate the proportion of flights with missing air times.
summarise(flights, sum(is.na(air_time)) / n())
summarise(flights, mean(is.na(air_time)))




