library(dplyr)
library(tidyverse)
library(nycflights13)

# foo(bar(baz(x))) is the same as 
# baz(x) %>% bar() %>% foo() 
# is the same as 
# x %>% baz() %>% bar() %>% foo() 

# summarise(group_by(flights, carrier), mean(arr_delay, na.rm=TRUE)) 
# becomes 
# group_by(flights, carrier) %>% summarise(mean(arr_delay, na.rm=TRUE)) 
# or 
# flights %>%group_by(carrier) %>% summarise(mean(arr_delay, na.rm=TRUE)) 

# Start from slide 23 
#Filter (normal version)
filter(flights,month==10)

#Piping version (same as above)
flights%>%
  filter(month==10)

# A few logical operators in R
1==2
1!=2
1>2
1<=2
x <- 2
between(x,3,4)

#Question: how to get the data where the month is either 10,11 or 12
# do it in normal version and piping version
# &, |, and ! are vectorized AND, OR, and NOT 
# Non-vectorized versions (for if statements) are && and ||
flights%>%filter(between(month,10,12))
flights%>%filter(month>=10 & month<=12)




#Arrange
arrange(flights,dep_delay)
#Piping
flights%>%
  arrange(dep_delay)
#piping with reverse order
flights%>%
  arrange(desc(dep_delay))


#Select
#removing tailnum and flight from the dataset
select(flights, -tailnum, -flight) 
select(flights, year:arr_delay) 

#Piping version of that?
flights%>%
  select(-tailnum,-flight)



#Rename is a variant of select
#it is simply rename the column
df <- data.frame(x=c(1L,2L,5L,9L),
                 y=c('a','b','c','d'),
                 'z !'=c(1.11,2.22,3.33,4.0),
                 row.names = c("Jo","Ha","Q","Final"),
                 check.names = FALSE,
                 stringsAsFactors = FALSE)
df%>%rename(XXX=x)


#Mutate
flights2 <- mutate(flights, 
                    speed = distance / air_time * 60,
                    distance_1=distance+1)

flights2%>%select(speed)
#Question: if you wanna create two new variables
# Let's say speed_1 = distance/air_time *60 +1
# speed_2= speed_1 +1
# Then store the new data as df2,
# do it in piping version
flights2 <- flights%>%
            mutate(speed_1=distance/air_time *60 +1,
                   speed_2=speed_1+1)%>%
            select(speed_1,speed_2)%>%
  summarise(X=mean(speed_1),
            Y=mean(speed_2))








df2 <- flights%>%
  mutate(speed_1=distance/air_time *60 +1,
         speed_2=speed_1+1)
df2%>%
  select(distance,air_time,speed_1,speed_2)



#Summarize
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
df%>%summarise(count=n(),
              count_distinct=n_distinct(x))

# Calculate the number of unique airline carriers.
summarise(flights, n_distinct(carrier))

# Remember that sum(x == 10) gives the count of x == 10
# What does mean(x == 10) calculate? 
  x <- c(1,2,10,3,10,4,5,10,12,10)
  # What will sum(x==10) return?
  
  # What will mean(x==10) return?

  
  
# Calculate the total number of flights delayed more than 120min on arrival.
summarise(flights, sum(arr_delay > 120)) #It won't work

summarise(flights, sum(arr_delay > 120,na.rm=TRUE)) 


# Calculate the proportion of flights delayed more than 120min on arrival.
summarise(flights, mean(arr_delay > 120,na.rm=TRUE)) 

# Calculate the total number of observation



summarise(flights, count=n())


# Calculate the proportion of flights delayed more than 2 hours on arrival.







summarise(flights, sum(arr_delay > 120,na.rm=TRUE)/n())



#Another Fast way to do it is use the mean!

summarise(flights, mean(arr_delay > 120,na.rm=TRUE))



summarise(flights, sum(arr_delay > 120,na.rm=TRUE)/sum(!is.na(arr_delay)))



#Calculate the total number of missing value in column "air_time"






summarise(flights,sum(is.na(air_time)))

#Calculate the total number in column "air_time"







summarise(flights,count=n())


#Calculate the proportion of missing value in this column










summarise(flights,sum(is.na(air_time))/n())



summarise(flights, mean(is.na(air_time)))

