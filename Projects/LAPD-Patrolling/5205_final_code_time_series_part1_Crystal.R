setwd('/Users/crystal19950624/Downloads')
crime = read.csv('data_cleaning_2.csv', stringsAsFactors = F)
str(crime)

library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling


#To deal with the NAs in 'month' column, we took out the number of month from 'DATE.OCC' column and replace the variables in 'month' columns with those numbers.
library(lubridate)
new_month <- month(as.POSIXlt(crime$DATE.OCC, format="%m/%d/%Y"))
crime$month <- new_month
unique(crime$month)

#The plot of count of all crime cases over months in 2019-2021 in LA
library(plyr)
crime$Month_Yr <- format(as.Date(crime$DATE.OCC, format="%m/%d/%Y"), "%Y-%m") #create a new variable
crime_each_month_every_year <- count(crime, "Month_Yr")
print(crime_each_month_every_year)
crime_each_month_every_year_drop <- crime_each_month_every_year %>% filter(Month_Yr != '0022-04')
print(crime_each_month_every_year_drop) # I dropped the row of 2022-04 because it was significantly lower than rest of the data, and I think it is because it is April 2022 now and the collection for data in April is not finished yet. 

library(ggplot2)
ggplot(data=crime_each_month_every_year_drop, aes(x=Month_Yr, y=freq, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Time(Year-Month)") +
  scale_y_continuous(name="Total Count of Crime Cases per Month") +
  ggtitle("Monthly Count of Crime Cases from 2019-01 to 2021-03") +
  theme(axis.text.x = element_text(angle = 45))

#The plot of count of all crime cases over 365 days in 2021 in LA
library(plyr)
crime_21 <- crime %>% filter(Month_Yr == c('0021-01', '0021-02', '0021-03', '0021-04', '0021-05', '0021-06', '0021-07', '0021-08', '0021-09', '0021-10', '0021-11', '0021-12'))
crime_each_day <- count(crime_21, "DATE.OCC")
print(crime_each_day)

library(ggplot2)
ggplot(data=crime_each_day, aes(x=DATE.OCC, y=freq, group=1)) +
  geom_line()+
  xlab("Date") + ylab("Total Count of Crime Cases per Day") +
  ggtitle("Daily Count of Crime Cases in 2021")

#Plot of Top 5 crime types in July
crime_month7 <- crime_21 %>% filter(crime_21$month == 7)
crime_type_7 <- crime_month7 %>% group_by(Crm.Cd.Desc) %>% dplyr::summarise(count =n())
print(crime_type_7)
crime_type_7_order <- crime_type_7 %>% 
  arrange(desc(count))
print(crime_type_7_order)

crime_type_7_top5 <- top_n(crime_type_7_order, n=5, count)
View(crime_type_7_top5)
library(ggplot2)
ggplot(data=crime_type_7_top5, aes(x=Crm.Cd.Desc, y=count)) +
  geom_bar(stat='identity')+
  xlab("Type of Crime") + ylab("Total Count of Crime Case") + ggtitle("Top 5 Kinds of Crime in July 2021") +
  scale_x_discrete('Type of Crime', labels = c('a', 'b', 'c', 'd', 'e'))

#Plot of Top 5 crime types in October
crime_month10 <- crime_21 %>% filter(crime_21$month == 10)
crime_type_10 <- crime_month10 %>% group_by(Crm.Cd.Desc) %>% dplyr::summarise(count = n())
print(crime_type_10)
crime_type_10_order <- crime_type_10 %>% 
  arrange(desc(count))
print(crime_type_10_order)

crime_type_10_top5 <- top_n(crime_type_10_order, n=5, count)
View(crime_type_10_top5)
library(ggplot2)
ggplot(data=crime_type_10_top5, aes(x=Crm.Cd.Desc, y=count)) +
  geom_bar(stat='identity')+
  xlab("Type of Crime") + ylab("Total Count of Crime Case") + ggtitle("Top 5 Kinds of Crime in October") +
  scale_x_discrete('Type of Crime', labels = c('a', 'b', 'c', 'd', 'e'))



