# First you need to install some libraries to manipulate and analyse dara
install.packages(c("openair","tidyverse", "lares"))
# Remember to edit the next line in order to use your city data
# Do not use my hourly data from Martorell
city<-read.csv('https://raw.githubusercontent.com/drfperez/openair/main/city.csv')
# You need to call tidyverse library in order to use pivot_longer
library (tidyverse)
# pivot_longer allows you to convert hour columns to hour rows
city1<-pivot_longer(city,cols=c(h01,h02,h03,h04,h05,h06,h07,h08,h09,h10,h11,h12,h13,h14, h15,h16,h17,h18,h19,h20,h21,h22,h23,h24), names_to="hour", values_to = "value")
# Delete unnecessary columns
city2<-city1[-c(1,2,4,6:16)]
# You need dplyr library from tidyverse to use pipe symbol %>% and combine two columns in one
library(dplyr)
city2 <- city2 %>% mutate(date=paste0(data, " ", hour))
# check the names of your columns
colnames (city2)
# delete some unnecessary columns
city2<-city2[-c(1,3)]
# reorder the columns
city2<-city2[,c(3,1,2)]
# rename the columns names
colnames (city2)<-c("date","pollutant","value")
# check changes of column names are correct
colnames(city2)
# check the class of city2
class (city2)
# convert city2 to a dataframe
city2 <- as_data_frame(city2)
# call lares library to find and replace some values including NA
library (lares)
city2<-replaceall(city2, c("T00:00:00.000 h01", "T00:00:00.000 h02","T00:00:00.000 h03","T00:00:00.000 h04","T00:00:00.000 h05","T00:00:00.000 h06","T00:00:00.000 h07","T00:00:00.000 h08","T00:00:00.000 h09","T00:00:00.000 h10","T00:00:00.000 h11","T00:00:00.000 h12","T00:00:00.000 h13","T00:00:00.000 h14","T00:00:00.000 h15","T00:00:00.000 h16","T00:00:00.000 h17","T00:00:00.000 h18","T00:00:00.000 h19","T00:00:00.000 h20","T00:00:00.000 h21","T00:00:00.000 h22","T00:00:00.000 h23","T00:00:00.000 h24"), c(" 01:00:00", " 02:00:00", " 03:00:00", " 04:00:00"," 05:00:00", " 06:00:00"," 07:00:00", " 08:00:00"," 09:00:00", " 10:00:00"," 11:00:00", " 12:00:00"," 13:00:00", " 14:00:00"," 15:00:00", " 16:00:00"," 17:00:00", " 18:00:00"," 19:00:00", " 20:00:00"," 21:00:00", " 22:00:00"," 23:00:00", " 00:00:00"))
# Call openair library to use built-in functions
library(openair)
# Convert date column from characters to dates
city2$date<-as.POSIXct(city2$date,"%Y-%m-%d %H:%M:%S", tz="Europe/Madrid")
# Check date column now is a date or POSIXct
class(city2$date)
# Convert pollutant column from numeric to factor
city2$pollutant<-as.factor(city2$pollutant)
# Check previous conversion is ok
class(city2$pollutant)
# To know the different levels of the factor pollutant in order to draw figures
levels(city2$pollutant)
# Create a figure with hour, day, week, month variations of pollutants
timeVariation(city2, pollutant=c("O3","NO2","H2S","NO","HCNM","CO","SO2","HCT", "NOX","PM10"), main="Air pollution in Martorell (1991-2022)")
# Create another view of the previous data centered in one pollutant
trendLevel(city2, pollutant = "H2S", main="Hydrogen sulfide evolution in Martorell")
# Calculate daily means from hourly data of poly
daily<-timeAverage(city2NO2,avg.time = "day")
View(daily)
# Create a calendar plot showing values of pollutants with colours
calendarPlot(daily, pollutant="NO2", year="2021")
# Select only one pollutant of my database
city2NO2 <- subset(city2, pollutant=="NO2")
# Calculate yearly means from previous data
yearly<-timeAverage(city2NO2,avg.time = "year")
View(yearly)
wind<-read.csv("https://raw.githubusercontent.com/drfperez/openair/main/wind.csv")
# Remember to put your data instead of Martorell default data
View(wind)
# View your data
wind1<-wind[-c(1,2,5,7,8)]
# Delete some unnecessary columns
wind2<-pivot_wider(wind1,names_from = CODI_VARIABLE, values_from = VALOR_LECTURA)
# Convert rows containing wind data in columns
names(wind2)[names(wind2) == "31"] <- "wd"
# Rename column name to wd (wind direction)
names(wind2)[names(wind2) == "30"] <- "ws"
# Rename column name to ws (wind speed)
names(wind2)[names(wind2) == "DATA_LECTURA"] <- "date"
# Rename column name to date (compulsory name for openair library)
write.csv(wind2,"C:\\Users\\YOURCOMPUTERNAME\\Documents\\wind3.csv")
# Have a copy of ordered original csv data in your local computer.
wind3<-timeAverage(wind2, time.avg="hour") 
# Combine two databases in one database.
cityall<-merge(city2, wind3, by ="date")
# Remember to edit the path to be used in your computer
write.csv(cityall,"C:\\Users\\YOURCOMPUTERNAME\\Documents\\cityall.csv")
View (cityall)
pollutionRose(cityall, pollutant="O3")
