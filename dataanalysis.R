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
