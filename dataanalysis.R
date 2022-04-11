library(openair)

city2$date<-as.POSIXct(city2$date,"%Y-%m-%d %H:%M:%S", tz="Europe/Madrid")

class(city2$date)

city2$pollutant<-as.factor(city2$pollutant)

class(city2$pollutant)

timeVariation(city2, pollutant=c("O3","NO2","H2S","NO","HCNM","CO","SO2","HCT", "NOX","PM10"), main="Air pollution in Martorell (1991-2022)")

trendLevel(city2, pollutant = "H2S", main="Hydrogen sulfide evolution in Martorell")

daily<-timeAverage(city2NO2,avg.time = "day")
View(daily)

calendarPlot(daily, pollutant="NO2", year="2021")

city2NO2 <- subset(city2, pollutant=="NO2")

yearly<-timeAverage(city2NO2,avg.time = "year")
View(yearly)
