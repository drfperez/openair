wind<-read.csv("https://raw.githubusercontent.com/drfperez/openair/main/wind.csv")
# Remember to put your data instead of Martorell default data
View(wind)
# View your data
wind1←wind[-c(1,2,5,7,8)]
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
