wind<-read.csv("C://Users/YOURCOMPUTERNAME/Documents/wind.csv")

View(wind)

wind1←wind[-c(1,2,5,7,8)]

wind2<-pivot_wider(wind1,names_from = CODI_VARIABLE, values_from = VALOR_LECTURA)

names(wind2)[names(wind2) == "31"] <- "wd"

names(wind2)[names(wind2) == "30"] <- "ws"

names(wind2)[names(wind2) == "DATA_LECTURA"] <- "date"

write.csv(wind2,"C:\\Users\\YOURCOMPUTERNAME\\

wind3<-timeAverage(wind2, time.avg="hour") 


cityall<-merge(city2, wind3, by ="date")
 View (cityall)


pollutionRose(cityall,...)