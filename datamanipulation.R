install.packages(c("dplyr", "tibble","openair","tidyverse", "lares"))
city<-read.csv('https://raw.githubusercontent.com/drfperez/openair/main/city.csv')
library (tidyverse)
city1<-pivot_longer(city,cols=c(h01,h02,h03,h04,h05,h06,h07,h08,h09,h10,h11,h12,h13,h14, h15,h16,h17,h18,h19,h20,h21,h22,h23,h24), names_to="hour", values_to = "value")
city2<-city1[-c(1,2,4,6:16)]
library(dplyr)
city2 <- city2 %>% mutate(date=paste0(data, " ", hour))


colnames (city2)

city2<-city2[-c(1,3)]
city2<-city2[,c(3,1,2)]

colnames (city2)<-c("date","pollutant","value")

colnames(city2)

class (city2)

city2 <- as_data_frame(city2)

library (lares)

city2<-replaceall(city2, c("T00:00:00.000 h01", "T00:00:00.000 h02","T00:00:00.000 h03","T00:00:00.000 h04","T00:00:00.000 h05","T00:00:00.000 h06","T00:00:00.000 h07","T00:00:00.000 h08","T00:00:00.000 h09","T00:00:00.000 h10","T00:00:00.000 h11","T00:00:00.000 h12","T00:00:00.000 h13","T00:00:00.000 h14","T00:00:00.000 h15","T00:00:00.000 h16","T00:00:00.000 h17","T00:00:00.000 h18","T00:00:00.000 h19","T00:00:00.000 h20","T00:00:00.000 h21","T00:00:00.000 h22","T00:00:00.000 h23","T00:00:00.000 h24"), c(" 01:00:00", " 02:00:00", " 03:00:00", " 04:00:00"," 05:00:00", " 06:00:00"," 07:00:00", " 08:00:00"," 09:00:00", " 10:00:00"," 11:00:00", " 12:00:00"," 13:00:00", " 14:00:00"," 15:00:00", " 16:00:00"," 17:00:00", " 18:00:00"," 19:00:00", " 20:00:00"," 21:00:00", " 22:00:00"," 23:00:00", " 00:00:00"))






