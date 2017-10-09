setwd("~/Google Drive/RAnalisis/CaudalesTempisque")
datos = read.csv("Datos_Caudal.csv", header = T)

library(dplyr)
library(lubridate)
datos = datos[-3]

datos$FECHA <- as.POSIXct(as.character(datos$FECHA), format="%d %b %y")

datos$Year = year(datos$FECHA)
datos$Month = month(datos$FECHA)

datos <- mutate(datos, FlowDay = CAUDAL * 86400)

datos_mes <- datos %>% group_by(Year, Month) %>% summarise(FlowMonth = sum (FlowDay))

datos_mes <- mutate(datos_mes, FlowMonthMill = FlowMonth / 1000000)

datos_mes$Month <- factor(datos_mes$Month)

promedio_mes <- datos_mes %>% group_by(Month) %>% summarise(MonthAvg = mean(FlowMonthMill))

library(ggplot2)
g = ggplot(datos_mes, aes(Month, FlowMonthMill))
g + geom_bar(stat = "identity") + 
  facet_wrap(~ Year, ncol = 3)

g2 = ggplot(datos_mes, aes(Month, FlowMonthMill))
g2 + geom_boxplot() + scale_y_log10()
  

