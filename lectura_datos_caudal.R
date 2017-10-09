setwd("~/Google Drive/RAnalisis/CaudalesTempisque")
datos = read.csv("Datos_Caudal.csv", header = T)
datos_viejos = read.csv("BD_Guardia.csv", header = T)

#preparación de datos_viejos
#separación de columnas
library(dplyr)
library(tidyr)

df <- datos_viejos %>%
  separate(Water.Year, c("Year1", "Year2")) 

df1 = subset(df, select = c(Year1, May:Dic))
df2 = subset(df, select = c(Year2, Ene:Abr))

colnames(df2)[1] <- "Year"
colnames(df1)[1] <- "Year"

cleandata <- full_join(df1, df2, by = "Year")
cleandata <- cleandata[, c("Year", "Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", "Dic")]

#cambiar nombres de meses a números
cleandata <- rename(cleandata, "01" = Ene, "02" = Feb, "03" = Mar, "04" = Abr, "05" = May, "06" = Jun, "07" = Jul, "08" = Ago, "09" = Set, "10" = Oct, "11" = Nov, "12" = Dic)

#convertir a tidy data set
cleandata <- gather(cleandata, "Month", "Flow", 2:13)

#ordenar por año y mes
cleandata <- arrange(cleandata, Year, Month)

#graficar
library(ggplot2)
cleandata$Year <- factor(cleandata$Year)
g = ggplot(cleandata, aes(Year, Flow))
g + geom_boxplot() + scale_y_log10()



#siguientes datos
class(datos)
datos = datos[-3]

datos$FECHA <- as.POSIXct(as.character(datos$FECHA), format="%d %b %y")
datos$FECHA = as.Date(datos$FECHA)

library(ggplot2)

qplot(x = FECHA, y = CAUDAL, data = datos)

g = ggplot(datos, aes(FECHA, CAUDAL))
g + geom_point()

library(lubridate)
library(dplyr)

datos$year = year(datos$FECHA)
datos$year <- factor(datos$year)

#grafico boxplot por año
g = ggplot(datos, aes(year, CAUDAL))
g + geom_boxplot() + scale_y_log10()

tally(datos_year) # número de registros por año
res_a = summarise(datos_year, mean(CAUDAL))

colnames(res_a) <- c("year", "caudal")

g2 = ggplot(res_a, aes(year, caudal))
g2 + geom_bar(stat = "identity") # gráfico caudal anual

datos$mes = month(datos$FECHA)

datos %>% group_by(year, mes) %>% summarise(sum(CAUDAL))

res_m = datos %>% group_by(year, mes) %>% summarise(sum(CAUDAL), FECHA = first(FECHA))
colnames(res_m) <- c("year", "mes", "caudal", "FECHA")
g3 = ggplot(res_m, aes(FECHA, caudal))
g3 + geom_bar(stat = "identity") # caudal mensual

#grafico por mes
datos$mes <- factor(datos$mes)
g = ggplot(datos, aes(mes, CAUDAL))
g + geom_boxplot() + scale_y_log10() 

library(scales)
library(gridExtra)
library(grid)

datos$year = year(datos$FECHA)
datos$juliano = yday(datos$FECHA) #convertir a día juliano

g = ggplot(datos, aes(juliano, CAUDAL))
g + geom_bar(stat = "identity") + facet_grid(. ~year)

g + geom_bar(stat = "identity") + facet_wrap(~ year, ncol = 3) +
  ggtitle("Caual del río Tempisque en la estación Guardia") +
  xlab("Día del año") + ylab("Caudal (m3/s)") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18))

#boxplots por mes para todos los años medidos
g = ggplot(datos, aes(mes, CAUDAL))
g + geom_boxplot() + facet_grid(. ~year)

g + stat_summary(fun.y= median, geom="point") + facet_wrap(~ year, ncol = 3) +
  ggtitle("Caual promedio diario en la estación Guardia") +
  xlab("Mes") + ylab("Caudal (m3/s)") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) + scale_y_log10() 

##

datos$mes = month(datos$FECHA)
datos %>% group_by(year, mes) %>% summarise(sum(CAUDAL))

res_m = datos %>% group_by(year, mes) %>% summarise(sum(CAUDAL), FECHA = first(FECHA))
colnames(res_m) <- c("year", "mes", "caudal", "FECHA")
head(res_m)

res_m$mes = as.factor(res_m$mes)

g2 = ggplot(res_m, aes(mes, caudal))
g2 + geom_bar(stat = "identity") + facet_grid(. ~year) 

g2 + geom_bar(stat = "identity") + facet_wrap(~ year, ncol = 3) +
  ggtitle("Caual del río Tempisque en la estación Guardia") +
  xlab("Mes") + ylab("Caudal (m3/s)") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) 

estacionLiberia = read.csv("807063.csv", header = T)
lluvia = estacionLiberia$DATE, estacionLiberia$PRCP

