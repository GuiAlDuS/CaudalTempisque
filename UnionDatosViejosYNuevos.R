# union de tablas para datos mensuales
# cargar los datos
setwd("~/Google Drive/RAnalisis/CaudalesTempisque")
datos = read.csv("Datos_Caudal.csv", header = T)
datos_viejos = read.csv("BD_Guardia.csv", header = T)

#acomodar datos_viejos
#separación de columnas
library(dplyr)
library(tidyr)

df <- datos_viejos %>%
  separate(Water.Year, c("Year1", "Year2")) 

df1 = subset(df, select = c(Year1, May:Dic))
df2 = subset(df, select = c(Year2, Ene:Abr))

colnames(df2)[1] <- "Year"
colnames(df1)[1] <- "Year"

datos_v_clean <- full_join(df1, df2, by = "Year")
datos_v_clean <- datos_v_clean[, c("Year", "Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", "Dic")]

#cambiar nombres de meses a números
datos_v_clean <- rename(datos_v_clean, "01" = Ene, "02" = Feb, "03" = Mar, "04" = Abr, "05" = May, "06" = Jun, "07" = Jul, "08" = Ago, "09" = Set, "10" = Oct, "11" = Nov, "12" = Dic)

#convertir a tidy data set
datos_v_clean <- gather(datos_v_clean, "Month", "Flow", 2:13)

#ordenar por año y mes
datos_v_clean <- arrange(datos_v_clean, Year, Month)

#convertir a numeros
datos_v_clean$Year = as.numeric(datos_v_clean$Year)
datos_v_clean$Month = as.numeric(datos_v_clean$Month)



#lectura de datos nuevos
library(lubridate)
datos = datos[-3]

datos$FECHA <- as.POSIXct(as.character(datos$FECHA), format="%d %b %y")

datos$Year = year(datos$FECHA)
datos$Month = month(datos$FECHA)

datos_n_clean <- datos %>% group_by(Year, Month) %>% summarise(Flow = mean (CAUDAL))

datos_total <- bind_rows(datos_n_clean, datos_v_clean)
datos_total <- distinct(datos_total, Year, Month, .keep_all = TRUE)

library(ggplot2)
#seleccionar datos entre 1980 y 2009
datos_80_09 <- filter(datos_total, Year > 1979)

#graficar datos entre 1980 y 2009
g = ggplot(datos_80_09, aes(factor(Year), Flow))
g + geom_boxplot() + scale_y_log10() + scale_x_discrete(breaks=c("1980", "1985", "1990", "1995", "2000", "2005", "2010"), labels=c("1980", "1985", "1990", "1995", "2000", "2005", "2010")) + xlab("Año") + ylab("Caudal promedio (m3/s)") + ggtitle("Caual del río Tempisque en la estación Guardia") + geom_smooth( se=F, aes(group=1))

#grafico boxplot por año
g = ggplot(datos_total, aes(factor(Year), Flow))
g + geom_boxplot() + scale_y_log10() + 
  scale_x_discrete(breaks=c("1955", "1960", "1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010"), labels=c("1955", "1960","1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010")) + xlab("Año") + ylab("Caudal promedio (m3/s)") + ggtitle("Caual del río Tempisque en la estación Guardia")

g2 = ggplot(datos_total, aes(factor(Year), Flow))
g2 + geom_bar(stat = "identity")+ xlab("Mes") + ylab("Caudal (m3/s)") + 
  scale_x_discrete(breaks=c("1955", "1960", "1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010"), labels=c("1955", "1960","1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010")) + xlab("Año") + ylab("Caudal promedio (m3/s)") + ggtitle("Caual promedio diario del río Tempisque en la estación Guardia") 



