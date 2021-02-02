library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# Establecemos el wd
wd <-"~/Documentos/FCiencias/2021-1/BEDU/Fase_2/Sesion_6"
setwd(wd)
rm(wd)

# Cargamos el csv
url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-06/Postwork/match.data.csv"
match.data <- read.csv(url)
rm(url)
glimpse(match.data)

# Agregamos una nueva columna sumagoles
match.data <- match.data %>% mutate(sumagoles = home.score + away.score)
glimpse(match.data)

# Promedio por meses. Primero cambiamos el tipo de dato en la columna date por
# una fecha, luego calculamos el promedio mensual
match.data$date <- ymd(match.data$date)
proms_month <- aggregate(match.data$sumagoles,
                        by = list(Mes = month(match.data$date),
                                 Anio = year(match.data$date)),
                        mean)
proms_month <- round(proms_month,3)

# Creamos la serie de tiempo y la graficamos
time_s <- ts(proms_month$x, start = c(2010, 8), end = c(2019,12),
             frequency = 12)
autoplot(time_s) +
    ggtitle("Serie de tiempo de la suma de goles promedio al mes") +
    xlab("Tiempo") + ylab("Suma de goles promedio")
