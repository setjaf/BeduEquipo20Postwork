library(mongolite)
library(dplyr)
library(lubridate)

# Establecemos el wd
wd <- "~/Documentos/FCiencias/2021-1/BEDU/Fase_2/Sesion_7"
setwd(wd)
rm(wd)

# Cargamos la base y la guardamos para despues subirla a MongoDB y para
# contrastar con la sintaxis de MongoDB
url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-07/Postwork/data.csv"
df <- read.csv(url)
names(df)[1] <- c("Id")
write.csv(df, "data.csv")
rm(url)

# Nos conectamos a MongoDB. Para esto la db y el nombre de la colección ya han
# sido creadas (NO HACKEARME POR FAVOR :( !!!)
my_collection <- mongo(collection = "match", db = "match_games",
                       url = "mongodb+srv://JuanMDB:%24%5Cnabla%24.B%3D0@cluster0.laumf.mongodb.net/test?authSource=admin&replicaSet=atlas-5cuoq9-shard-0&readPreference=primary&appname=MongoDB%20Compass&ssl=true")

# Contamos el numero de registros y verificamos con R
my_collection$count(); dim(df)[1]

# Consultamos el número de goles que metio RM el 20/12/15.
# NO EXISTE TAL FECHA EN EL DATAFRAME!!!!!!!!!
# LA SOLUCION PROPUESTA ES SUBIR A MONGODB UNA BASE CON LAS MISMAS COLUMNAS PERO
# EXPANDIENDO LAS FECHAS. ASI VAMOS A COMPLETAR LAS FECHAS DEL DATAFRAME
# data.csv

# Asi creamos un nuevo df. Para mayor comodidad quitaremos la primera columna de
# data.csv. Para evitar problemas debemos dar formato a las fechas 
df <- df[,-1]
colnames(df)
df$Date <- ymd(df$Date)
glimpse(df)

# Cargamos los df
url_1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
url_1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"

df_1516 <- read.csv(url_1516)
df_1617 <- read.csv(url_1617)
glimpse(df_1516) # La fecha tiene formato dd/mm/yy
glimpse(df_1617) # La fecha tiene formato dd/mm/yy

df_1516$Date <- dmy(df_1516$Date)
df_1617$Date <- dmy(df_1617$Date)

glimpse(df_1516) 
glimpse(df_1617) 

# Seleccionamos las columnas que tiene data.csv
df_1516 <- df_1516 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
df_1617 <- df_1617 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

# Fusionamos df y creamos un nuevo csv que subiremos a MongoDB
df_fin <- rbind(df_1516, df_1617, df)
write.csv(df_fin, "data_aumentado.csv")

# Antes de abrir otra conexión cerramos la conexión actual
my_collection$disconnect()

# Abrimos una nueva conexión (NO HACKEARME POR FAVOR :( !!!)
my_collection_2 <- mongo(collection = "match_aumentado", db = "match_games",
                       url = "mongodb+srv://JuanMDB:%24%5Cnabla%24.B%3D0@cluster0.laumf.mongodb.net/test?authSource=admin&replicaSet=atlas-5cuoq9-shard-0&readPreference=primary&appname=MongoDB%20Compass&ssl=true")

# Nuevamente veamos la longitud de la base y contrastamos con R
my_collection_2$count(); dim(df_fin)[1]

# Ahora si la fecha se encuentra, vamos por ella
(query <- my_collection_2$find('{"Date" : "2015-12-20",
                              "HomeTeam" : "Real Madrid"}'))

# Claramente el Real Madrid goleó al Vallecano 10-2 marcador final. Ahora
# cerramos la conexión
my_collection_2$disconnect()
