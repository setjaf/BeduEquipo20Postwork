library(dplyr)

# Ruta para el wd
path <- "~/BEDU/2da fase/Postworks/Sesion2"
setwd(path)
rm(path)

# URL de donde están las bases
url1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

# Cargamos las bases
fut_1718 <- read.csv(url1)
fut_1819 <- read.csv(url2)
fut_1920 <- read.csv(url3)

# Removemos los urls
rm(url1, url2, url3)

# Contenido de los dataframes
str(fut_1718)
str(fut_1819)
str(fut_1920)

head(fut_1718)
head(fut_1819)
head(fut_1920)

View(fut_1718)
View(fut_1819)
View(fut_1920)

summary(fut_1718)
summary(fut_1819)
summary(fut_1920)

# Seleccionamos las columnas a trabajar
fut_1718 <- fut_1718 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
fut_1819 <- fut_1819 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
fut_1920 <- fut_1920 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

# Damos el formato correcto (en este caso solo es la fecha la que está mal)
fut_1718 <- fut_1718 %>% mutate(Date = as.Date(Date, "%d/%m/%y"))
fut_1819 <- fut_1819 %>% mutate(Date = as.Date(Date, "%d/%m/%Y"))
fut_1920 <- fut_1920 %>% mutate(Date = as.Date(Date, "%d/%m/%Y"))

# Creamos un único df
fut_1720 <- rbind(fut_1718, fut_1819, fut_1920)

# Dado que se usará en el postwork 3 guardaremos el df final en el directorio
# apropiado
path_pw3 <- "~/BEDU/2da fase/Postworks/Sesion2"
write.csv(fut_1720, paste0(path_pw3, "/fut_1720.csv"))
rm(path_pw3)
