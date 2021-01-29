library(dplyr)
library(fbRanks)

# Directorio de trabajo
wd <- "~/BEDU/2da fase/Postworks/Sesion5"
setwd(wd)
rm(wd)

# Cargamos los datos
url_1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url_1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url_1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

fut_1718 <- read.csv(url_1718)
fut_1819 <- read.csv(url_1819)
fut_1920 <- read.csv(url_1920)
rm(url_1718, url_1819, url_1920)

# Analizamos las variables
glimpse(fut_1718)
glimpse(fut_1819)
glimpse(fut_1920)

# Seleccionamos variables
aux_1 <- fut_1718 %>% select(Date, HomeTeam, FTHG, AwayTeam, FTAG) %>%
    transmute(date = Date, home.team = HomeTeam, home.score = FTHG,
           away.team = AwayTeam, away.score = FTAG)
aux_2 <- fut_1819 %>% select(Date, HomeTeam, FTHG, AwayTeam, FTAG) %>%
    transmute(date = Date, home.team = HomeTeam, home.score = FTHG,
           away.team = AwayTeam, away.score = FTAG)
aux_3 <- fut_1920 %>% select(Date, HomeTeam, FTHG, AwayTeam, FTAG) %>%
    transmute(date = Date, home.team = HomeTeam, home.score = FTHG,
           away.team = AwayTeam, away.score = FTAG)

# Nuevamente hay que arreglar el formato de la fecha
aux_1$date <- as.Date(aux_1$date, format = "%d/%m/%y")
aux_2$date <- as.Date(aux_2$date, format = "%d/%m/%Y")
aux_3$date <- as.Date(aux_3$date, format = "%d/%m/%Y")

# Ya podemos combinar los df's en uno solo
SmallData <- rbind(aux_1, aux_2, aux_3)
rm(aux_1, aux_2, aux_3, fut_1718, fut_1819, fut_1920)

# Escribimos el df
write.csv(SmallData, "soccer.csv", row.names = F)

# Importamos el .csv usando fbRanks
listasoccer <- create.fbRanks.dataframes("soccer.csv")

# Filtramos los df que arroja create.fb...
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

# Creamos un vector de fechas únicas
fecha <- unique(SmallData$date)
n <- length(fecha)

# Corremos la funcion rank.teams
ranking <- rank.teams(anotaciones, equipos, min.date = fecha[1],
                      max.date = fecha[(n-1)])

# Vamos a predecir los resultados de la ultima fecha
pred <- predict(ranking, date = fecha[n])

# Creamos un df con las probabilidades
preds <- data.frame(Fecha = pred$scores$date, Eq_local = pred$scores$home.team,
                    Eq_visita = pred$scores$away.team,
                    Prob_local_win = pred$scores$home.win,
                    Prob_visita_win = pred$scores$away.win,
                    Prob_empate = pred$scores$tie)
