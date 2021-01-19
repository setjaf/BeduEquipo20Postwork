library(dplyr)

# Seleccionamos la ruta del wd
wd <- "/home/juan/Documentos/FCiencias/2021-1/BEDU/Fase_2/Sesion_1"
setwd(wd)
rm(wd)

# URL en donde se encuentra la base
url <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

# Cargamos la base
fut_1920 <- read.csv(url)
rm(url)

# Seleccionamos solo las columnas de interes
goles <- fut_1920 %>% mutate(Casa = FTHG, Visitante = FTAG) %>%
    select(Casa, Visitante)

# Tabla de goles
tabla <- table(goles)

# Matriz en donde se guardarán las probabilidades
prob <- matrix(NA, nrow = 8, ncol = 7)

# Llenamos la matriz y la redondeamos a 3 cifras
for (i in 1:7) {
    for (j in 1:6) {
        prob[i, j] <- tabla[i, j]/sum(tabla)
    }
}
prob[, 7] <- apply(prob[1:8, 1:6], 1, sum)
prob[8, ] <- apply(prob[1:7, 1:7], 2, sum)
round(prob, 3)

p_GC <- data.frame(Goles = factor(0:6), Probabilidad = prob[1:7,7])
p_GV <- data.frame(Goles = factor(0:5), Probabilidad = prob[8, 1:6])
p_conj <- data.frame(prob[1:7, 1:6], row.names = c("GC0", "GC1", "GC2", "GC3",
                                                   "GC4", "GC5", "GC6"))

colnames(p_conj) <- c("GV0", "GV1", "GV2", "GV3", "GV4", "GV5")
