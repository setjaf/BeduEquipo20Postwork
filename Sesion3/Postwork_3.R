library(dplyr)
library(ggplot2)

# Ruta para el wd
path <- "~/BEDU/2da fase/Postworks/Sesion3"
setwd(path)
rm(path)

# Leemos el df que se obtuvo al final del pw2
fut_1720 <- read.csv("fut_1720.csv")

# Analogamente al pw1 se seleccionan solamente las columnas necesarias
goles <- fut_1720 %>% mutate(Casa = FTHG, Visitante = FTAG) %>%
    select(Casa, Visitante)

# Tabla de goles
tabla <- table(goles)
row <- dim(tabla)[1]
col <- dim(tabla)[2]

# Matriz de probabilidades
prob <- matrix(NA, nrow = row + 1, ncol = col + 1)

for (i in 1:(row)) {
    for (j in 1:(col)) {
        prob[i, j] <- tabla[i, j]/sum(tabla)
    }
}
prob[, col+1] <- apply(prob[1:(row+1), 1:col], 1, sum)
prob[row+1, ] <- apply(prob[1:row, 1:(col+1)], 2, sum)
prob <- round(prob, 3)

# Construcción de los df (esto para poder usar ggplot2)
p_GC <- data.frame(Goles = factor(0:8), Probabilidad = prob[1:row,col+1])
p_GV <- data.frame(Goles = factor(0:6), Probabilidad = prob[row+1,1:col])

p_GC %>% ggplot(aes(x=Goles, y = Probabilidad)) + geom_bar(stat = "identity") +
    ggtitle("Probabilidad marginal de los goles como local.")

p_GV %>% ggplot(aes(x=Goles, y = Probabilidad)) + geom_bar(stat = "identity") +
    ggtitle("Probabilidad marginal de los goles como visitante.")

grid <- expand.grid(X = 0:8, Y = 0:6)
conj <- data.frame(cbind(grid, rep(NA)))
rm(grid)

aux <- 1
for (i in 1:7) {
    for (j in 1:9) {
        conj[aux, 3] = prob[j, i]
        aux = aux + 1
    }
}

colnames(conj) <- c("Local", "Visitante", "Probabilidad")
conj$Local <- factor(conj$Local)
conj$Visitante <- factor(conj$Visitante)

conj %>% ggplot(aes(Local, Visitante, fill = Probabilidad)) +
    geom_tile(colour = "#4058ce") +
    ggtitle("Probabilidad conjunta de goles.")

