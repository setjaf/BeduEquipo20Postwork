library(dplyr)
library(ggplot2)

# Fijamos el wd
wd <- "~/Documentos/FCiencias/2021-1/BEDU/Fase_2/Sesion_4"
setwd(wd)
rm(wd)

# Corremos los datos obtenidos en el pw3 y solo conservamos la matriz de
# probabilidad y la tabla de frecuencias
setwd("~/..")
dir()
source("~/BEDU/2da fase/Postworks/Sesion3/Postwork_3.R")
rm(conj, fut_1720, p_GC, p_GV, aux, col, i, j, row)

# Calculamos los cocientes y los guardamos en una nueva matriz
prob_coc <- matrix(NA, ncol = 7, nrow = 9)

for (i in 1:9) {
    for (j in 1:7) {
        prob_coc[i,j] = prob[i,j]/(prob[i,8]*prob[10,j])
    }
}
prob_coc <- round(prob_coc,3)

# Para aplicar bootstrap debemos crear más tablas y calcular la matriz de
# probabilidades y luego la matriz de los cocientes. Como esto se hará muchas
# veces conviene crear una función que reciba una tabla de goles y regrese una
# matriz de probabilidades y a partir de ahí repetir lo de la parte anterior.
# Además conviene crear una función que haga los cocientes para poder hacerlo de
# forma rapida.

m_conj <- function(tabla){
    row = dim(tabla)[1]
    col = dim(tabla)[2]
    aux <- matrix(NA, nrow = row+1, ncol = col+1)
    for (i in 1:row) {
        for (j in 1:col) {
            aux[i,j] = tabla[i,j]/sum(tabla)
        }
    }
    aux[, (col+1)] = apply(aux[1:(row+1), 1:col], 1 ,sum)
    aux[(row+1),] = apply(aux[1:row, 1:(col+1)], 2, sum)
    return(round(aux,3))
}

m_coc <- function(probs){
    row = dim(probs)[1]
    col = dim(probs)[2]
    aux <- matrix(NA, nrow = (row-1), ncol = (col-1))
    for (i in 1:(row-1)) {
        for (j in 1:(col-1)) {
            aux[i,j] = probs[i,j]/(probs[i, col]*probs[row, j])
        }
    }
    return(round(aux,3))
}

# Creamos una lista que contendrá las columnas de goles como local y visitante
# repetidos por el número de iteraciones de bootstrap. Al hacer bootstrap se
# toma una muestra con reemplazo de la muestra original.
iters <- 5000
lista_goles <- vector(mode = "list", length = iters)

for (i in 1:iters){
    lista_goles[[i]] <- data.frame(Casa= sample(goles$Casa,
                                              size = length(goles$Casa),
                                              replace = T),
                                 Visitante = sample(goles$Visitante,
                                                    size = 
                                                        length(goles$Visitante),
                                                    replace = T))
}

# Creamos las listas que contienen a las matrices de probabilidades y la de
# cocientes, dado que ya tenemos las funciones esto se puede hacer usando la
# función lapply
lista_tabla <- lapply(lista_goles, table)
lista_mat_probs <- lapply(lista_tabla, m_conj)
lista_mat_coc <- lapply(lista_mat_probs, m_coc)

# Debido a la naturaleza aleatoria del muestreo ocurrirá que las matrices de 
# probabilidad no tendran la misma dimensión en todos los casos, por lo que se
# debe extraer la dimensión de la matriz más grande y a partir de ésta hacer una
# lista que contenga los valores de los cocientes en cada entrada.

aux <- sapply(lista_mat_coc, dim)
mrow <- max(aux[1, ])
mcol <- max(aux[2, ])

lista_coord <- vector(mode = "list", length = mrow*mcol)
aux <- c()
ind <- 1
for (i in 0:(mrow-1)) {
    for (j in 0:(mcol-1)) {
        aux[ind] = paste0("(",i,",",j, ")")
        ind = ind +1
    }
}
names(lista_coord) <- aux

# Finalmente llenamos la lista de coordenadas (x,y) con el valor del cociente
# de probabilidades en la entrada de la matriz (x,y).
for (names in names(lista_coord)) {
    i = as.numeric(strsplit(names, "")[[1]][2])
    j = as.numeric(strsplit(names, "")[[1]][4])
    for (k in 1:iters) {
        if ((i+1) > dim(lista_mat_coc[[k]])[1]) {
            next
        } else if ((j+1) > dim(lista_mat_coc[[k]])[2]) {
            next
        } else {
            lista_coord[[names]][k] = lista_mat_coc[[k]][i + 1 ,j + 1]
        }
    }
}

# Resulta conveniente hacer una matriz más que contenga los promedios obtenidos
# usando bootstrap, dado que no todas las matrices tienen la misma dimensión se
# deben omitir los NA al momento de hacer promedios
prob_boot <- matrix(sapply(lista_coord, mean, na.rm = T),
                    nrow = 9, ncol = 7, byrow = T)
prob_boot <- round(prob_boot, 3)

# Bonus track con los histogramas de los cocientes de probabilidades.
plot_hist <- function(GC, GV) {
    if (sum(paste0("(", GC, ",", GV, ")") == names(lista_coord)) == 1) {
        aux = which(names(lista_coord) == paste0("(", GC, ",", GV, ")"))
        plot = hist(lista_coord[[aux]],
                    main = paste("Histograma de probabilidades GC =", GC,
                                 "GV =", GV),
                    xlab = "Cociente de probabilidades", ylab = "Densidad",
                    probability = T, breaks = 50, col = "blue")
        abline(v = mean(lista_coord[[aux]], na.rm = T), lwd = 2, col = "red")
        return(plot)
    } else{
        print(paste("La combinación GC =", GC, "GV =", GV, "no existe"))
    }
    
}

par(mfrow = c(3, 4))
for (i in 0:(mrow-1)) {
    for (j in 0:(mcol-1)) {
        plot_hist(i,j)
    }
}

