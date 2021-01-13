
sp1 <- read.csv("D:/Users/setjafet/Documents/Bedu/2da fase/070621/sp1.csv")
FTHG <- sp1$FTHG
FTAG <- sp1$FTAG
x <- table(FTHG)
y <- table(FTAG)
xy <- table(FTHG,FTAG)
