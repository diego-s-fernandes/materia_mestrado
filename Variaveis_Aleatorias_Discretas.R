
rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(agricolae);

#---------------------------------------------------------------------------------#
#--- Variáveis aleatórias discretas                                            ---#
#---------------------------------------------------------------------------------#

#--- variável aleatória binomial

N <- 100
p <- 0.2

v0 <- dbinom(0, N, p); v0
v1 <- dbinom(1, N, p); v1
v2 <- dbinom(2, N, p); v2
v50 <- dbinom(50, N, p); v50
v100 <- dbinom(100, N, p); v100

fx <- rep(0, 101)
Fx <- rep(0, 101)
x <- 0:100

for (i in x)
{
    fx[i+1] <- dbinom(i, N, p)
    Fx[i+1] <- pbinom(i, N, p)
}

rx <- rbinom(2000, N, p)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de frequência para a distribuição binomial",
           xlab = "valores de x", ylab = "f(x) = Prob[X = x]")

plot(x, Fx, col="blue", main = "Função dist. acumulada para a distribuição binomial",
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 20, col = "green", main = "Histograma para a variável binomial", 
     xlab = "Valores gerados", ylab = "Frequência")

mean(rx)
sd(rx)
var(rx)

#--- variável aleatória de Poisson

lambda <- 5.3

fx <- rep(0, 41)
Fx <- rep(0, 41)
x <- 0:40

for (i in x)
{
    fx[i+1] <- dpois(i, lambda)
    Fx[i+1] <- ppois(i, lambda)
}

rx <- rpois(2000, lambda)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de frequência para a distribuição de Poisson",
     xlab = "valores de x", ylab = "f(x) = Prob[X = x]")

plot(x, Fx, col="blue", main = "Função dist. acumulada para a distribuição de Poisson",
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 20, col = "green", main = "Histograma para a variável de Poisson", 
     xlab = "Valores gerados", ylab = "Frequência")

#--- variável aleatória geométrica

p <- 0.07

fx <- rep(0, 41)
Fx <- rep(0, 41)
x <- 0:40

for (i in x)
{
    fx[i+1] <- dgeom(i, p)
    Fx[i+1] <- pgeom(i, p)
}

rx <- rgeom(2000, p)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de frequência para a distribuição geométrica",
     xlab = "valores de x", ylab = "f(x) = Prob[X = x]")

plot(x, Fx, col="blue", main = "Função dist. acumulada para a distribuição geométrica",
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 20, col = "green", main = "Histograma para a variável geométrica", 
     xlab = "Valores gerados", ylab = "Frequência")

#--- variável aleatória binomial negativa

p <- 0.07
r <- 3.2

fx <- rep(0, 101)
Fx <- rep(0, 101)
x <- 0:100

for (i in x)
{
    fx[i+1] <- dnbinom(i, r, p)
    Fx[i+1] <- pnbinom(i, r, p)
}

rx <- rnbinom(2000, r, p)

rx
par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de frequência para a dist. bin. negativa",
     xlab = "valores de x", ylab = "f(x) = Prob[X = x]")

plot(x, Fx, col="blue", main = "Função dist. acumulada para a dist. bin. negativa",
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável bin. negativa", 
     xlab = "Valores gerados", ylab = "Frequência")

#---------------------------------------------------------------------------------#
#--- the end                                                                   ---#
#---------------------------------------------------------------------------------#
