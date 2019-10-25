
rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(agricolae);
library(VGAM);

#---------------------------------------------------------------------------------#
#--- Variáveis aleatórias contínuas                                            ---#
#---------------------------------------------------------------------------------#

#---- exemplo de variável aleatória contínua

x <- (0:10000) / 1000;
length(x)

px <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    px[i] = exp(-x[i])
    Fx[i] = 1 - exp(-x[i])
}

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

plot(x, px, col="red", main = "Função de densidade de exemplo", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "p(x)")

plot(x, Fx, col="blue", main = "Função distribuição acumulada de exemplo", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x)")

#---- fazendo a integração numérica

integrand <- function(x) {exp(-x)}
integrate(integrand, lower = 0, upper = 2)

#---- calculando média e variância numericamente

integrand1 <- function(x) {x * exp(-x)}
media <- integrate(integrand1, lower = 0, upper = Inf)
media

integrand2 <- function(x) {(x-1)^2 * exp(-x)}
variancia <- integrate(integrand2, lower = 0, upper = Inf)
variancia

#---------------------------------------------------------------------------------#
#--- Variáveis aleatórias contínuas 'template'                                 ---#
#---------------------------------------------------------------------------------#

#---- variável aleatória gamma

alpha <- 8.8
delta <- 1/8.3

pgamma(70, alpha, delta)

(1 - pgamma(70, alpha, delta))/(1 - pgamma(50, alpha, delta))

pgamma(30, alpha, delta)

pgamma(26, alpha, delta)
pgamma(27, alpha, delta) - pgamma(26, alpha, delta)


x <- (-100:800)/5
fx <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    fx[i] <- dgamma(x[i], alpha, delta)
    Fx[i] <- pgamma(x[i], alpha, delta)
}

rx <- rgamma(10000, alpha, delta)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de densidade da gamma", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Função dist. acumulada da gamma", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável gamma", 
     xlab = "Valores gerados", ylab = "Frequência")

media_teorica <- alpha / delta; media_teorica
media_simulacoes <- mean(rx); media_simulacoes

sd_teorica <- sqrt(alpha / (delta^2)); sd_teorica
sd_simulacoes <- sd(rx); sd_simulacoes

#---- variável aleatória normal

mu <- 4.0
sigma <- 0.25

x <- (-400:400)/40
fx <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    fx[i] <- dnorm(x[i], mean = mu, sd = sigma)
    Fx[i] <- pnorm(x[i], mean = mu, sd = sigma)
}

rx <- rnorm(10000, mean = mu, sd = sigma)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de densidade da Normal", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Função dist. acumulada da Normal", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável Normal", 
     xlab = "Valores gerados", ylab = "Frequência")

pnorm(3.5, mu, sigma)
1 - pnorm(4.5, mu, sigma)
pnorm(4.3, mu, sigma) - pnorm(3.7, mu, sigma)

pnorm(4.579, mu, sigma)

qnorm(0.99, mu, sigma)

qnorm(0.05, mu, sigma)

qnorm(0.005, mu, sigma)

mean(rx)
sd(rx)
var(rx)

mu <- 4
sigma <- 0.25
pnorm(3.5, mean = 4, sd = 0.25)
pnorm(4.5, mean = 4, sd = 0.25, lower.tail = FALSE)

qnorm(0.01, mean = 4, sd = 0.25)
qnorm(0.05, mean = 4, sd = 0.25, lower.tail = FALSE)

#---- variável aleatória exponencial negativa

lambda <- 0.12

x <- (-100:600)/40
fx <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    fx[i] <- dexp(x[i], lambda)
    Fx[i] <- pexp(x[i], lambda)
}

rx <- rexp(10000, lambda)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de densidade da exponencial negativa", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Função dist. acumulada da exponencial negativa", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável exponencial negativa", 
     xlab = "Valores gerados", ylab = "Frequência")

#---- variável aleatória de Weibull 

alpha <- 2.3
beta <- 3.12

x <- (-10:300)/40
fx <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    fx[i] <- dweibull(x[i], alpha, beta)
    Fx[i] <- pweibull(x[i], alpha, beta)
}

rx <- rweibull(10000, alpha, beta)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de densidade da Weibull", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Função dist. acumulada da Weibull", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável Weibull", 
     xlab = "Valores gerados", ylab = "Frequência")

#---- variável aleatória lognormal

mu <- 2.0
sigma <- 1.4

x <- (-40:900)/40
fx <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    fx[i] <- dlnorm(x[i], mean = mu, sd = sigma)
    Fx[i] <- plnorm(x[i], mean = mu, sd = sigma)
}

rx <- rlnorm(10000, mean = mu, sd = sigma)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de densidade da LogNormal", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Função dist. acumulada da LogNormal", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável LogNormal", 
     xlab = "Valores gerados", ylab = "Frequência")

#---- variável aleatória de Rayleigh (precisa do pacote VGAM)

beta <- 2

x <- (-100:600)/40
fx <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    fx[i] <- drayleigh(x[i], beta)
    Fx[i] <- prayleigh(x[i], beta)
}

rx <- rrayleigh(10000, beta)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de densidade da var. de Rayleigh", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Função dist. acumulada da var. de Rayleigh", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável de Rayleigh", 
     xlab = "Valores gerados", ylab = "Frequência")

#---- variável aleatória beta

alpha <- 2.3
beta <- 3.12

x <- (-50:450)/400
fx <- rep(0, length(x))
Fx <- rep(0, length(x))

for (i in 1:length(x))
{
    fx[i] <- dbeta(x[i], alpha, beta)
    Fx[i] <- pbeta(x[i], alpha, beta)
}

rx <- rbeta(10000, alpha, beta)

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Função de densidade da dist. beta", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Função dist. acumulada da dist. beta", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a variável beta", 
     xlab = "Valores gerados", ylab = "Frequência")

#---------------------------------------------------------------------------------#
#--- the end                                                                   ---#
#---------------------------------------------------------------------------------#
