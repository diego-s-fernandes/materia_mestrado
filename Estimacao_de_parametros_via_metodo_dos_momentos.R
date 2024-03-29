
rm(list=ls()); #---- limpa todo o ambiente de vari�veis

library(plyr);
library(agricolae);
library(VGAM);

#-----------------------------------------------
#--- vari�vel aleat�ria de Poisson
#-----------------------------------------------

lambda <- 3

fx <- rep(0, 41)
Fx <- rep(0, 41)
x <- 0:40

for (i in x)
{
    fx[i+1] <- dpois(i, lambda)
    Fx[i+1] <- ppois(i, lambda)
}

rx <- rpois(2000, lambda)
media_empirica <- mean(rx); media_empirica
var_empirica <- var(rx); var_empirica

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Fun��o de frequ�ncia para a distribui��o de Poisson",
     xlab = "valores de x", ylab = "f(x) = Prob[X = x]")

plot(x, Fx, col="blue", main = "Fun��o dist. acumulada para a distribui��o de Poisson",
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 20, col = "green", main = "Histograma para uma amostra da vari�vel de Poisson", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

#---- exemplo do m�todo de momentos

X <- c(2, 6, 8, 11, 5, 4, 13, 3, 0, 2, 0, 12)
lambda1 <- mean(X); lambda1
lambda2 <- var(X); lambda2

#----- simula��es de Monte Carlo para estudar as propriedades dos estimadores de m�todos de momentos

n <- 5
lambda0 <- 3.0;

amostra <- rpois(n, lambda0)
amostra

lambda1 <- mean(amostra); lambda1
lambda2 <- var(amostra); lambda2

nsimul <- 20000;

lambda1_simul <- matrix(nrow = nsimul, ncol = 1)
lambda2_simul <- matrix(nrow = nsimul, ncol = 1)

for (i in 1:nsimul)
{
    amostra <- rpois(n, lambda0)
    
    lambda1 <- mean(amostra); 
    lambda2 <- var(amostra);
    
    lambda1_simul[i] <- lambda1;
    lambda2_simul[i] <- lambda2;
}

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

hist(lambda1_simul, breaks = 20, col = "red", main = "Histograma do Estimador de MM com a M�dia", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

hist(lambda2_simul, breaks = 20, col = "green", main = "Histograma do Estimador de MM com a Vari�ncia", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

mean(lambda1_simul); sd(lambda1_simul)
mean(lambda2_simul); sd(lambda2_simul)

#---- efeito do tamanho da amostra nas estimativas

n_amostras <- c(5, 10, 20, 100, 200, 400, 1000, 2000)
sds1 <- matrix(nrow = length(n_amostras), ncol = 1);
sds2 <- matrix(nrow = length(n_amostras), ncol = 1)

means1 <- matrix(nrow = length(n_amostras), ncol = 1);
means2 <- matrix(nrow = length(n_amostras), ncol = 1)

obs <- 0

for (k in n_amostras)
{
    obs <- obs+1
    
    lambda1_simul <- matrix(nrow = nsimul, ncol = 1)
    lambda2_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rpois(k, lambda0)
        
        lambda1 <- mean(amostra); 
        lambda2 <- var(amostra);
        
        lambda1_simul[i] <- lambda1;
        lambda2_simul[i] <- lambda2;
    }
    
    means1[obs] <- mean(lambda1_simul)
    means2[obs] <- mean(lambda2_simul)
    
    sds1[obs] <- sd(lambda1_simul)
    sds2[obs] <- sd(lambda2_simul)
}

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, means1, col="red", type="o", main = "MM usando a M�dia",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(lambda0, length(n_amostras)), col = "black")

plot(n_amostras, means2, col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(lambda0, length(n_amostras)), col = "black")

plot(n_amostras, sds1, col="red", type="o", main = "MM usando a M�dia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, sds1, col="red", type="o", main = "MM usando a M�dia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds1), col="red", type="o", main = "MM usando a M�dia",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds2), col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

lm1 <- lm(log(sds1) ~ log(n_amostras))
lm1

lm2 <- lm(log(sds2) ~ log(n_amostras))
lm2

#---- o que acontece com a forma da distribui��o das estimativas quando
#---- a amostra aumenta?

par(mfrow = c(2,4));
par(mar = c(4,4,2,2));

n_amostras <- c(5, 10, 100, 2000)

for (k in n_amostras)
{
    lambda1_simul <- matrix(nrow = nsimul, ncol = 1)
    lambda2_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rpois(k, lambda0)
        
        lambda1 <- mean(amostra); 
        lambda2 <- var(amostra);
        
        lambda1_simul[i] <- lambda1;
        lambda2_simul[i] <- lambda2;
    }
    
    hist(lambda1_simul, breaks = 20, col = "red", main = paste("MM com m�dia com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
    
    hist(lambda2_simul, breaks = 20, col = "green", main = paste("MM com vari�ncia com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
}

#-----------------------------------------------
#---- vari�vel aleat�ria exponencial negativa
#-----------------------------------------------

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
media_empirica <- mean(rx); media_empirica
var_empirica <- var(rx); var_empirica

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(x, fx, col="red", main = "Fun��o de densidade da exponencial negativa", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Fun��o dist. acumulada da exponencial negativa", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a vari�vel exponencial negativa", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

#---- exemplo de estima��o com m�todo dos momentos

amostra <- c(2.3, 1.5, 2.1, 6.3, 2.2,
             6.1, 5.3, 0.8, 8.1, 4.12,
             0.2, 3.12, 7.42, 8.3, 15.6,
             9.6, 2.3, 21.3, 4.3, 2.8)

m <- mean(amostra); m
v <- var(amostra); v

lambda1 <- 1/m; lambda1
lambda2 <- 1/(v^0.5); lambda2

#----- simula��es de Monte Carlo para estudar as propriedades dos estimadores de m�todos de momentos

n <- 5
lambda0 <- 0.12;

amostra <- rexp(n, lambda0)
amostra

lambda1 <- 1/mean(amostra); lambda1
lambda2 <- 1/sd(amostra); lambda2

nsimul <- 20000;

lambda1_simul <- matrix(nrow = nsimul, ncol = 1)
lambda2_simul <- matrix(nrow = nsimul, ncol = 1)

for (i in 1:nsimul)
{
    amostra <- rexp(n, lambda0)
    
    lambda1 <- 1/mean(amostra); 
    lambda2 <- 1/sd(amostra);
    
    lambda1_simul[i] <- lambda1;
    lambda2_simul[i] <- lambda2;
}

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

hist(lambda1_simul, breaks = 20, col = "red", main = "Histograma do Estimador de MM com a M�dia", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

hist(lambda2_simul, breaks = 20, col = "green", main = "Histograma do Estimador de MM com a Vari�ncia", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

mean(lambda1_simul); sd(lambda1_simul)
mean(lambda2_simul); sd(lambda2_simul)

#---- efeito do tamanho da amostra nas estimativas

n_amostras <- c(5, 10, 20, 100, 200, 400, 1000, 2000)
sds1 <- matrix(nrow = length(n_amostras), ncol = 1);
sds2 <- matrix(nrow = length(n_amostras), ncol = 1)

means1 <- matrix(nrow = length(n_amostras), ncol = 1);
means2 <- matrix(nrow = length(n_amostras), ncol = 1)

obs <- 0

for (k in n_amostras)
{
    obs <- obs+1
    
    lambda1_simul <- matrix(nrow = nsimul, ncol = 1)
    lambda2_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rexp(k, lambda0)
        
        lambda1 <- 1/mean(amostra); 
        lambda2 <- 1/sd(amostra);
        
        lambda1_simul[i] <- lambda1;
        lambda2_simul[i] <- lambda2;
    }
    
    means1[obs] <- mean(lambda1_simul)
    means2[obs] <- mean(lambda2_simul)
    
    sds1[obs] <- sd(lambda1_simul)
    sds2[obs] <- sd(lambda2_simul)
}

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, means1, col="red", type="o", main = "MM usando a M�dia",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(lambda0, length(n_amostras)), col = "black")

plot(n_amostras, means2, col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(lambda0, length(n_amostras)), col = "black")

plot(n_amostras, sds1, col="red", type="o", main = "MM usando a M�dia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, sds1, col="red", type="o", main = "MM usando a M�dia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds1), col="red", type="o", main = "MM usando a M�dia",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds2), col="blue", type="o", main = "MM usando a vari�ncia",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

lm1 <- lm(log(sds1) ~ log(n_amostras))
lm1

lm2 <- lm(log(sds2) ~ log(n_amostras))
lm2

#---- o que acontece com a forma da distribui��o das estimativas quando
#---- a amostra aumenta?

par(mfrow = c(2,4));
par(mar = c(4,4,2,2));

n_amostras <- c(5, 10, 100, 2000)

for (k in n_amostras)
{
    lambda1_simul <- matrix(nrow = nsimul, ncol = 1)
    lambda2_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rexp(k, lambda0)
        
        lambda1 <- 1/mean(amostra); 
        lambda2 <- 1/sd(amostra);
        
        lambda1_simul[i] <- lambda1;
        lambda2_simul[i] <- lambda2;
    }
    
    hist(lambda1_simul, breaks = 20, col = "red", main = paste("MM com m�dia com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
    
    hist(lambda2_simul, breaks = 20, col = "green", main = paste("MM com vari�ncia com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
}

#-----------------------------------------------
#---- vari�vel aleat�ria gamma
#-----------------------------------------------

alpha <- 12
delta <- 1/8

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

plot(x, fx, col="red", main = "Fun��o de densidade da gamma", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Fun��o dist. acumulada da gamma", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a vari�vel gamma", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

media_teorica <- alpha / delta; media_teorica
media_simulacoes <- mean(rx); media_simulacoes

sd_teorica <- sqrt(alpha / (delta^2)); sd_teorica
sd_simulacoes <- sd(rx); sd_simulacoes

#---- exemplo de estima��o com m�todo de momentos

amostra <- c(2.3, 1.5, 2.1, 6.3, 2.2,
             6.1, 5.3, 0.8, 8.1, 4.12,
             0.2, 3.12, 7.42, 8.3, 15.6,
             9.6, 2.3, 21.3, 4.3, 2.8)

m <- mean(amostra); m
v <- var(amostra); v

beta_hat <- var(amostra) / mean(amostra); beta_hat
alpha_hat <- (mean(amostra)^2) / var(amostra); alpha_hat

#----- simula��es de Monte Carlo para estudar as propriedades dos estimadores de m�todos de momentos

n <- 5

alpha0 <- 12
beta0 <- 8

amostra <- rgamma(n, alpha0, 1/beta0)
amostra

beta_hat <- var(amostra) / mean(amostra); beta_hat
alpha_hat <- (mean(amostra)^2) / var(amostra); alpha_hat

nsimul <- 20000;

beta_hat_simul <- matrix(nrow = nsimul, ncol = 1)
alpha_hat_simul <- matrix(nrow = nsimul, ncol = 1)

for (i in 1:nsimul)
{
    amostra <- rgamma(n, alpha0, 1/beta0)
    
    beta_hat <- var(amostra) / mean(amostra); beta_hat
    alpha_hat <- (mean(amostra)^2) / var(amostra); alpha_hat
    
    beta_hat_simul[i] <- beta_hat;
    alpha_hat_simul[i] <- alpha_hat;
}

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

hist(alpha_hat_simul, breaks = 20, col = "red", main = "Histograma do Estimador de Alpha", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

hist(beta_hat_simul, breaks = 20, col = "green", main = "Histograma do Estimador de Beta", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

mean(alpha_hat_simul); sd(alpha_hat_simul)
mean(beta_hat_simul); sd(beta_hat_simul)

#---- efeito do tamanho da amostra nas estimativas

n_amostras <- c(10, 20, 100, 200, 400, 1000, 2000)
sds1 <- matrix(nrow = length(n_amostras), ncol = 1);
sds2 <- matrix(nrow = length(n_amostras), ncol = 1)

means1 <- matrix(nrow = length(n_amostras), ncol = 1);
means2 <- matrix(nrow = length(n_amostras), ncol = 1)

obs <- 0

for (k in n_amostras)
{
    obs <- obs+1
    
    beta_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    alpha_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rgamma(k, alpha0, 1/beta0)
        
        beta_hat <- var(amostra) / mean(amostra); beta_hat
        alpha_hat <- (mean(amostra)^2) / var(amostra); alpha_hat
        
        beta_hat_simul[i] <- beta_hat;
        alpha_hat_simul[i] <- alpha_hat;
    }
    
    means1[obs] <- mean(alpha_hat_simul)
    means2[obs] <- mean(beta_hat_simul)
    
    sds1[obs] <- sd(alpha_hat_simul)
    sds2[obs] <- sd(beta_hat_simul)
}

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, means1, col="red", type="o", main = "MM para Alpha",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(alpha0, length(n_amostras)), col = "black")

plot(n_amostras, means2, col="blue", type="o", main = "MM para Beta",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(beta0, length(n_amostras)), col = "black")

plot(n_amostras, sds1, col="red", type="o", main = "MM para Alpha",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM para Beta",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, sds1, col="red", type="o", main = "MM para Alpha",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM para Beta",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds1), col="red", type="o", main = "MM para Alpha",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds2), col="blue", type="o", main = "MM para Beta",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

lm1 <- lm(log(sds1) ~ log(n_amostras))
lm1

lm2 <- lm(log(sds2) ~ log(n_amostras))
lm2

#---- o que acontece com a forma da distribui��o das estimativas quando
#---- a amostra aumenta?

par(mfrow = c(2,4));
par(mar = c(4,4,2,2));

n_amostras <- c(5, 10, 100, 2000)

for (k in n_amostras)
{
    beta_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    alpha_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rgamma(k, alpha0, 1/beta0)
        
        beta_hat <- var(amostra) / mean(amostra); beta_hat
        alpha_hat <- (mean(amostra)^2) / var(amostra); alpha_hat
        
        beta_hat_simul[i] <- beta_hat;
        alpha_hat_simul[i] <- alpha_hat;
    }
    
    hist(alpha_hat_simul, breaks = 20, col = "red", main = paste("Alpha hat com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
    
    hist(beta_hat_simul, breaks = 20, col = "green", main = paste("Beta hat com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
}

#-----------------------------------------------
#---- vari�vel aleat�ria normal
#-----------------------------------------------

mu <- 2.0
sigma <- 1.4

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

plot(x, fx, col="red", main = "Fun��o de densidade da Normal", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "f(x)")

plot(x, Fx, col="blue", main = "Fun��o dist. acumulada da Normal", type="o", pch='', lwd = 3,
     xlab = "valores de x", ylab = "F(x) = Prob[X <= x]")

hist(rx, breaks = 30, col = "green", main = "Histograma para a vari�vel Normal", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

mean(rx)
sd(rx)
var(rx)

#----- exemplo de m�todo de momentos

amostra <- c(2.3, 1.5, 2.1, 6.3, 2.2,
             6.1, 5.3, 0.8, 8.1, 4.12,
             0.2, 3.12, 7.42, 8.3, 15.6,
             9.6, 2.3, 21.3, 4.3, 2.8)

m <- mean(amostra); m
v <- var(amostra); v

#----- simula��es de Monte Carlo para estudar as propriedades dos estimadores de m�todos de momentos

n <- 5

mu0 <- 2.0
sigma0 <- 1.4

amostra <- rnorm(n, mu0, sigma0)
amostra

mu_hat <- mean(amostra); mu_hat
sigma_hat <- sd(amostra); sigma_hat

nsimul <- 20000;

mu_hat_simul <- matrix(nrow = nsimul, ncol = 1)
sigma_hat_simul <- matrix(nrow = nsimul, ncol = 1)

for (i in 1:nsimul)
{
    amostra <- rnorm(n, mu0, sigma0)
    
    mu_hat <- mean(amostra)
    sigma_hat <- sd(amostra)
    
    mu_hat_simul[i] <- mu_hat;
    sigma_hat_simul[i] <- sigma_hat;
}

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

hist(mu_hat_simul, breaks = 20, col = "red", main = "Histograma do Estimador de Mu", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

hist(sigma_hat_simul, breaks = 20, col = "green", main = "Histograma do Estimador de Sigma", 
     xlab = "Valores gerados", ylab = "Frequ�ncia")

mean(mu_hat_simul); sd(mu_hat_simul)
mean(sigma_hat_simul); sd(sigma_hat_simul)

#---- efeito do tamanho da amostra nas estimativas

n_amostras <- c(5, 10, 20, 100, 200, 400, 1000, 2000)
sds1 <- matrix(nrow = length(n_amostras), ncol = 1);
sds2 <- matrix(nrow = length(n_amostras), ncol = 1)

means1 <- matrix(nrow = length(n_amostras), ncol = 1);
means2 <- matrix(nrow = length(n_amostras), ncol = 1)

obs <- 0

for (k in n_amostras)
{
    obs <- obs+1
    
    mu_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    sigma_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rnorm(k, mu0, sigma0)
        
        mu_hat <- mean(amostra)
        sigma_hat <- sd(amostra)
        
        mu_hat_simul[i] <- mu_hat;
        sigma_hat_simul[i] <- sigma_hat;
    }
    
    means1[obs] <- mean(mu_hat_simul)
    means2[obs] <- mean(sigma_hat_simul)
    
    sds1[obs] <- sd(mu_hat_simul)
    sds2[obs] <- sd(sigma_hat_simul)
}

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, means1, col="red", type="o", main = "MM para  Mu",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(mu0, length(n_amostras)), col = "black")

plot(n_amostras, means2, col="blue", type="o", main = "MM para Sigma",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(sigma0, length(n_amostras)), col = "black")

plot(n_amostras, sds1, col="red", type="o", main = "MM para Mu",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM para Sigma",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, sds1, col="red", type="o", main = "MM para Mu",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM para Sigma",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds1), col="red", type="o", main = "MM para Mu",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds2), col="blue", type="o", main = "MM para Sigma",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

lm1 <- lm(log(sds1) ~ log(n_amostras))
lm1

lm2 <- lm(log(sds2) ~ log(n_amostras))
lm2

#---- o que acontece com a forma da distribui��o das estimativas quando
#---- a amostra aumenta?

par(mfrow = c(2,4));
par(mar = c(4,4,2,2));

n_amostras <- c(5, 10, 100, 2000)

for (k in n_amostras)
{
    mu_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    sigma_hat_simul <- matrix(nrow = nsimul, ncol = 1)
    
    for (i in 1:nsimul)
    {
        amostra <- rnorm(k, mu0, sigma0)
        
        mu_hat <- mean(amostra)
        sigma_hat <- sd(amostra)
        
        mu_hat_simul[i] <- mu_hat;
        sigma_hat_simul[i] <- sigma_hat;
    }
    
    hist(mu_hat_simul, breaks = 20, col = "red", main = paste("Mu hat com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
    
    hist(sigma_hat_simul, breaks = 20, col = "green", main = paste("Sigma hat com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
}

#----------------------------------------------------
#--- The End
#----------------------------------------------------

