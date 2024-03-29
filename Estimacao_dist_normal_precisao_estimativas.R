
rm(list=ls()); #---- limpa todo o ambiente de vari�veis

library(plyr);
library(agricolae);
library(VGAM);

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

#------------------------------------------------------------------------------------
#--- � poss�vel estimar a imprecis�o das estimativas a partir de apenas uma amostra
#------------------------------------------------------------------------------------

mu0 <- 2.0
sigma0 <- 1.4

nsimul <- 20000

n_amostras <- c(10, 20, 50, 100, 200, 400, 1000, 2000)
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

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

plot(n_amostras, sds1, col="red", type="o", main = "MM para Mu - Precis�o Simula��es",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(n_amostras, sds2, col="blue", type="o", main = "MM para Sigma - Precis�o Simula��es",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

#---- vamos focar na imprecis�o para a estimativa para a m�dia mu

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, sds1, col="red", type="o", main = "MM para Mu - Precis�o Simula��es",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

sd_teorico_mu <- sigma/sqrt(n_amostras)
sd_teorico_mu

plot(n_amostras, sd_teorico_mu, col="blue", type="o", main = "MM para Mu - Precis�o Te�rica",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

sds1

#---- na pr�tica, temos apenas uma amostras nas m�os para trabalhar
#---- para estimar a precis�o te�rica, precisamos estimar apenas a vari�ncia da popula��o,
#---- dado que n�o temos a vari�ncia populacional - estimamos com a amostra

obs <- 0;
sd_estimado_mu <- matrix(nrow = length(n_amostras), ncol = 1)
    
for (k in n_amostras)
{
    obs <- obs+1

    amostra <- rnorm(k, mu0, sigma0)

    sigma_hat <- sd(amostra)
    
    nobs_amostra <- k;
    
    print(nobs_amostra)
    print(sigma_hat)
    
    sd_estimado_mu[obs] <- sigma_hat / sqrt(nobs_amostra);
}

sd_estimado_mu

plot(n_amostras, sd_estimado_mu, col="black", type="o", main = "MM para Mu - Precis�o Estimada",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

#---- comparando precis�es com diferentes m�todos

plot(n_amostras, sd_estimado_mu, col="black", type="o", main = "MM para Mu - Precis�es Comparadas",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

lines(n_amostras, sd_teorico_mu, col = "blue")
points(n_amostras, sd_teorico_mu, col = "blue")

lines(n_amostras, sds1, col = "red")
points(n_amostras, sds1, col = "red")

comparacao_sds <- data.frame(n_amostras, sd_simulacoes_mu = sds1, sd_teorico_mu, sd_estimado_mu)
View(comparacao_sds)

#--------------------------------------------------------------------------------------
#--- The End
#--------------------------------------------------------------------------------------

