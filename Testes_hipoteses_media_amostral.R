
rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(agricolae);
library(VGAM);

#-----------------------------------------------
#---- variável aleatória gamma
#-----------------------------------------------

alpha <- 0.4
beta <- 7.5

media_pop <- alpha*beta; media_pop
var_pop <- alpha*(beta^2); var_pop

amostra <- rgamma(500000, alpha, 1/beta)

mean(amostra)
var(amostra)

#----- média amostral para estimar o tempo média de duração das quedas de energia

n <- 82

amostra_aneel <- rgamma(n, alpha, 1/beta)
amostra_aneel

hist(amostra_aneel, col = 'red', main = "Duração das quedas de energia", xlab = "horas", ylab = "Frequência")

media_amostra <- mean(amostra_aneel); media_amostra

#----- simulações de Monte Carlo para estudar as propriedades da média amostral

nsimul <- 50000;

media_amost_simul <- matrix(nrow = nsimul, ncol=1)

for (i in 1:nsimul)
{
    amostra_aneel <- rgamma(n, alpha, 1/beta)
    
    media_amostra <- mean(amostra_aneel);
    
    media_amost_simul[i] <- media_amostra;
}

par(mfrow = c(1,1));
par(mar = c(4,4,2,2));

hist(media_amost_simul, breaks = 30, col = "blue", main = "Histograma das Médias Amostrais", 
     xlab = "Médias amostrais", ylab = "Frequência")

#----- quão comuns sao médias amostrais iguais ou maiores a 3.21? 

ind_maior_que_corte <- (media_amost_simul >= 3.21) #--- indica qual observações são maiores do que 3.21

head(media_amost_simul)
head(ind_maior_que_corte)

prob_maior_que_corte <- sum(ind_maior_que_corte) / nsimul; prob_maior_que_corte

#----- usando a aproximação normal para calcular a probabilidade da cauda

prob_cauda <- 1 - pnorm(3.21, 3, sqrt(22.5/82)); prob_cauda

#----- valores críticos teste uni-caudal

c1 <- qnorm(0.99); c1

#----- exemplo de teste de hipóteses (exemplo 1)

X <- c(2.3, 3.1, 4.2, 0.3, 2.2, 5.1, 2.3, 5.2, 2.1, 6.4, 5.3, 1.1, 2.3, 7.2, 1.5)

length(X)
mean(X)
sd(X)

tteste <- (mean(X)-3)/(sd(X)/sqrt(length(X))); tteste

#----- exemplo de teste de hipóteses (exemplo 2)

X <- c(4.3, 3.1, 4.2, 3.3, 2.2, 5.1, 8.3, 5.2, 12.1, 6.4, 5.3, 1.1, 2.3, 7.6, 3.5)

length(X)
mean(X)
sd(X)

tteste <- (mean(X)-3)/(sd(X)/sqrt(length(X))); tteste

#----- usando o R (função t.test)

amostra1 <- c(2.3, 3.1, 4.2, 0.3, 2.2, 5.1, 2.3, 5.2, 2.1, 6.4, 5.3, 1.1, 2.3, 7.2, 1.5)
t.test(amostra1, mu = 3, alternative = "greater")

amostra2 <- c(4.3, 3.1, 4.2, 3.3, 2.2, 5.1, 8.3, 5.2, 12.1, 6.4, 5.3, 1.1, 2.3, 7.6, 3.5)
t.test(amostra2, mu = 3, alternative = "greater")

amostra3 = c(508.8, 500.0, 491.2, 509.5, 521.2, 489.3, 489.0, 515.5, 486.1, 493.5, 493.6,
             497.3, 500.0, 496.6, 499.3, 499.9, 496.4, 482.4, 498.0, 496.3, 501.8, 498.6);
t.test(amostra3, mu = 500, alternative = "less")

mean(amostra3); sd(amostra3)
(mean(amostra3)-500)/(sd(amostra3)/sqrt(length(amostra3)))

qnorm(0.975)
qt(0.975, df=24)

amostra4 <- c(506.7, 505.5, 504.2, 505.9, 495.8, 508.4, 502.6, 510.1, 491.5, 498.0, 507.0, 515.3, 502.4,     	
              509.2, 519.2, 508.4, 496.7, 503.3, 502.4, 499.8, 503.8, 503.9, 501.5, 497.3, 501.9)
t.test(amostra4, mu = 500, alternative = "two.sided")

mean(amostra4); sd(amostra4)
(mean(amostra4)-500)/(sd(amostra4)/sqrt(length(amostra4)))

#----- teste de hipóteses com duas amostras pareadas

Y = c(11.2, 10.3, 9.7, 5.6, 13.3, 8.3, 9.4, 6.3, 8.4, 11.6, 12.1, 7.8, 9.3, 12.9, 10.6)
length(Y)
     
X = c(7.6, 11.1, 6.4, 4.5, 6.3, 7.9, 11.3, 6.4, 9.1, 12.7, 5.4, 6.1, 8.3, 4.3, 6.1)
length(X)

media <- mean(X) - mean(Y); media
sddif <- sd(X-Y);sddif

testat <- (media - 0)/(sddif/sqrt(length(Y))); testat

t.test(x = X, y = Y, conf.level = 0.95, paired = TRUE)

#----- teste de hipóteses com duas amostras independentes

Y = c(998.59, 1002.97, 780.48, 714.87, 968.01, 651.46, 701.09, 865.96, 
      947.31, 840.91, 1144.25, 724.93, 890.38, 901.48, 730.38, 1097.44)
length(Y)

X = c(1177.92, 1006.40, 1184.42, 850.41, 1636.79, 1807.31, 738.28, 1356.64, 975.25, 1150.08,   
      1039.39, 1351.76, 1227.46, 492.71, 722.45, 915.05, 693.47, 1539.14, 988.98)
length(X)

mediaX <- mean(X); mediaX
mediaY <- mean(Y); mediaY

t.test(x = X, y = Y, paired = FALSE, alternative = "greater", var.equal = FALSE)

t.test(x = X, y = Y, paired = FALSE, alternative = "greater", var.equal = TRUE)

#----------------------------------------------------
#--- The End
#----------------------------------------------------

