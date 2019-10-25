
rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(agricolae);

#-----------------------------------------------
#---- amostra aleatória simples 
#-----------------------------------------------

setwd("C:\\Users\\Alexandre\\Dropbox\\Novos_cursos\\ENAP_Introducao_a_Inferencia\\ProgramasR\\Dados_Auxiliares");
dados <- read.csv2("Populacao_escolaridade_para_simulacoes.csv", header=T, sep=";", dec=".", encoding="latin1");

media_populacional <- mean(dados$Anos_estudos); media_populacional
N <- nrow(dados); N

par(mfrow = c(1,2));
par(mar = c(4,4,2,2));

hist(dados$Anos_estudos,
     main = "Distribuição de escolaridade na população", xlab = "Anos estudo", ylab = "Frequência", col = "red")

#-- selecionando amostras aleatórias --#

namostra <- 82;

obs_amostra1 <- sample(nrow(dados), namostra, replace = FALSE); obs_amostra1
obs_amostra2 <- sample(nrow(dados), namostra, replace = FALSE); obs_amostra2
obs_amostra3 <- sample(nrow(dados), namostra, replace = FALSE); obs_amostra3

amostra1 <- dados[obs_amostra1,]
amostra2 <- dados[obs_amostra2,]
amostra3 <- dados[obs_amostra3,]

media_amostra1 <- mean(amostra1$Anos_estudos); media_amostra1
media_amostra2 <- mean(amostra2$Anos_estudos); media_amostra2
media_amostra3 <- mean(amostra3$Anos_estudos); media_amostra3

#----- simulações de Monte Carlo para estudar as propriedades da média amostral

nsimul <- 50000;

media_amost_simul <- matrix(nrow = nsimul, ncol=1)

for (i in 1:nsimul)
{
    obs_amostra1 <- sample(nrow(dados), namostra, replace = FALSE);
    amostra1 <- dados[obs_amostra1,]
    media_amostra <- mean(amostra1$Anos_estudos);
    media_amost_simul[i] <- media_amostra;
}

hist(media_amost_simul, breaks = 30, col = "blue", main = "Histograma das Médias Amostrais", 
     xlab = "Médias amostrais", ylab = "Frequência")

#----- quão comuns sao médias amostrais iguais ou maiores a 6.12? 

ind_maior_que_corte <- (media_amost_simul >= 6.12) #--- indica qual observações são maiores do que 6.12

head(media_amost_simul)
head(ind_maior_que_corte)

prob_maior_que_corte <- sum(ind_maior_que_corte) / nsimul; prob_maior_que_corte

#--------------------------------------------------
#----- valores críticos teste uni-caudal
#--------------------------------------------------

c1 <- qnorm(0.99); c1

#----- exemplo de teste de hipóteses (exemplo 1)

X <- c(5.3, 6.1, 7.2, 3.3, 5.2, 8.1, 5.3, 8.2, 5.1, 9.4, 8.3, 4.1, 5.3, 10.2, 4.5)

length(X)
mean(X)
sd(X)

tteste <- (mean(X)-6)/(sd(X)/sqrt(length(X))); tteste

#----- exemplo de teste de hipóteses (exemplo 2)

X <- c(7.3, 6.1, 7.2, 6.3, 5.2, 8.1, 11.3, 8.2, 15.1, 9.4, 8.3, 4.1, 5.3, 10.6, 6.5)

length(X)
mean(X)
sd(X)

tteste <- (mean(X)-6)/(sd(X)/sqrt(length(X))); tteste

#----- usando o R (função t.test)

amostra1 <- c(5.3, 6.1, 7.2, 3.3, 5.2, 8.1, 5.3, 8.2, 5.1, 9.4, 8.3, 4.1, 5.3, 10.2, 4.5)
t.test(amostra1, mu = 6, alternative = "greater")

amostra2 <- c(7.3, 6.1, 7.2, 6.3, 5.2, 8.1, 11.3, 8.2, 15.1, 9.4, 8.3, 4.1, 5.3, 10.6, 6.5)
t.test(amostra2, mu = 6, alternative = "greater")

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

