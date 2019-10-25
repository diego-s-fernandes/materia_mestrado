
rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(agricolae);
#library(xlsx);

#---- indique aqui o diretorio de trabalho

setwd("C:\\Users\\Alexandre\\Dropbox\\Novos_cursos\\Introducao_a_Estatistica\\ProgramasR\\Dados_Municipios");
#setwd("E:\\Dropbox\\Novos_cursos\\Introducao_a_Estatistica\\ProgramasR\\Dados_Municipios");

#---- lendo as bases de dados em CSV

dados <- read.csv2("IDH_Brasil_2010.csv", header=T, sep=";", dec=",", encoding="latin1");

names(dados)

#---- vamos fazer um estudo da estimativa da renda percapita da população
#---- com base na renda percapita de uma amostra aleatória simples

media_renda_populacao <- mean(dados$renda_per_capita); media_renda_populacao

par(mfrow = c(1,1));
par(mar = c(4,4,2,2));

hist(dados$renda_per_capita, breaks = 30, col = "green", main = "Histograma para a renda percapita", 
     xlab = "Renda Percapita", ylab = "Frequência")

#---- retirando amostras aleatórias dos municípios (amostra aletória simples)

namostra <- 20;

obs_amostra1 <- sample(nrow(dados), namostra, replace = FALSE); obs_amostra1
obs_amostra2 <- sample(nrow(dados), namostra, replace = FALSE); obs_amostra2
obs_amostra3 <- sample(nrow(dados), namostra, replace = FALSE); obs_amostra3

amostra1 <- dados[obs_amostra1, c("codmun", "renda_per_capita")]
amostra2 <- dados[obs_amostra2, c("codmun", "renda_per_capita")]
amostra3 <- dados[obs_amostra3, c("codmun", "renda_per_capita")]

media_amostra1 <- mean(amostra1$renda_per_capita); media_amostra1
media_amostra2 <- mean(amostra2$renda_per_capita); media_amostra2
media_amostra3 <- mean(amostra3$renda_per_capita); media_amostra3

#---- simulações de Monte Carlo para estudar a propriedade das médias 
#---- das amostras para estimar a média na população

namostra1 <- 5;
#namostra1 <- 10;
namostra2 <- 40;
namostra3 <- 640;

nsimulacoes <- 10000;
medias_simulacoes1 <- matrix(nrow=nsimulacoes, ncol=1)
medias_simulacoes2 <- matrix(nrow=nsimulacoes, ncol=1)
medias_simulacoes3 <- matrix(nrow=nsimulacoes, ncol=1)

for (iter in 1:nsimulacoes)
{
    obs_amostra1 <- sample(nrow(dados), namostra1, replace = FALSE);
    obs_amostra2 <- sample(nrow(dados), namostra2, replace = FALSE);
    obs_amostra3 <- sample(nrow(dados), namostra3, replace = FALSE);    
    
    amostra1 <- dados[obs_amostra1,"renda_per_capita"]
    amostra2 <- dados[obs_amostra2,"renda_per_capita"]
    amostra3 <- dados[obs_amostra3,"renda_per_capita"]
    
    medias_simulacoes1[iter,1] = mean(amostra1)
    medias_simulacoes2[iter,1] = mean(amostra2)
    medias_simulacoes3[iter,1] = mean(amostra3)
}

head(medias_simulacoes1, 15)

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

# hist(medias_simulacoes1, breaks=30, col = "green", 
#      main = "Histograma da médias nas amostras (n = 10)", 
#      xlab = "médias em cada amostra", ylab = "frequência")

hist(medias_simulacoes1, breaks=30, col = "green", 
     main = "Histograma da médias nas amostras (n = 5)", 
     xlab = "médias em cada amostra", ylab = "frequência")

hist(medias_simulacoes2, breaks=30, col = "blue", 
     main = "Histograma da médias nas amostras (n = 40)", 
     xlab = "médias em cada amostra", ylab = "frequência")

hist(medias_simulacoes3, breaks=30, col = "red", 
     main = "Histograma da médias nas amostras (n = 640)", 
     xlab = "médias em cada amostra", ylab = "frequência")

hist(dados$renda_per_capita, breaks=30, col = "orange", 
     main = "Histograma da renda percapita na população total", 
     xlab = "renda per capita", ylab = "frequência")

media_renda_populacao
mean(medias_simulacoes1); sd(medias_simulacoes1)
mean(medias_simulacoes2); sd(medias_simulacoes2)
mean(medias_simulacoes3); sd(medias_simulacoes3)

#-------------------------------------------------------
#---- efeito do tamanho da amostra nas estimativas
#-------------------------------------------------------

n_amostras <- c(5, 10, 20, 100, 200, 400, 1000, 2000)

means1 <- matrix(nrow = length(n_amostras), ncol = 1);
sds1 <- matrix(nrow = length(n_amostras), ncol = 1);

obs <- 0

for (k in n_amostras)
{
    obs <- obs+1
    
    medias_simulacoes1 <- matrix(nrow=nsimulacoes, ncol=1)
    
    for (iter in 1:nsimulacoes)
    {
        obs_amostra1 <- sample(nrow(dados), k, replace = FALSE);    
        
        amostra1 <- dados[obs_amostra1,"renda_per_capita"]
        
        medias_simulacoes1[iter,1] = mean(amostra1)
    }
    
    means1[obs] <- mean(medias_simulacoes1)
    sds1[obs] <- sd(medias_simulacoes1)
}

par(mfrow = c(3,1));
par(mar = c(4,4,2,2));

plot(n_amostras, means1, col="red", type="o", main = "Médias das estimativas da amostra",
     xlab = "núm obs na amostra", ylab = "Média das estimativas")
lines(x = n_amostras, y = rep(media_renda_populacao, length(n_amostras)), col = "black")

plot(n_amostras, sds1, col="blue", type="o", main = "Precisão das estimativas da amostra",
     xlab = "núm obs na amostra", ylab = "imprecisão (desv. pad.)")

plot(log(n_amostras), log(sds1), col="blue", type="o", main = "Precisão das estimativas da amostra",
     xlab = "log núm obs na amostra", ylab = "log imprecisão (desv. pad.)")

lm1 <- lm(log(sds1) ~ log(n_amostras))
lm1

#---- o que acontece com a forma da distribuição das estimativas quando
#---- a amostra aumenta?

par(mfrow = c(2,4));
par(mar = c(4,4,2,2));

hist(dados$renda_per_capita, breaks = 30, col = "green", main = "Histograma para a renda percapita", 
     xlab = "Renda Percapita", ylab = "Frequência")

n_amostras <- c(5, 10, 40, 100, 500, 1000, 2000)

for (k in n_amostras)
{
    medias_simulacoes1 <- matrix(nrow=nsimulacoes, ncol=1)
    
    for (iter in 1:nsimulacoes)
    {
        obs_amostra1 <- sample(nrow(dados), k, replace = FALSE);    
        
        amostra1 <- dados[obs_amostra1,"renda_per_capita"]
        
        medias_simulacoes1[iter,1] = mean(amostra1)
    }
    
    hist(medias_simulacoes1, breaks = 30, col = "red", main = paste("Médias amostrais com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequência")
}

#------------------------------------------------------------------------------------
#--- é possível estimar a imprecisão das estimativas a partir de apenas uma amostra
#------------------------------------------------------------------------------------

n_amostras <- c(5, 10, 20, 100, 200, 400, 1000, 2000)

means1 <- matrix(nrow = length(n_amostras), ncol = 1);
sds1 <- matrix(nrow = length(n_amostras), ncol = 1);

obs <- 0

for (k in n_amostras)
{
    obs <- obs+1
    
    medias_simulacoes1 <- matrix(nrow=nsimulacoes, ncol=1)
    
    for (iter in 1:nsimulacoes)
    {
        obs_amostra1 <- sample(nrow(dados), k, replace = FALSE);    
        
        amostra1 <- dados[obs_amostra1,"renda_per_capita"]
        
        medias_simulacoes1[iter,1] = mean(amostra1)
    }
    
    means1[obs] <- mean(medias_simulacoes1)
    sds1[obs] <- sd(medias_simulacoes1)
}

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

plot(n_amostras, sds1, col="red", type="o", main = "Média Amostral - Precisão Simulações",
     xlab = "núm obs na amostra", ylab = "imprecisão (desv. pad.)")

#---- precisão teórica

Npopulacao <- nrow(dados)
sd_populacao <- sd(dados$renda_per_capita)

sd_teorico <- (1 - n_amostras / Npopulacao) * (sd_populacao/sqrt(n_amostras))
sd_teorico

plot(n_amostras, sd_teorico, col="blue", type="o", main = "Média Amostral - Precisão Teórica",
     xlab = "núm obs na amostra", ylab = "imprecisão (desv. pad.)")

#---- na prática, temos apenas uma amostras nas mãos para trabalhar
#---- para estimar a precisão teórica, precisamos estimar apenas a variância da população,
#---- dado que não temos a variância populacional - estimamos com a amostra

obs <- 0;
sd_estimado <- matrix(nrow = length(n_amostras), ncol = 1)

for (k in n_amostras)
{
    obs <- obs+1
    
    obs_amostra1 <- sample(nrow(dados), k, replace = FALSE);    
    
    amostra1 <- dados[obs_amostra1,"renda_per_capita"]
    
    sigma_hat <- sd(amostra1)
    
    nobs_amostra <- k;
    
    print(nobs_amostra)
    print(sigma_hat)
    
    sd_estimado[obs] <- (1 - nobs_amostra / Npopulacao) * sigma_hat / sqrt(nobs_amostra);
}

sd_estimado

plot(n_amostras, sd_estimado, col="black", type="o", main = "Média Amostral - Precisão Estimada",
     xlab = "núm obs na amostra", ylab = "imprecisão (desv. pad.)")

#---- comparando precisões com diferentes métodos

plot(n_amostras, sd_estimado, col="black", type="o", main = "MM para Mu - Precisões Comparadas",
     xlab = "núm obs na amostra", ylab = "imprecisão (desv. pad.)")

lines(n_amostras, sd_teorico, col = "blue")
points(n_amostras, sd_teorico, col = "blue")

lines(n_amostras, sds1, col = "red")
points(n_amostras, sds1, col = "red")

comparacao_sds <- data.frame(n_amostras, sd_simulacoes = sds1, sd_teorico, sd_estimado)
View(comparacao_sds)

#--------------------------------------------------------------------------------------
#--- The End
#--------------------------------------------------------------------------------------

