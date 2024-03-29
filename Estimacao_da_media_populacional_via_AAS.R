
rm(list=ls()); #---- limpa todo o ambiente de vari�veis

library(plyr);
library(agricolae);
#library(xlsx);

#---- indique aqui o diretorio de trabalho

setwd("C:\\Users\\Alexandre\\Dropbox\\Novos_cursos\\Introducao_a_Estatistica\\ProgramasR\\Dados_Municipios");
#setwd("E:\\Dropbox\\Novos_cursos\\Introducao_a_Estatistica\\ProgramasR\\Dados_Municipios");

#---- lendo as bases de dados em CSV

dados <- read.csv2("IDH_Brasil_2010.csv", header=T, sep=";", dec=",", encoding="latin1");

names(dados)

#---- vamos fazer um estudo da estimativa da renda percapita da popula��o
#---- com base na renda percapita de uma amostra aleat�ria simples

media_renda_populacao <- mean(dados$renda_per_capita); media_renda_populacao

par(mfrow = c(1,1));
par(mar = c(4,4,2,2));

hist(dados$renda_per_capita, breaks = 30, col = "green", main = "Histograma para a renda percapita", 
     xlab = "Renda Percapita", ylab = "Frequ�ncia")

#---- retirando amostras aleat�rias dos munic�pios (amostra alet�ria simples)

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

#---- simula��es de Monte Carlo para estudar a propriedade das m�dias 
#---- das amostras para estimar a m�dia na popula��o

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
#      main = "Histograma da m�dias nas amostras (n = 10)", 
#      xlab = "m�dias em cada amostra", ylab = "frequ�ncia")

hist(medias_simulacoes1, breaks=30, col = "green", 
     main = "Histograma da m�dias nas amostras (n = 5)", 
     xlab = "m�dias em cada amostra", ylab = "frequ�ncia")

hist(medias_simulacoes2, breaks=30, col = "blue", 
     main = "Histograma da m�dias nas amostras (n = 40)", 
     xlab = "m�dias em cada amostra", ylab = "frequ�ncia")

hist(medias_simulacoes3, breaks=30, col = "red", 
     main = "Histograma da m�dias nas amostras (n = 640)", 
     xlab = "m�dias em cada amostra", ylab = "frequ�ncia")

hist(dados$renda_per_capita, breaks=30, col = "orange", 
     main = "Histograma da renda percapita na popula��o total", 
     xlab = "renda per capita", ylab = "frequ�ncia")

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

plot(n_amostras, means1, col="red", type="o", main = "M�dias das estimativas da amostra",
     xlab = "n�m obs na amostra", ylab = "M�dia das estimativas")
lines(x = n_amostras, y = rep(media_renda_populacao, length(n_amostras)), col = "black")

plot(n_amostras, sds1, col="blue", type="o", main = "Precis�o das estimativas da amostra",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

plot(log(n_amostras), log(sds1), col="blue", type="o", main = "Precis�o das estimativas da amostra",
     xlab = "log n�m obs na amostra", ylab = "log imprecis�o (desv. pad.)")

lm1 <- lm(log(sds1) ~ log(n_amostras))
lm1

#---- o que acontece com a forma da distribui��o das estimativas quando
#---- a amostra aumenta?

par(mfrow = c(2,4));
par(mar = c(4,4,2,2));

hist(dados$renda_per_capita, breaks = 30, col = "green", main = "Histograma para a renda percapita", 
     xlab = "Renda Percapita", ylab = "Frequ�ncia")

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
    
    hist(medias_simulacoes1, breaks = 30, col = "red", main = paste("M�dias amostrais com n =", as.character((k))), 
         xlab = "Valores gerados", ylab = "Frequ�ncia")
}

#------------------------------------------------------------------------------------
#--- � poss�vel estimar a imprecis�o das estimativas a partir de apenas uma amostra
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

plot(n_amostras, sds1, col="red", type="o", main = "M�dia Amostral - Precis�o Simula��es",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

#---- precis�o te�rica

Npopulacao <- nrow(dados)
sd_populacao <- sd(dados$renda_per_capita)

sd_teorico <- (1 - n_amostras / Npopulacao) * (sd_populacao/sqrt(n_amostras))
sd_teorico

plot(n_amostras, sd_teorico, col="blue", type="o", main = "M�dia Amostral - Precis�o Te�rica",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

#---- na pr�tica, temos apenas uma amostras nas m�os para trabalhar
#---- para estimar a precis�o te�rica, precisamos estimar apenas a vari�ncia da popula��o,
#---- dado que n�o temos a vari�ncia populacional - estimamos com a amostra

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

plot(n_amostras, sd_estimado, col="black", type="o", main = "M�dia Amostral - Precis�o Estimada",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

#---- comparando precis�es com diferentes m�todos

plot(n_amostras, sd_estimado, col="black", type="o", main = "MM para Mu - Precis�es Comparadas",
     xlab = "n�m obs na amostra", ylab = "imprecis�o (desv. pad.)")

lines(n_amostras, sd_teorico, col = "blue")
points(n_amostras, sd_teorico, col = "blue")

lines(n_amostras, sds1, col = "red")
points(n_amostras, sds1, col = "red")

comparacao_sds <- data.frame(n_amostras, sd_simulacoes = sds1, sd_teorico, sd_estimado)
View(comparacao_sds)

#--------------------------------------------------------------------------------------
#--- The End
#--------------------------------------------------------------------------------------

