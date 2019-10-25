
#----------------------------------------------------------------------------------------------------------#
#--- Este programa contém uma série de comandos básicos iniciais para familiarização com o software R   ---#
#----------------------------------------------------------------------------------------------------------#

rm(list=ls()); #---- limpa todo o ambiente de variáveis para a execução do R

#install.packages("plyr")
#install.packages("agricolae")
#install.packages("corrplot")
#install.packages("psych")

library(plyr);
library(agricolae);
library(corrplot)
library(psych);

#---- indique aqui o diretorio de trabalho

setwd("C:\\Users\\Alexandre\\Dropbox\\Novos_cursos\\IDP\\IDP_Introducao_a_Estatistica\\ProgramasR\\Dados_Municipios");

#---- lendo as bases de dados em CSV

dados <- read.csv2("IDH_Brasil_2010.csv", header=T, sep=";", dec=",", encoding="latin1");
codigos_ufs <- read.csv2("codigos_ufs.csv", header=T, sep=";", dec=",", encoding="latin1");
empresas <- read.csv2("CADASTRO_EMPRESAS_2008.csv", header=T, sep=";", dec=".", encoding="latin1")
fiscal <- read.csv2("financas_publicas_2008.csv", header=T, sep=";", dec=".", encoding="latin1")
obitos <- read.csv2("OBITOS_DATASUS.csv", header=T, sep=";", dec=".", encoding ="latin1")
sanfrancisco <- read.csv2("CADASTRO_EMPRESAS_SAO_FRANCISCO_2008.csv", header=T, sep=";", dec=".", encoding ="latin1")

#---- listando as variáveis em tabela de dados

names(dados)
names(obitos)
str(dados)

#---- fazendo um sumário das variáveis nas tabelas de dados

summary(dados)
summary(fiscal)

#---- printing as primeiras cinco observações e as cinco últimas observações (ou mais)

head(codigos_ufs)
tail(codigos_ufs)

head(codigos_ufs, n = 10)
tail(codigos_ufs, n = 12)

#---- encontrando decis das variáveis 

qrenda <- quantile(dados$renda_per_capita, c(0.01, 0.02, 0.32, 0.95))
qrenda

qrec_iss <- quantile(fiscal$receitas_orc_realizadas_iss)
qrec_iss

#---- desvio padrão e variância

sd(dados$renda_per_capita)  #--- amostral (divide por n-1)
sum(dados$renda_per_capita)

var(dados$renda_per_capita) #--- amostral

var_pop <- sum((dados$renda_per_capita - mean(dados$renda_per_capita))^2)/length(dados$renda_per_capita)
var_pop

var_amost<- sum((dados$renda_per_capita - mean(dados$renda_per_capita))^2)/(length(dados$renda_per_capita) - 1)
var_amost

#---- coeficiente de assimetria e kurtosis

s <- skewness(dados$renda_per_capita)
s

k <- kurtosis(dados$renda_per_capita) #--- indica o excesso de kurtosis (acima de 3)
k

#---- correlações (diferentes tipos)

cov(dados$renda_per_capita, dados$esperanca_vida_ao_nascer)
cor(dados$renda_per_capita, dados$esperanca_vida_ao_nascer)

cov(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, method="pearson")
cor(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, method="pearson")

cov(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, method="kendall")
cor(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, method="kendall")

cov(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, method="spearman")
cor(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, method="spearman")

?cor

#---- organizando Box-Plots para visualização de variáveis

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

boxplot(dados$renda_per_capita ~ dados$uf, col = "green", 
        main = "Renda per capita por UF", 
        xlab = "Código UF", ylab = "Renda percapita (R$)")

boxplot(dados$esperanca_vida_ao_nascer ~ dados$uf, col = "red", 
        main = "Esperança de vida ao nascer por UF", 
        xlab = "Código UF", ylab="Esperança de vida (anos)")

#--- organizando histogramas

par(mfrow = c(2,2));
par(mar = c(4,4,2,2));

hist(dados$renda_per_capita, breaks = 40, col = "red", 
     main = "Renda per capita municipal", xlab = "Renda per capita (R$)")
rug(dados$renda_per_capita)

hist(dados$taxa_desocupacao_18anosoumais, breaks = 40, col = "blue", 
     main = "Percentual de desocupação", xlab = "Percentual desoc. (%)")

hist(dados$perc_extremamente_pobres, breaks = 40, col = "orange", 
     main = "% Extremamente pobres", xlab = "Ext. pobres (%)")

hist(dados$perc_pop_dom_energia_eletrica, breaks = 40, col = "green", 
     main = "% pess com energia elétrica", xlab = "pess. com energia (%)")
rug(dados$perc_pop_dom_energia_eletrica)

#---- fazendo um gráfico de dispersão entre duas variáveis

par(mfrow = c(1,2));
par(mar = c(4,4,2,2));

plot(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, col = "red", 
     main = "Esperança ao nascer versus renda per capita",
     xlab="Renda per capita (R$)", ylab = "Esperança de vida ao nascer (anos)")

plot(dados$renda_per_capita, dados$esperanca_vida_ao_nascer, col = dados$uf, 
     main = "Esperança ao nascer versus renda per capita",
    xlab="Renda per capita (R$)", ylab = "Esperança de vida ao nascer (anos)")

#---- selecionando apenas algumas variáveis de uma tabela

regioes <- fiscal[,c("cod_mun", "Regiao")]

sanfranc1 <- sanfrancisco[, c("codmun", "nomemun")]
sanfranc2 <- sanfrancisco[, colnames(sanfrancisco) %in% c("codmun", "Regiao")]

#---- adicionando uma variável à tabela de dados

sanfranc1[, c("ind_sf")] <- 1;
sanfranc1$ind_sf1 <- 1;

#---- analisando pares de variáveis em uma tabela

dados_cor <- dados[,c("populacao_ate1ano", "PEA_10a14anos", "populacao_1a3anos", 
                      "IDHM_renda", "IDHM_logenvidade", "IDHM_educacao")]

cov(dados_cor)

cor(dados_cor)
cor(dados_cor, method="spearman")

par(mfrow = c(1,1));
par(mar = c(4,4,2,2));

mat_cor <- cor(dados_cor)
corrplot(mat_cor, method = "circle")

pairs(dados_cor)

pairs.panels(dados_cor)
pairs.panels(dados_cor, stars = TRUE)
?pairs.panels

#---- renomeando variáveis 

dados_cor <- rename(dados_cor, c("populacao_1a3anos" = "pop_1a3anos", 
                                 "populacao_ate1ano" = "pop_ate1ano"));

cor(dados_cor)

#---- excluindo colunas de uma tabela de dados 

fiscal1 <- fiscal[, !(colnames(fiscal) %in% c("nome_mun", "cod_uf", "uf"))]

#---- fazendo um join de colunas de duas tabelas

dados1 <- merge(x = dados, y = codigos_ufs, by.x = "uf", by.y = "uf", all.x = T, all.y = T);
dados2 <- merge(x = dados1, y = empresas, by.x = "codmun", by.y = "codmun", all.x = TRUE, all.y = TRUE)
dados3 <- merge(x = dados2, y = sanfranc1, by.x = "codmun", by.y = "codmun", all.x = TRUE)
dados3 <- merge(x = dados3, y = fiscal1, by.x = "codmun", by.y = "cod_mun", all.x = TRUE)

#---- listando classes de uma coluna 

table(dados3$ind_sf)
table(dados3$uf)
table(dados3$nome_uf)
table(dados3$Regiao)

sum(table(dados3$nome_uf))

#---- mudando valores de uma variável condicionalmente

dados3[,"ind_sf"] <- ifelse(is.na(dados3[,"ind_sf"]), 0, dados3[,"ind_sf"]);

dados3$sigla_uf <- ifelse(dados3$uf == 11, dados3$sigla_uf <- "RO", dados3$sigla_uf <- "Outros");

dados4 <- dados3[, colnames(dados3) %in% c("uf", "sigla_uf")]

table(dados3$ind_sf)

#---- gerando valores agregados por macroregião 

macroregiao <- ddply(dados3, .(Regiao), summarize, media_gini = mean(indice_gini, rm.na = TRUE), 
                                        PEA_18oumaisanos = sum(PEA_18oumaisanos),
                                        receitas_orc_realizadas = sum(receitas_orc_realizadas),
                                        media_renda_percapita = mean(renda_per_capita))

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

boxplot(dados3$renda_per_capita ~ dados3$Regiao, col = "green", 
        main = "Renda per capita por Região", 
        xlab = "Região", ylab = "Renda percapita (R$)")

boxplot(dados3$esperanca_vida_ao_nascer ~ dados3$Regiao, col = "red", 
        main = "Esperança de vida ao nascer por Região", 
        xlab = "Região", ylab="Esperança de vida (anos)")

#---- gerando valores agregados por microregião

microregiao <- ddply(dados3, .(Regiao, cod_microregiao, nome_microregiao, cod_mesoregiao, nome_mesoregiao), summarize, 
                     media_gini = mean(indice_gini, rm.na = TRUE), 
                     PEA_18oumaisanos = sum(PEA_18oumaisanos),
                     receitas_orc_realizadas = sum(receitas_orc_realizadas),
                     media_renda_percapita = mean(renda_per_capita))

#---- salvando resultados de tabelas para usar em outros programas

write.table(macroregiao, "resultados_por_macroregiao.csv", sep=";", 
            row.names=FALSE, col.names=TRUE, fileEncoding="latin1", dec=",");

write.table(microregiao, "resultados_por_microregiao.csv", sep=";", 
            row.names=FALSE, col.names=TRUE, fileEncoding="latin1", dec=",");

#---- selecionando um subconjunto de municipios

dados3_sf <- dados3[dados3$ind_sf == 1,]
dados3_sf_ba <- dados3[dados3$ind_sf == 1 & dados3$nomeuf == "Bahia",]

dados3_sf1 <- merge(x = dados3, y = sanfranc1, by.x = "codmun", by.y = "codmun", all.x = FALSE)

#---- quebrando variáveis em decis (ou cortes mais gerais)

dados4 <- within(dados3, cod_grupos_renda_percapita <- as.integer(cut(renda_per_capita, 
                                                                      quantile(renda_per_capita, probs=0:10/10), 
                                                                      include.lowest=TRUE)))

dados4$grupos_renda_percapita <- paste("Decil_renda_", as.character(dados4$cod_grupos_renda_percapita), sep="")

dados4$cod_regiao = floor(dados4$uf/10) #--- variável para ser usada depois

table(dados4$grupos_renda_percapita)

#---- ordenando um data.frame de acordo uma ou mais variáveis

dados5 <- dados4[order(dados4$cod_grupos_renda_percapita),]
dados6 <- dados4[order(-dados4$uf, dados4$cod_grupos_renda_percapita),]

write.table(dados5, "dados_ordenados_decis_renda.csv", sep=";", 
            row.names=FALSE, col.names=TRUE, fileEncoding="latin1", dec=",");

write.table(dados6, "dados_ordenados_uf_decis_renda.csv", sep=";", 
            row.names=FALSE, col.names=TRUE, fileEncoding="latin1", dec=",");

#---- apresentando os dados por decis

par(mfrow = c(2,1));
par(mar = c(4,4,2,2));

boxplot(dados4$perc_pop_dom_energia_eletrica ~ dados4$cod_grupos_renda_percapita, col = "green", 
        main = "Acesso a energia por decil de renda", 
        xlab = "Decil de renda", ylab = "Proporção de acesso (%)")

boxplot(dados4$esperanca_vida_ao_nascer ~ dados4$cod_grupos_renda_percapita, col = "red", 
        main = "Esperança de vida por decil de renda", 
        xlab = "Decil de renda", ylab="Esperança de vida (anos)")

#--------------------------------------------------------------------------------#
#--- Efetuando regressões lineares 
#--------------------------------------------------------------------------------#

str(dados)

par(mfrow = c(2,3));
par(mar = c(4,4,2,2));

modelo1 <- lm(renda_per_capita ~ IDHM_educacao, data = dados)
modelo2 <- lm(log(renda_per_capita) ~ log(IDHM_educacao), data = dados)

summary(modelo1)
summary(modelo2)

pred_modelo1 <- fitted(modelo1)
pred_modelo2 <- fitted(modelo2)

X1 <- model.matrix(renda_per_capita ~ IDHM_educacao, data = dados)
X2 <- model.matrix(log(renda_per_capita) ~ log(IDHM_educacao), data = dados)

head(X1)
head(X2)

plot(dados$IDHM_educacao, dados$renda_per_capita, col = "red", 
     main = "Modelo 1 - Predito versus Observado",
     ylab="Renda per capita (R$)", xlab = "IDHM educação", ylim=c(-200, 2000))
points(dados$IDHM_educacao, pred_modelo1, col="black", lty = 2)

hist(pred_modelo1, col = "red", main = "Histograma Pred Modelo 1")

plot(dados$renda_per_capita, pred_modelo1, col = "red", 
     main = "Modelo 1 - Predito versus Observado",
     xlab="Renda per capita (R$)", ylab = "Renda per capita (R$) - Predita")

plot(dados$IDHM_educacao, dados$renda_per_capita, col = "blue", 
     main = "Modelo 2 - Predito versus Observado", 
     ylab="Renda per capita (R$)", xlab = "IDHM educação", ylim=c(-200, 2000))
points(dados$IDHM_educacao, exp(pred_modelo2), col="black", lty = 2)

hist(exp(pred_modelo2), col = "blue", main = "Histograma Pred Modelo 2")

plot(dados$renda_per_capita, exp(pred_modelo2), col = "blue", 
     main = "Modelo 2 - Predito versus Observado",
     xlab="Renda per capita (R$)", ylab = "Renda per capita (R$) - Predita")

cor(pred_modelo1, dados$renda_per_capita)
cor(exp(pred_modelo2), dados$renda_per_capita)

modelo3 <- lm(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
                                 + indice_gini, data = dados)
summary(modelo3)

modelo4 <- lm(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
              + indice_gini + as.factor(Regiao), data = dados3)
summary(modelo4)

X3 <- model.matrix(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
                                      + indice_gini, data = dados)
head(X3, 10)

X4 <- model.matrix(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
                                      + indice_gini + as.factor(Regiao), data = dados3)
head(X4, 10)

#----------------------------------------------------------------------------------------------------------#
#---------------------------- THE END ---------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------#

