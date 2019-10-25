
rm(list=ls()); #---- limpa todo o ambiente de variáveis para a execução do R

#install.packages("plyr")

library(plyr);

#---- indique aqui o diretorio de trabalho

setwd("C:\\Users\\Alexandre\\Dropbox\\Novos_cursos\\IDP\\IDP_Introducao_a_Estatistica\\ProgramasR\\Dados_Municipios");

#---- lendo as bases de dados em CSV

dados <- read.csv2("IDH_Brasil_2010.csv", header=T, sep=";", dec=",", encoding="latin1");
codigos_ufs <- read.csv2("codigos_ufs.csv", header=T, sep=";", dec=",", encoding="latin1");
empresas <- read.csv2("CADASTRO_EMPRESAS_2008.csv", header=T, sep=";", dec=".", encoding="latin1")
fiscal <- read.csv2("financas_publicas_2008.csv", header=T, sep=";", dec=".", encoding="latin1")
obitos <- read.csv2("OBITOS_DATASUS.csv", header=T, sep=";", dec=".", encoding ="latin1")

#---- fazendo um join de colunas de duas tabelas

fiscal1 <- fiscal[, !(colnames(fiscal) %in% c("nome_mun", "cod_uf", "uf"))]  #-- excluindo colunas

dados1 <- merge(x = dados, y = codigos_ufs, by.x = "uf", by.y = "uf", all.x = T, all.y = T);
dados2 <- merge(x = dados1, y = empresas, by.x = "codmun", by.y = "codmun", all.x = TRUE, all.y = TRUE)
dados3 <- merge(x = dados2, y = fiscal1, by.x = "codmun", by.y = "cod_mun", all.x = TRUE)
dados4 <- merge(x = dados3, y = obitos, by.x = "codmun", by.y = "codmun", all.x = TRUE)

#--------------------------------------------------------------------------------#
#--- Efetuando regressões lineares 
#--------------------------------------------------------------------------------#

str(dados)
str(dados1)
colnames(dados1)

ob1 <- colnames(dados1)
ob1

par(mfrow = c(2,3));
par(mar = c(4,4,2,2));

modelo1 <- lm(renda_per_capita ~ IDHM_educacao, data = dados)
modelo2 <- lm(log(renda_per_capita) ~ log(IDHM_educacao), data = dados)

summary(modelo1)
summary(modelo2)

pred_modelo1 <- fitted(modelo1)   #--- gerando as previsões de acordo com o modelo 1
pred_modelo2 <- fitted(modelo2)   #--- gerando as previsões de acordo com o modelo 2

#--- criando uma matriz apenas com os dados utilizados nas regressões específicas

X1 <- model.matrix(renda_per_capita ~ IDHM_educacao, data = dados)
X2 <- model.matrix(log(renda_per_capita) ~ log(IDHM_educacao), data = dados)

head(X1)
head(X2)
tail(X2)

#--- criando gráficos para comparar os valores reais e os valores preditos 
#--- da variável Y

plot(dados$IDHM_educacao, dados$renda_per_capita, col = "red", 
     main = "Modelo 1 - Predito versus Observado",
     ylab="Renda per capita (R$)", xlab = "IDHM educação", ylim=c(-200, 2000))
points(dados$IDHM_educacao, pred_modelo1, col="black", lty = 2)

hist(pred_modelo1, col = "red", main = "Histograma Pred Modelo 1", breaks = 30)

plot(dados$renda_per_capita, pred_modelo1, col = "red", 
     main = "Modelo 1 - Predito versus Observado",
     xlab="Renda per capita (R$)", ylab = "Renda per capita (R$) - Predita")

plot(dados$IDHM_educacao, dados$renda_per_capita, col = "blue", 
     main = "Modelo 2 - Predito versus Observado", 
     ylab="Renda per capita (R$)", xlab = "IDHM educação", ylim=c(-200, 2000))
points(dados$IDHM_educacao, exp(pred_modelo2), col="black", lty = 2)

hist(exp(pred_modelo2), col = "blue", main = "Histograma Pred Modelo 2", breaks = 30)

plot(dados$renda_per_capita, exp(pred_modelo2), col = "blue", 
     main = "Modelo 2 - Predito versus Observado",
     xlab="Renda per capita (R$)", ylab = "Renda per capita (R$) - Predita")

#--- correlação entre os valores preditos e os valores reais

cor(pred_modelo1, dados$renda_per_capita)
cor(exp(pred_modelo2), dados$renda_per_capita)

#--- podemos estimar outros modelos 

modelo3 <- lm(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
              + indice_gini, data = dados)
summary(modelo3)

table(dados3$Regiao)

modelo4 <- lm(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
              + indice_gini + as.factor(Regiao), data = dados3)
summary(modelo4)

X3 <- model.matrix(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
                   + indice_gini, data = dados)
head(X3, 10)

X4 <- model.matrix(renda_per_capita ~ esperanca_vida_ao_nascer + IDHM_educacao 
                   + indice_gini + as.factor(Regiao), data = dados3)
head(X4, 10)

#--------------------------------------------------------------------------------#
#--- Efetuando regressões lineares para o Exercício 
#--------------------------------------------------------------------------------#

dados3$perc_pop_rural <- dados3$populacao_rural / dados3$populacao_total

mod1.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
                                     + dados3$indice_gini
                                     + dados3$salario_medio_mensal
                                     + dados3$perc_criancas_extrem_pobres
                                     + dados3$perc_criancas_pobres
                                     + dados3$perc_pessoas_dom_agua_estogo_inadequados
                                     + dados3$perc_pessoas_dom_paredes_inadequadas
                                     + dados3$perc_pop_dom_com_coleta_lixo)
summary(mod1.ex)

mod2.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
              + dados3$indice_gini
              + dados3$salario_medio_mensal
              + dados3$perc_criancas_extrem_pobres
              + dados3$perc_criancas_pobres
              + dados3$perc_pessoas_dom_agua_estogo_inadequados
              + dados3$perc_pessoas_dom_paredes_inadequadas
              + dados3$perc_pop_dom_com_coleta_lixo
              + dados3$perc_pop_rural
              + as.factor(dados3$Regiao))
summary(mod2.ex)

mod3.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
              + dados3$indice_gini
              + dados3$salario_medio_mensal
              + dados3$perc_criancas_extrem_pobres
              + dados3$perc_criancas_pobres
              + dados3$perc_pessoas_dom_agua_estogo_inadequados
              + dados3$perc_pessoas_dom_paredes_inadequadas
              + dados3$perc_pop_dom_com_coleta_lixo
              + dados3$perc_pop_rural
              + as.factor(dados3$Regiao)
              + as.factor(dados3$Regiao)*dados3$renda_per_capita)
summary(mod3.ex)

mod1a.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita
              + I(renda_per_capita^2)
              + dados3$indice_gini
              + dados3$salario_medio_mensal
              + dados3$perc_criancas_extrem_pobres
              + dados3$perc_criancas_pobres
              + dados3$perc_pessoas_dom_agua_estogo_inadequados
              + dados3$perc_pessoas_dom_paredes_inadequadas
              + dados3$perc_pop_dom_com_coleta_lixo, data = dados)
summary(mod1a.ex)

#---- intervalos de confiança para os parâmetros da regressão estimada

confint(mod1.ex)                #--- probabilidade de cobertura de 95%
confint(mod1.ex, level = 0.9)   #--- probabilidade de cobertura de 90%
confint(mod1.ex, level = 0.8)   #--- probabilidade de cobertura de 80%

#---- testando hipóteses para vários parâmetros

mod2.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
              + dados3$indice_gini
              + dados3$salario_medio_mensal
              + dados3$perc_criancas_extrem_pobres
              + dados3$perc_criancas_pobres
              + dados3$perc_pessoas_dom_agua_estogo_inadequados
              + dados3$perc_pessoas_dom_paredes_inadequadas
              + dados3$perc_pop_dom_com_coleta_lixo
              + dados3$perc_pop_rural
              + as.factor(dados3$Regiao))
summary(mod2.ex)

mod2.ex.rest <- lm(dados3$mort_infantil ~ dados3$renda_per_capita 
                   + dados3$indice_gini
                   + dados3$salario_medio_mensal
                   + dados3$perc_criancas_extrem_pobres
                   + dados3$perc_criancas_pobres
                   + dados3$perc_pessoas_dom_agua_estogo_inadequados
                   + dados3$perc_pessoas_dom_paredes_inadequadas
                   + dados3$perc_pop_dom_com_coleta_lixo
                   + dados3$perc_pop_rural)
summary(mod2.ex.rest)

anova(mod2.ex.rest, mod2.ex, test='LRT')

mod1b.ex <- lm(dados3$mort_infantil ~ dados3$renda_per_capita
               + I(renda_per_capita^2)
               + I(renda_per_capita^3)
               + dados3$indice_gini
               + dados3$salario_medio_mensal
               + dados3$perc_criancas_extrem_pobres
               + dados3$perc_criancas_pobres
               + dados3$perc_pessoas_dom_agua_estogo_inadequados
               + dados3$perc_pessoas_dom_paredes_inadequadas
               + dados3$perc_pop_dom_com_coleta_lixo, data = dados)
summary(mod1b.ex)

mod1b.ex.rest <- lm(dados3$mort_infantil ~ dados3$renda_per_capita
                   + dados3$indice_gini
                   + dados3$salario_medio_mensal
                   + dados3$perc_criancas_extrem_pobres
                   + dados3$perc_criancas_pobres
                   + dados3$perc_pessoas_dom_agua_estogo_inadequados
                   + dados3$perc_pessoas_dom_paredes_inadequadas
                   + dados3$perc_pop_dom_com_coleta_lixo, data = dados)
summary(mod1b.ex.rest)

anova(mod1b.ex.rest, mod1b.ex, test='LRT')

#----------------------------------------------------------------------------
#---- The end
#----------------------------------------------------------------------------
