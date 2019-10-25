###### Aula 4: Métodos avançados para manipulação de bases de dados. ######

rm(list=ls()); #---- limpa todo o ambiente de variáveis

#Define o Working Directory:
setwd("C:\\Users\\Alexandre\\Dropbox\\Novos_cursos\\IntroducaoComputacaoCientifica\\Dados\\POF 2008")

##Exemplo 1: Invoca a função LeBasesPosicaoFixa.R
source("LeBasesPosicaoFixa.R")

### Cria um novo arquivo somene com as informações necessárias
# Seleciona tudo: 
fselpr<-function(x) x

#Seleciona somente UF 53: fselpr <- function(x) x[substring(x,3,4)==53]

rcsel.pfix(file.inp="T_MORADOR_S.txt", file.out="MORADOR.txt",
           first=c(3,5,8,9,11,12,60,112),
           last=c(4,7,8,10,11,13,62,127),
           fselpr)

###Lê os dados do arquivo de interesse
dados<-read.table("MORADOR.txt")

###Deleta o arquivo MORADOR.txt
file.remove("MORADOR.txt")

#Coloca os nomes das variáveis
colnames(dados)<-c("COD_UF","NUM_SEQ","NUM_DV","COD_DOMC","NUM_UC","NUM_INFORMANTE",
                  "IDADE_ANOS","RENDA_BRUTA_MONETARIA")

#Coloca os labels nas variáveis
library(Hmisc)
label(dados$COD_UF)<-'CÓDIGO DA UF'
label(dados$NUM_SEQ)<-'NÚMERO SEQUENCIAL'
label(dados$NUM_DV)<-'DV DO SEQUENCIAL'
label(dados$COD_DOMC)<-'NÚMERO DO DOMICÍLIO'
label(dados$NUM_UC)<-'NÚMERO DA UC'
label(dados$NUM_INFORMANTE)<-'NÚMERO DO INFORMANTE'
label(dados$IDADE_ANOS)<-'IDADE CALCULADA EM ANOS'
label(dados$RENDA_BRUTA_MONETARIA)<-'RENDA MONETÁRIA MENSAL DA UC'
describe(dados)

##Exercício : Lê os dados do arquivo T_DESPESA_INDIVIDUAL_S.txt
rcsel.pfix(file.inp="T_DESPESA_INDIVIDUAL_S.txt", file.out="DESPESA.txt",
           first=c(3,5,8,9,11,12,44,46,53),
           last=c(4,7,8,10,11,13,45,50,63), fselpr)

###Lê os dados do arquivo de interesse
desp<-read.table("DESPESA.txt")

###Deleta o arquivo MORADOR.txt
file.remove("DESPESA.txt")

#Coloca os nomes das variáveis
colnames(desp)<-c("COD_UF","NUM_SEQ","NUM_DV","COD_DOMC","NUM_UC",
"NUM_INF","NUM_QUADRO","COD_ITEM","VAL_DESPESA")

#Coloca os labels nas variáveis
label(desp$COD_UF)<-'CÓDIGO DA UF'
label(desp$NUM_SEQ)<-'NÚMERO SEQUENCIAL'
label(desp$NUM_DV)<-'DV DO SEQUENCIAL'
label(desp$COD_DOMC)<-'NÚMERO DO DOMICÍLIO'
label(desp$NUM_UC)<-'NÚMERO DA UC'
label(desp$NUM_INF)<-'NÚMERO DO INFORMANTE'
label(desp$NUM_QUADRO)<-'NÚMERO DO QUADRO'
label(desp$COD_ITEM)<-'CÓDIGO DO ITEM'
label(desp$VAL_DESPESA)<-'VALOR DA DESPESA / AQUISIÇÃO'

#Mostrar até 8 casas decimais 
options("scipen" = 8) 

##Exercício :Faz o merge entre as bases
#Renomeia a variável NUM_INFORMANTE
colnames(dados)[6]<-"NUM_INF"

#Faz o merge
tudo<-merge(dados,desp,by=c("COD_UF","NUM_SEQ","NUM_DV","COD_DOMC",
                            "NUM_UC","NUM_INF"),all=TRUE)

#Obtêm a base somente com os itens 101 e 102 do quadro 28
ingresso<-tudo[which(tudo$NUM_QUADRO==28&
                       tudo$COD_ITEM%in%c(101,201)),]

##Exemplo 2: Invoca o pacote dplyr e faz o filtro
library(dplyr)
tudo53<-filter(tudo,COD_UF==53)

##Exemplo 3: Renomeia a variável
tudo53<-rename(tudo53,INGRESSO=VAL_DESPESA)
head(tudo53)

##Exemplo 4: Elimina as linhas repetidas
tudo.simples<-distinct(select(tudo53, COD_UF,NUM_SEQ,
                              NUM_DV,COD_DOMC,NUM_UC,
                              NUM_INF,IDADE_ANOS))

##Exemplo 5: Cria as novas variáveis
tudo53<-mutate(tudo53,
                  Indice = (INGRESSO-min(INGRESSO, na.rm = T))/
                           (max(INGRESSO, na.rm = T)-min(INGRESSO, na.rm = T)),
                  Z = (INGRESSO-mean(INGRESSO, na.rm = T))/sd(INGRESSO, na.rm = T)
               )


##Exemplo 6: Acha algumas medidas de interesse
estat<-summarise(tudo53,
                 Ingresso.m = mean(INGRESSO, na.rm = TRUE),
                 Ingresso.sd = sd(INGRESSO, na.rm = TRUE),
                 Idade = mean(IDADE_ANOS, na.rm = TRUE),
                 Idade.sd = sd(IDADE_ANOS, na.rm = TRUE)
)
estat

#Exemplo 7:Usando a função group_by(.)
by_tudo <- group_by(tudo, COD_UF)

#Faz as estatísticas por UF
estat.UF <- summarise(by_tudo,
                   count = n(),
                   Ingresso.m = mean(VAL_DESPESA, na.rm = TRUE),
                   Ingresso.sd = sd(VAL_DESPESA, na.rm = TRUE),
                   Idade = mean(IDADE_ANOS, na.rm = TRUE),
                   Idade.sd = sd(IDADE_ANOS, na.rm = TRUE))

#Calcula as estatísticas por grupo
tudo.estat<-mutate(by_tudo,
                  Indice = (VAL_DESPESA-min(VAL_DESPESA, na.rm = T))/
                           (max(VAL_DESPESA, na.rm = T)-min(VAL_DESPESA, na.rm = T)),
                  Z = (VAL_DESPESA-mean(VAL_DESPESA, na.rm = T))/
                       sd(VAL_DESPESA, na.rm = T)
)

#Exemplo 8: Importando os dados do Excel - nem sempre funciona
setwd("C:\\Users\\Alexandre\\Dropbox\\Novos_cursos\\IntroducaoComputacaoCientifica\\Dados")

library(XLConnect)
wb <- loadWorkbook("USEletric.xls")
df <- readWorksheet(wb, sheet = "Data", header = TRUE)





