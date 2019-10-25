---
  title: "Estatistica"
author: "Diego dos Santos Fernandes e Humberto Nunes Alencar"
output:
  word_document: default
pdf_document: default
html_document: default
---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)
library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(timeSeries)
library(xts)
library(qcc)
library(gridExtra)
library(RiskPortfolios)
library(tidyquant)
library(fTrading)
library(corrplot)
library(GGally)
library(moments)
library(e1071)
library(psych)

```

# Observações

Primeiro serão resolvidas as questões do livro de finanças do professor e posteriormente as questões sobre a base de dados apresentados em sala

#Questão 2.2 
##intem (1) - importação dos dados e cálculo do EWMA e MA
A importação dos dados e os gráficos estão nos códigos em seguida


```{r Importanto os dados, warning=FALSE}

#importando os dados

petro <-  getSymbols("PETR4.SA", src = "yahoo", from = "2014-01-01", to = "2018-12-31", auto.assign = FALSE)
oi <-  getSymbols("OIBR3.SA", src = "yahoo", from = "2014-01-01", to = "2018-12-31", auto.assign = FALSE)
vale <-  getSymbols("VALE3.SA", src = "yahoo", from = "2014-01-01", to = "2018-12-31", auto.assign = FALSE)
cielo <-  getSymbols("CIEL3.SA", src = "yahoo", from = "2016-01-01", to = "2019-12-31", auto.assign = FALSE)
b3 <-  getSymbols("B3SA3.SA", src = "yahoo", from = "2014-01-01", to = "2018-12-31", auto.assign = FALSE)
ibovespa <-  getSymbols("^BVSP", src = "yahoo", from = "2014-01-01", to = "2018-12-31", auto.assign = FALSE)

ibovespa2 <-  getSymbols("^BVSP", src = "yahoo", from = "2014-01-01", to = "2018-12-31", auto.assign = FALSE)
# retirando as NAs da Base do Ibovespa
ibovespa <- ibovespa[complete.cases(ibovespa),]

