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

# Observa��es

Primeiro ser�o resolvidas as quest�es do livro de finan�as do professor e posteriormente as quest�es sobre a base de dados apresentados em sala

#Quest�o 2.2 
##intem (1) - importa��o dos dados e c�lculo do EWMA e MA
A importa��o dos dados e os gr�ficos est�o nos c�digos em seguida


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

