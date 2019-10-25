
rm(list=ls()); #---- limpa todo o ambiente de variáveis

library(plyr);
library(agricolae);
library(VGAM);

#---- parâmetros de entrada

S0 <- 100
sigma <- 0.045
mu <- 0.000001
T <- 60
taxa_ano <- 0.12
K <- 95

#--- taxa de desconto diária

r <- (1 + taxa_ano)^(1 / 360) - 1; r

#---- cálculos via simulação de Monte Carlo

nsimul <- 20000;

log_STS0_simul <- rnorm(nsimul, mean = mu*T, sd = sigma*sqrt(T))
ST_simul <- S0 * exp(log_STS0_simul)

hist(ST_simul, col = "red")

prob_exercer_opcao <- sum(ST_simul < K) / nsimul
prob_exercer_opcao

VST_simul <- matrix(nrow = nsimul, ncol = 1, 0)

for (i in 1:nsimul)
{
    if (ST_simul[i] < K)
    {
        VST_simul[i] <- (K - ST_simul[i])/((1+r)^T);
    }
}

mean(VST_simul)

#---------------------------------------------------------------------------------#
#--- the end                                                                   ---#
#---------------------------------------------------------------------------------#
