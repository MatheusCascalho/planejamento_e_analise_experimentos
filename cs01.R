# Carregamento de dados

imc2016 <- read.csv('imc_20162.csv')
imc2017 <- read.csv('CS01_20172.csv', sep = ';')

# Filtragem de dados
imc2016 <- imc2016[imc2016$Course == 'PPGEE',]

# Padronização do dataframe
colnames(imc2016)[colnames(imc2016)=='Gender'] <- 'Sex'
colnames(imc2016)[colnames(imc2016)=='Height.m'] <- 'height.m'
colnames(imc2016)[colnames(imc2016)=='Weight.kg'] <- 'weight.kg'
colnames(imc2017)[colnames(imc2017)=='Weight.kg'] <- 'weight.kg'
imc2016 = imc2016[,c('Sex', 'height.m', 'weight.kg')]
imc2017 = imc2017[,c('Sex', 'height.m', 'weight.kg')]
imc2016$amostra <- '2016'
imc2017$amostra <- '2017'

# Calculando o IMC de cada população
imc2016$imc <- imc2016$weight.kg/(imc2016$height.m^2)
imc2017$imc <- imc2017$weight.kg/(imc2017$height.m^2)

# Normalização das amostras
n1 = length(imc2016$imc)
sigma_1 = sd(imc2016$imc)
mu_1 = mean(imc2016$imc)
imc2016$z0 <- (imc2016$imc - mu_1)/(sigma_1/sqrt(n1))

n2 = length(imc2017$imc)
sigma_2 = sd(imc2017$imc)
mu_2 = mean(imc2017$imc)
imc2017$z0 <- (imc2017$imc - mu_2)/(sigma_2/sqrt(n2))


imc <- rbind(imc2016, imc2017)




# Segregação de dados entre Homens e Mulheres
imc_feminino <- imc[imc$Sex == 'F',]
imc_masculino <- imc[imc$Sex == 'M',]

#hist(imc$imc, col = 'grey')
#hist(imc_masculino$imc, col = 'lightblue', probability=TRUE)
#hist(imc_feminino$imc, col = 'red', probability=TRUE, add = TRUE)


# Teste de hipótese

## - Teste de hipótese entre imc homens e mulheres da pós (supõe iid)
# IID(Independente e Igualmente distribuida): significa que temos uma amostra representativa da população total
# Vamos supor que o IMC de homens é igual ao de mulheres
# H0: mu_m = mu_h
# H1: mu_m < mu_h
# Teste unilateral. Podemos assumir isso pois acreditamos que o IMC das mulheres será menor que o dos homens
# Premissa: amostrar com a mesma variância
fligner.test(imc ~ Sex, data = imc) # H0: VARIANCIAS IGUAIS
# Resulto do teste de fligner < 0.05 -> podemos assumir variancias iguais


# Premissa: amostras seguem a normalidade
# H0: Distribuição é normal
# Resulto do teste de shapiro < 0.05 -> podemos assumir normalidade

shapiro.test(imc$imc[imc$Sex == "F"]) # p-value = 0.3179 -> aceitamos H0
power.t.test(delta       = mean(imc_feminino$imc)*0.1,
             sd          = sd(imc_feminino$imc),
             sig.level   = 0.05,
             # power       = 0.8,
             n = 11,
             type        = "one.sample",
             alternative = "one.sided")

shapiro.test(imc$imc[imc$Sex == "M"]) # p-value = 0.0618 -> rejeitamos H0, não segue normalidade (por muito pouco)

power.t.test(delta       = mean(imc_masculino$imc)*0.1,
             sd          = sd(imc_masculino$imc),
             sig.level   = 0.05,
             # power       = 0.8,
             n = 42,
             type        = "one.sample",
             alternative = "one.sided")


t.test(imc$imc ~ imc$Sex, 
       alternative = "less", 
       mu          = 0, 
       var.equal   = TRUE, 
       conf.level  = 0.95)
# p-value = 0.0003175 -> rejeitamos H0, ou seja, o IMC das populações é diferente. 
# H1: mu_m < mu_h
# Resulto do hipotese < 0.05 -> podemos rejeitar H0

power.t.test(delta       = mean(imc$imc)*0.1,
             sd          = sd(imc$imc),
             sig.level   = 0.05,
             # power       = 0.8,
             n = length(imc$imc),
             type        = "two.sample",
             alternative = "one.sided")
# power = 0.911528


library(confintr) # package for confidence intervals
library(ggplot2)

ci_masculino = ci_mean(imc_masculino$imc, probs = c(0.05, 0.95),
             type = c("t"))
ci_feminino = ci_mean(imc_feminino$imc, probs = c(0.05, 0.95),
                       type = c("t"))




for (i in 1:N) {
  # generate normally distributed sample of size n
  rsample <- rnorm(n, mean = 50, sd = 5)  
  # calculate confidence interval
  ci = ci_mean(rsample, probs = c(0.025, 0.975),
               type = c("t"))
  mean[i] <- ci$estimate
  L[i] <- ci$interval[1]
  U[i] <- ci$interval[2]
}

mean <- rep(0, 2)  # vector of zeros and size N
mean[1]=mean(imc_masculino$imc)
mean[2]=mean(imc_feminino$imc)

L <- rep(0, 2)  # vector of zeros and size N
L[1]=ci_masculino$interval[1]
L[2]=ci_feminino$interval[1]

U <- rep(0, 2)  # vector of zeros and size N
U[1]=ci_masculino$interval[2]
U[2]=ci_feminino$interval[2]



data <- data.frame(x = 1:2,
                   y = mean,
                   low = L,
                   up = U)

p <- ggplot(data, aes(x, y)) + geom_point() + 
            geom_errorbar(aes(ymin = low, ymax = up))

p

p + labs(x = "Intervalos")
p + labs(title = "Intervalos de Confian?a 95%")

# -----------------------------------------------------------------------

# DÚVIDAS
# Estimação do tamanho do efeito e do intervalo de
# confiança na grandeza de interesse;


