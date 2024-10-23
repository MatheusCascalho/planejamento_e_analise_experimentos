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

imc <- rbind(imc2016, imc2017)



# Calculando o IMC de cada população
imc$imc <- imc$weight.kg/(imc$height.m^2)

# Segregação de dados entre Homens e Mulheres
imc_feminino <- imc[imc$Sex == 'F',]
imc_masculino <- imc[imc$Sex == 'M',]

#hist(imc$imc, col = 'grey')
#hist(imc_masculino$imc, col = 'lightblue', probability=TRUE)
#hist(imc_feminino$imc, col = 'red', probability=TRUE, add = TRUE)



# Teste de hipótese
mu_0 = 21.7 # IMC que representa o peso normal
delta = 3.2

t.test(
  imc$imc, 
  alternative = 'two.sided', 
  mu=21.7, 
  conf.level = 0.99
)

# Tamanho do efeito
mu_bar = mean(imc$imc)
s = sd(imc$imc)
d <- (mu_bar - mu_0)/s

power.t.test(n           = length(imc$imc), 
             delta       = delta, 
             sd          = s, 
             sig.level   = 0.01, 
             type        = "one.sample", 
             alternative = "two.sided")


# Melhorias do estudo
# Avaliação de outras métricas, como bioimpedância para identificação de massa
# magra e gorduras. Homens tendem a ter mais massa magra devido a composição 
# corporal masculina, enquanto mulheres tem mais regiões de acúmulo de gordura
# como mamas e glúteos.k
