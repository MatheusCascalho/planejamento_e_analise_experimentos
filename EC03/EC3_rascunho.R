suppressWarnings(suppressPackageStartupMessages(library(smoof)))
# FOR INSTANCE: set dimensao = 10
dimensao <- 10
fn <- function(X){
  if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
  Y <- apply(X, MARGIN = 1,
             FUN = smoof::makeRosenbrockFunction(dimensions = dimensao))
  return(Y)
}
# testing the function on a matrix composed of 2 points
X <- matrix(runif(20), nrow = 2) # runif gera uma distribuição uniforme aleatória
fn(X)

# FOR INSTANCE: set dimensao = 10


suppressPackageStartupMessages(library(ExpDE))


# Configurações
configuracoes <- list(
  list(name = "Config 1", recpars = list(name = "recombination_blxAlphaBeta", alpha = 0, beta = 0), mutpars = list(name = "mutation_rand", f = 4)),
  list(name = "Config 2", recpars = list(name = "recombination_exp", cr = 0.6), mutpars = list(name = "mutation_best", f = 2))
)

# DEFINIR VARIANCIA ESTIMADA
# OBTÉM A DIFERENÇA DA VARIÂNCIA ESTIMADA DAS AMOSTRAS
# para os grupos selecionados, 
# selecionar 5 aleatórios e verificar quantas iterações são necessárias em cada grupo para chegar na variância esperada
#

################### CALCULO DA QUANTIDADE DE REPETIÇÕES ###################################
sample_iterations <- function(instance, algo1, algo2, se_threshold, n0, n_max) {
  # Inicializando as amostras iniciais
  x1 <- sample(algo1(instance), n0, replace = TRUE)
  x2 <- sample(algo2(instance), n0, replace = TRUE)
  
  # Número inicial de execuções
  n1 <- n0
  n2 <- n0
  
  # Função para calcular erro padrão da diferença simples
  calc_se <- function(x1, x2) {
    s1 <- sd(x1)
    s2 <- sd(x2)
    sqrt(s1^2 / length(x1) + s2^2 / length(x2))
  }
  
  # Calcula o erro padrão inicial
  se_current <- calc_se(x1, x2)
  
  # Loop até atingir o limite de precisão ou o orçamento máximo
  while (se_current > se_threshold && (n1 + n2) < n_max) {
    # Calcula a proporção ótima de amostras
    s1 <- sd(x1)
    s2 <- sd(x2)
    r_opt <- s1 / s2
    
    # Determina qual algoritmo amostrar com base na proporção
    if (n1 / n2 < r_opt) {
      # Adiciona uma nova amostra para o algoritmo 1
      new_sample <- algo1(instance)
      x1 <- c(x1, new_sample)
      n1 <- n1 + 1
    } else {
      # Adiciona uma nova amostra para o algoritmo 2
      new_sample <- algo2(instance)
      x2 <- c(x2, new_sample)
      n2 <- n2 + 1
    }
    
    # Atualiza o erro padrão
    se_current <- calc_se(x1, x2)
  }
  
  # Retorna os resultados
  list(
    x1 = x1,
    x2 = x2,
    n1 = n1,
    n2 = n2,
    se = se_current
  )
}

###################################################################

# DEFINIÇÃO DE SE*
# CALCULAR VARIANCIA TOTAL SIGMA_PHI
# SIGMA_PHI = SIGMA_phi + SIGMA_ERRO
# SIGMA_phi = VARIANCIA DAS INSTANCIAS
# SIGMA_ERRO = VARIANCIA DO ERRO DE MEDIÇÃO
# Erro de medição: erro de cada repetição em relação ao valor médio 
# Para definir o número de repetições para encontrar o sigma_erro
# teste para extrair o número de amostrar para encontrar a variancia da população

## estimativa do erro
n_amostrar_para_erro = 10
n_dimensoes_para_erro = 10
SIGMA_phi = 3857629.7 # Desvio padrão das funções objetivo de 38 instâncias

# estimativa de amostras
se_threshold, 
n0=2
n_max=50
amostras_teste=10

dimensoes_teste = sample(dimensoes, amostras_teste)


###################################################################
########## EXECUÇÃO DO ExpDE #####################################

library("CAISEr")
# Defina os parâmetros
# dimensaoensoes, configuracoes e repeticoes devem ser definidos como vetores ou listas.
# dimensoes<- c(10, 20)  # Exemplos de dimensaoensões
repeticoes <- 1:n_amostrar_para_erro  # Número de repetições

n_blocos = calc_instances(2, 0.5, power=0.8) # 2: n instancias, 0.5: tamanho de efeito mínimo; power: poder do teste
dimensoes = seq(2,150,length.out=38)

# Parâmetros adicionais para o ExpDE
resultados <- data.frame(matrix(ncol = length(configuracoes), nrow = length(dimensoes)))
colnames(resultados) <- sapply(configuracoes, function(config) config$name)
rownames(resultados) <- dimensoes

# Loop sobre dimensaoensões, configurações e repetições
for (dimensao in dimensoes) {
  probpars$dimensaoensions <- dimensao  # Atualiza o número de dimensaoensões no problema
  for (configuracao in configuracoes) {
    resultados_config <- c()  # Vetor para armazenar os resultados das repetições
    for (repeticao in repeticoes) {
      selpars <- list(name = "selection_standard")
      stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dimensao, maxiter = 100 * dimensao)
      probpars <- list(name = "fn", xmin = rep(-5, dimensao), xmax = rep(10, dimensao))
      popsize = 5 * dimensao
      # Executa o algoritmo ExpDE e armazena o resultado
      # Run algorithm on problem:
      out <- ExpDE(mutpars = configuracao$mutpars,
                   recpars = configuracao$recpars,
                   popsize = popsize,
                   selpars = selpars,
                   stopcrit = stopcrit,
                   probpars = probpars,
                   showpars = list(show.iters = "dots", showevery = 20))
      # Extract observation:
      resultados_config <- c(resultados_config, out$Fbest)  # Supondo que "fitness" seja uma métrica retornada
      cat("Configuracao", configuracao$name, "| dimensão: ", dimensao, "| resultado: ", resultados_config)
      #print(resultados_config)
    }
    resultados[as.character(dimensao), configuracao$name] <- mean(resultados_config)
    
  }
}

write.csv(resultados, "resultados_comparacao_algoritmo.csv")

novo_df <- data.frame(
  Dimensão = rep(rownames(resultados), times = ncol(resultados)),
  Configuração = rep(colnames(resultados), each = nrow(resultados)),
  Resultado = as.vector(as.matrix(resultados))
)
write.csv(resultados, "resultados_comparacao_algoritmo_REORGANIZADO.csv")



################################################################################
######### TESTE DE BLOCAGEM ####################################################

model <- aov(Resultado~Configuração+Dimensão,
             data = novo_df)

summary(model)
summary.lm(model)$r.squared