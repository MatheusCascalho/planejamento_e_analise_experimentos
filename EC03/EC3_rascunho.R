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

# Defina os parâmetros
# dimensaoensoes, configuracoes e repeticoes devem ser definidos como vetores ou listas.
dimensoes<- c(10, 20)  # Exemplos de dimensaoensões

# Configurações
configuracoes <- list(
  list(name = "Config 1", recpars = list(name = "recombination_blxAlphaBeta", alpha = 0, beta = 0), mutpars = list(name = "mutation_rand", f = 4)),
  list(name = "Config 2", recpars = list(name = "recombination_exp", cr = 0.6), mutpars = list(name = "mutation_best", f = 2))
)

repeticoes <- 1:2  # Número de repetições

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
      
    }
    resultados[as.character(dimensao), configuracao$name] <- mean(resultados_config)
    
  }
}


