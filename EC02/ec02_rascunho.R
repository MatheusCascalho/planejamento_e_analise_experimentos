# Carregar o arquivo CSV
dados <- read.csv("DadosAcoesGrupoG.csv", header = FALSE)

# Renomear as colunas
colnames(dados) <- c("Ação 1", "Ação 2", "Ação 3", "Ação 4", "Ação 5")

# Criar um novo data.frame para rentabilidades
rentabilidade <- data.frame(matrix(NA, nrow = nrow(dados), ncol = ncol(dados)))
colnames(rentabilidade) <- paste0("Rentabilidade ", colnames(dados))

# Calcular a rentabilidade para cada coluna
for (i in 1:ncol(dados)) {
  # Cálculo da rentabilidade: (valor atual - valor anterior) / valor anterior
  rentabilidade[1:(nrow(dados) - 1), i] <- (dados[1:(nrow(dados) - 1), i] - dados[2:nrow(dados), i]) / dados[2:nrow(dados), i]
}

library(ggplot2)
library(reshape2)

# Transformar o data frame em formato longo para usar com ggplot
# rentabilidade
rentabilidade <- na.omit(rentabilidade)
rentabilidade_classificada <- melt(rentabilidade*100, variable.name = "Ação", value.name = "Valor")

# Plotar histogramas para cada coluna
ggplot(rentabilidade_classificada, aes(x = Valor)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  facet_wrap(~Ação, scales = "free") +
  labs(title = "Histogramas das Ações", x = "Valores", y = "Frequência") +
  theme_minimal()

rentabilidade_classificada$Ação <- as.factor(rentabilidade_classificada$Ação)


# Computational modeling and analysis
my.model <- aov(Valor ~ Ação, 
                data = rentabilidade_classificada)
summary.aov(my.model)


# Verificação de premissas
# Residuals
my.model$residuals


# Check normality
shapiro.test(my.model$residuals)


# library(car)
# png(filename = "../figs/paperqq.png",
#     width = 600, height = 600, 
#     bg = "transparent")


qqnorm(my.model$residuals)
qqline(my.model$residuals)

# Check homoscedasticity
fligner.test(rentabilidade_classificada$Valor ~ rentabilidade_classificada$Ação, 
             data = rentabilidade_classificada)


## Multiple comparisons

# Situation 1: all vs. all
library(multcomp)
# rentabilidade_classificada.Ação <- rentabilidade_classificada$Ação

m <- mcp(Ação="Tukey")
mc1    <- glht(my.model, linfct=m)
mc1_CI <- confint(mc1, level = 0.95)
plot(mc1_CI, 
     xlab       = "Rentabilidade da ação",
     sub        = "- Dados da ação -",
     cex.axis   = 1.2,
     cex        = 2)

# A ação 5 e a 1 são as melhores! Recomendamos a 5 pois ela se sobressai mais
# que a 1, que possui valores negativos conforme visto no histograma 

## Sample size calculations for anova
a       <- 4
alpha   <- 0.05
sigma   <- sd(rentabilidade_classificada$Valor)
delta   <- 1.618  # Razão áurea
beta    <- 0.2

# Case 1: two levels symmetrically biased about the grand mean
tau <- c(-delta / 2, 
         delta / 2, 
         rep(0, a - 2)) # define tau vector
vartau <- var(tau)  # variance between groups
power.anova.test(groups      = 4, 
                 between.var = vartau, 
                 within.var  = sigma ^ 2, 
                 sig.level   = alpha, 
                 power       = 1 - beta)$n

#qqPlot(my.model$residuals, 
#       pch = 16, 
#       lwd = 3, 
#       cex = 2, 
#       las = 1)

# Adicionar as rentabilidades ao dataset original
# dados_com_rentabilidade <- cbind(dados, rentabilidade)

# Salvar o resultado em um novo arquivo
# write.csv(dados_com_rentabilidade, "Dados_Com_Rentabilidade.csv", row.names = FALSE)

# print("Cálculo de rentabilidade realizado e arquivo salvo como 'Dados_Com_Rentabilidade.csv'")
