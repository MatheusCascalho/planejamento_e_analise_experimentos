suppressWarnings(suppressPackageStartupMessages(library(smoof)))

library(ggplot2)
library(dplyr)

################################################################################
######### TESTE DE BLOCAGEM ####################################################

novo_df <- read.csv('/Users/henriquealvesbarbosa/Documents/planejamento-analise-experimentos/planejamento_e_analise_experimentos/Trabalho Final/resultados_comparacao_algoritmo_REORGANIZADO.csv')
repeticoes_resultados <- read.csv("/Users/henriquealvesbarbosa/Documents/planejamento-analise-experimentos/planejamento_e_analise_experimentos/Trabalho Final/resultado_cada_repeticao.csv")

novo_df$dim_numeric <- as.numeric(novo_df$Dimensão)
novo_df$Configuração <- as.factor(novo_df$Configuração)
novo_df$Dimensão <- as.factor(novo_df$Dimensão)

model <- aov(Resultado~Configuração+Dimensão,
             data = novo_df)

summary(model)
summary.lm(model)$r.squared

arrange(novo_df, dim_numeric)

# Análise da Configuração pela Dimensão
p <- ggplot(novo_df, aes(x = dim_numeric, 
                         y = Resultado, 
                         group = Configuração, 
                         colour = Configuração))
p + geom_line(linetype=2) + geom_point(size=5)

################################################################################
######### Teste POST-HOC ######################################################

library(multcomp)
repeticoes_resultados$Config <- as.factor(repeticoes_resultados$Config)
repeticoes_resultados$Dimensao <- as.factor(repeticoes_resultados$Dimensao)
model2 <- aov(Valor~Config+Dimensao,
             data = repeticoes_resultados)

######################
## comparação da configuraçao #######
duntest     <- glht(model,
                    linfct = mcp(Configuração = "Dunnett"))

summary(duntest)

duntestCI   <- confint(duntest)

par(mar = c(5, 8, 4, 2), las = 1)
plot(duntestCI,
     xlab = "Mean difference")


######################
## comparação da configuração #######
duntest     <- glht(model2,
                    linfct = mcp(Dimensao = "Dunnett"))

summary(duntest)

duntestCI   <- confint(duntest)

par(mar = c(5, 8, 4, 2), las = 1)
plot(duntestCI,
     xlab = "Mean difference")


#########################################################
##### Checagem de premissas

# Check normality
shapiro.test(model$residuals)


qqnorm(model$residuals)
qqline(model$residuals)

hist(model$residuals)

plot(ecdf(model$residuals))
dados_test <- rnorm(100)
plot(ecdf(dados_test))


# Check homoscedasticity
fligner.test(novo_df$Resultado ~ novo_df$Configuração, 
             data = novo_df)

