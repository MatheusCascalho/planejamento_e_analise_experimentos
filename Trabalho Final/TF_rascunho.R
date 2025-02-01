suppressWarnings(suppressPackageStartupMessages(library(smoof)))

library(ggplot2)
library(dplyr)
library(multcomp)

################################################################################
######### GERAÇÃO E TRANFORMAÇÃO DOS DADOS ####################################################

df <- read.csv('/Users/henriquealvesbarbosa/Documents/planejamento-analise-experimentos/planejamento_e_analise_experimentos/Trabalho Final/Consolidated_Errors_Data.csv')
df$runway_frame <- as.factor(df$runway_frame)
df$frame <- as.numeric(df$frame)
df$error_pitch <- as.numeric(df$error_pitch)
df$error_roll <- as.numeric(df$error_roll)
df$error_distance <- as.numeric(df$error_distance)
df$model <- as.factor(df$model)

################################################################################
######### INÍCIO TESTES PITCH ######################################################

model_aov <- aov(error_pitch~model+runway_frame,
             data = df)

summary(model_aov)
summary.lm(model_aov)$r.squared

arrange(df, runway_frame)

# Análise da Configuração pela Dimensão
p <- ggplot(df, aes(x = frame, 
                         y = error_pitch, 
                         group = model, 
                         colour = model))

p + geom_line(linetype=2) + geom_point(size=5) + ggtitle("Título Personalizado") +
  theme(plot.title = element_text(size = 16, color = "black", hjust = 0.5))

################################################################################
######### Teste POST-HOC ######################################################

## comparação da configuraçao #######
tuktest     <- glht(model_aov,
                    linfct = mcp(model = "Tukey"))

summary(tuktest)

tuktestCI   <- confint(tuktest)

par(mar = c(5, 8, 4, 2), las = 1)
plot(tuktestCI,
     xlab = "Mean difference")

# Analisando o PITCH, 2 e PNP são os melhores

################################################################################
######### INÍCIO TESTES ROLL ######################################################

model_aov <- aov(error_roll~model+runway_frame,
                 data = df)

summary(model_aov)
summary.lm(model_aov)$r.squared

arrange(df, runway_frame)

# Análise da Configuração pela Dimensão
p <- ggplot(df, aes(x = frame, 
                    y = error_roll, 
                    group = model, 
                    colour = model))

p + geom_line(linetype=2) + geom_point(size=5) + ggtitle("Título Personalizado") +
  theme(plot.title = element_text(size = 16, color = "black", hjust = 0.5))

################################################################################
######### Teste POST-HOC ######################################################

## comparação da configuraçao #######
tuktest     <- glht(model_aov,
                    linfct = mcp(model = "Tukey"))

summary(tuktest)

tuktestCI   <- confint(tuktest)

par(mar = c(5, 8, 4, 2), las = 1)
plot(tuktestCI,
     xlab = "Mean difference", main="Comparação Configurações - Roll")

# Analisando o ROLL, 2 e PNP são os melhores

################################################################################
######### INÍCIO TESTES DISTANCE ######################################################

model_aov <- aov(error_distance~model+runway_frame,
                 data = df)

summary(model_aov)
summary.lm(model_aov)$r.squared

arrange(df, runway_frame)

# Análise da Configuração pela Dimensão
p <- ggplot(df, aes(x = frame, 
                    y = error_distance, 
                    group = model, 
                    colour = model))

p + geom_line(linetype=2) + geom_point(size=5) + ggtitle("Título Personalizado") +
  theme(plot.title = element_text(size = 16, color = "black", hjust = 0.5))

################################################################################
######### Teste POST-HOC ######################################################

## comparação da configuraçao #######
tuktest     <- glht(model_aov,
                    linfct = mcp(model = "Tukey"))

summary(tuktest)

tuktestCI   <- confint(tuktest)

par(mar = c(5, 8, 4, 2), las = 1)
plot(tuktestCI,
     xlab = "Mean difference")

# Analisando o DISTANCE, PNP é o melhor
