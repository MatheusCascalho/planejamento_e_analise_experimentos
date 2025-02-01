suppressWarnings(suppressPackageStartupMessages(library(smoof)))

library(ggplot2)
library(dplyr)
library(multcomp)
library(BSDA)

################################################################################
######### GERAÇÃO E TRANFORMAÇÃO DOS DADOS ####################################################

df <- read.csv('/Users/henriquealvesbarbosa/Documents/planejamento-analise-experimentos/planejamento_e_analise_experimentos/Trabalho Final/Consolidated_Errors_Data.csv')
df$runway_frame <- as.factor(df$runway_frame)
df$frame <- as.numeric(df$frame)
df$error_pitch <- as.numeric(df$error_pitch)
df$error_roll <- as.numeric(df$error_roll)
df$error_distance <- as.numeric(df$error_distance)
df$model <- as.factor(df$model)

df_times <- read.csv('/Users/henriquealvesbarbosa/Documents/planejamento-analise-experimentos/planejamento_e_analise_experimentos/Trabalho Final/execution_times_mean.csv')
df_times$runway_id <- as.factor(df_times$runway_id)
df_times$model_id <- as.factor(df_times$model_id)
df_times$execution_time_mean <- as.numeric(df_times$execution_time_mean)

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

p + geom_line(linetype=2) + geom_point(size=5) + ggtitle("Erros Pitch") +
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
     xlab = "Mean difference",
     main = "Comparação Modelos - Pitch")

# Analisando o PITCH, 2 e PNP são os melhores

######### TESTES DAS PREMISSAS ######################################################
# Check normality
shapiro.test(model_aov$residuals)


qqnorm(model_aov$residuals)
qqline(model_aov$residuals)

hist(model_aov$residuals)

plot(ecdf(model_aov$residuals))
dados_test <- rnorm(100)
plot(ecdf(dados_test))


# Check homoscedasticity
fligner.test(df$error_pitch ~ df$model, 
             data = df)

power.t.test(n=29, delta=1, sd=sd(df$error_pitch))

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

p + geom_line(linetype=2) + geom_point(size=5) + ggtitle("Erros Roll") +
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
     xlab = "Mean difference", 
     main="Comparação Modelos - Roll")

# Analisando o ROLL, 2 e PNP são os melhores

######### TESTES DAS PREMISSAS ######################################################
# Check normality
shapiro.test(model_aov$residuals)


qqnorm(model_aov$residuals)
qqline(model_aov$residuals)

hist(model_aov$residuals)

plot(ecdf(model_aov$residuals))
dados_test <- rnorm(100)
plot(ecdf(dados_test))


# Check homoscedasticity
fligner.test(df$error_roll ~ df$model, 
             data = df)

power.t.test(n=29, delta=1, sd=sd(df$error_roll))

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

p + geom_line(linetype=2) + geom_point(size=5) + ggtitle("Erros Distance") +
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
     xlab = "Mean difference",
     main="Comparação Modelos - Distance")

# Analisando o DISTANCE, PNP é o melhor

######### TESTES DAS PREMISSAS ######################################################
# Check normality
shapiro.test(model_aov$residuals)


qqnorm(model_aov$residuals)
qqline(model_aov$residuals)

hist(model_aov$residuals)

plot(ecdf(model_aov$residuals))
dados_test <- rnorm(100)
plot(ecdf(dados_test))


# Check homoscedasticity
fligner.test(df$error_distance ~ df$model, 
             data = df)

power.t.test(n=29, delta=1, sd=sd(df$error_distance))

######### TESTE NÃO PARAMÉTRICO ######################################################

# two sided H1
#H0: teta0 = 200
#H1: teta0 != 200
pnp_values <- df[df$model == 'PnP', ]$error_distance
model_1_values <- df[df$model == 'Model 1', ]$error_distance
model_2_values <- df[df$model == 'Model 2', ]$error_distance
model_3_values <- df[df$model == 'Model 3', ]$error_distance

X <- pnp_values - model_1_values
SIGN.test(X, md=0, alternative="less")

X <- pnp_values - model_2_values
SIGN.test(X, md=0, alternative="less")

hist(X, main="Histograma - PnP - Modelo 2", xlab="Diferença dos Erros")

X <- pnp_values - model_3_values
SIGN.test(X, md=0, alternative="less")

################################################################################
######### INÍCIO TESTES TEMPO ######################################################

model_aov <- aov(execution_time_mean~model_id+runway_id,
                 data = df_times)

summary(model_aov)
summary.lm(model_aov)$r.squared

p <- ggplot(df_times, aes(x = runway_id, 
                    y = execution_time_mean, 
                    group = model_id, 
                    colour = model_id))

p + geom_line(linetype=2) + geom_point(size=5) + ggtitle("Tempos") +
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
     xlab = "Mean difference",
     main="Comparação Modelos - Tempo")

# Analisando o DISTANCE, PNP é o melhor

######### TESTES DAS PREMISSAS ######################################################
# Check normality
shapiro.test(model_aov$residuals)

qqnorm(model_aov$residuals)
qqline(model_aov$residuals)

hist(model_aov$residuals)

plot(ecdf(model_aov$residuals))
dados_test <- rnorm(100)
plot(ecdf(dados_test))


# Check homoscedasticity
fligner.test(df_times$execution_time_mean ~ df_times$model_id, 
             data = df_times)

power.t.test(n=4, delta=0.1, sd=sd(df_times$execution_time_mean))