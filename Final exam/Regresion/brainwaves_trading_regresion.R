install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("dplyr")
install.packages("plotly")

library(plotly)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

#https://medium.com/@hsergeyfrolov/how-to-create-an-algorithmic-trading-tool-using-brainwaves-and-a-neural-interface-29b1a939961b

datos_trading <- read.csv("C:/Users/akane/OneDrive/Documentos/waves_metrics.csv")
str(datos_trading)

datos_trading <- na.omit(datos_trading)
datos_trading

datos_trading <- datos_trading %>% select(-X)

#===========================================================================================
#Cual predice mejor el precio? las ondas cerebrales del trader o las variables del mercado?
#===========================================================================================

cor(datos_trading)
#correlacion de pearson


which(abs(correlaciones["NormClose", ]) > 0.3) #mayor a 0.3 es correlacion moderada
#HighMomentum FastMomentum    NormClose 
#2            4            6 

#busco cuales son mayores

cor(datos_trading$NormClose, datos_trading$FastMomentum) #mayor 0.4023284
cor(datos_trading$NormClose, datos_trading$HighMomentum) #0.3614008
cor(datos_trading$FastMomentum, datos_trading$HighMomentum) #0.086807
#normlcose se relaciona bien con los momemntum pero no se ve relacion con las 
#ondas cerebrales

#comparacion de variables de mercado vs ondas cerebrales

#variables de mercado
cierre <- datos_trading$NormClose
fastM <- datos_trading$FastMomentum
highM <- datos_trading$HighMomentum

regresion_mercado = lm(cierre ~ fastM + highM , data=datos_trading)
print(regresion_mercado)
summary(regresion_mercado)

#evaluo cada parametro por separado para visualizar donde hay disminucion de los valores
regresion_mercado_fm = lm(cierre ~ fastM , data=datos_trading)
summary(regresion_mercado_fm)
plot(fastM, cierre, 
     main = "Regresión Lineal: cierre ~ FastMomentum",
     xlab = "FastMomentum", ylab = "NormClose", pch = 19, col = "steelblue")
abline(regresion_mercado_fm, col = "red", lwd = 2)

regresion_mercado_hm = lm(cierre ~ highM , data=datos_trading)
summary(regresion_mercado_hm)
plot(highM, cierre,
     main = "Regresión Lineal: cierre ~ High Momentum",
     xlab = "HighMomentum", ylab = "NormClose", pch = 19, col = "steelblue")
abline(regresion_mercado_hm , col = "red", lwd = 2)
#en ambos casos la regresion lineal tiene una correlacion pero muy baja
#el rsquared da 0.2 y 0.1, muy alejado de 1 (poder de prediccion con este
#modelo lineal de 6 27% y 16%)
#los datos se observan muy dispersos

#---------------------------------------------------------------
#Evaluacion con regresion logistica para mejor entendimiento
#variables del mercado, criterio subida de precio por comparacion de datos de cierre



#datos_trading$Sube <- ifelse(cierre > lag(cierre), 1, 0)#lag devuelve el valor anterior

datos_trading$Sube <- ifelse(datos_trading$NormClose > lag(datos_trading$NormClose), 1, 0)#lag devuelve el valor anterior
datos_trading <- na.omit(datos_trading)
fastM <- datos_trading$FastMomentum

regresiom_log_mercado <- glm(Sube ~ FastMomentum + HighMomentum, data = datos_trading, family = "binomial")
print(regresiom_log_mercado)
summary(regresiom_log_mercado)
#fastMomentum parece ser el mejor predictor

#grafico:
pred_prob <- predict(regresiom_log_mercado, type = "response")
plot(fastM, pred_prob, col = datos_trading$Sube + 1,
     main = "Probabilidad de que suba el precio",
     xlab = "FastMomentum", ylab = "Probabilidad predicha", pch = 19)

#Conclusion:
#FastMomentum tiene un efecto positivo fuerte y significativo porque a mayor 
#momentum, mas probabilidad de que el precio suba.

#Ondas cerebrales, separo los datos en sube o baja






