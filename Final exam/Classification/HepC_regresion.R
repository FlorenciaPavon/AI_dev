install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("dplyr")

library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
#----------------------------------------------------------------------------------------
#OBJETIVO: armar un árbol de clasificación que pueda predecir si los donantes de sangre 
#son sanos o tienen una enfermedad relacionada con Hepatitis C (enfermedad hepática)
#----------------------------------------------------------------------------------------

datos <- read.csv('C:/Users/akane/OneDrive/Documentos/hcvdat0.csv')


plot(datos)
#summary(datos)
str(datos)

#elimino la columna X
datos <- datos %>% select(-X)
str(datos)
unique(datos$Category) 
#categorías encontradas:
#0=Blood Donor""0s=suspect Blood Donor" "1=Hepatitis" "2=Fibrosis" "3=Cirrhosis" 


#creo un dataset con la misma cantidad de columnas y tipos de datos pero de datos
#con valores de referencia para las enzimas, marcadores que estan en el dataset 
#de HCV

# n <- 307 enfoque 50/50 que no servia para clasificar
n <- 492 #enfoque 60/40, mas realista, mas sanos que enfermos

hombres_sanos <- data.frame(
  Category = rep("00=Normal Values", n),
  Age = sample(19:77, n, replace = TRUE),
  Sex = rep("m", n),
  ALB = runif(n, 3.5, 5.0), #runif crea n valores aleatorios entre los numeros de parametros normales
  ALP = runif(n, 40, 130),
  ALT = runif(n, 10, 40),
  AST = runif(n, 10, 40),
  BIL = runif(n, 0.1, 1.2),
  CHE = runif(n, 5.3, 12.9),
  CHOL = runif(n, 120, 199),
  CREA = runif(n, 0.74, 1.35),
  GGT = runif(n, 10, 71),
  PROT = runif(n, 6.3, 7.9)
)

mujeres_sanas <- data.frame(
  Category = rep("00=Normal Values", n),
  Age = sample(19:77, n, replace = TRUE),
  Sex = rep("f", n),
  ALB = runif(n, 3.5, 5.0),
  ALP = runif(n, 35, 105),
  ALT = runif(n, 7, 35),
  AST = runif(n, 9, 32),
  BIL = runif(n, 0.1, 1.2),
  CHE = runif(n, 4.0, 11.3),
  CHOL = runif(n, 120, 199),
  CREA = runif(n, 0.59, 1.04),
  GGT = runif(n, 6, 42),
  PROT = runif(n, 6.3, 7.9)
)

normales <- rbind(hombres_sanos, mujeres_sanas)
str(normales)

#verifico cuantos NA o nulos hay y en que columnas
colSums(is.na(normales))


#unir con el dataset descargado
datos_modelo <- rbind(normales, datos)
datos_modelo <- na.omit(datos_modelo)

#verifico limpieza
colSums(is.na(datos_modelo))

summary(normales)
summary(datos)
summary(datos_modelo)
unique(datos_modelo$Category)

    
#DIVISION DE DATOS
#datos_entrenamiento: son todos las categorias MENOS los donantes.
#datos_prediccion: son todos los donantes 

datos_entrenamiento <- datos_modelo %>% filter(!(Category %in% c("0=Blood Donor", "0s=suspect Blood Donor")) ) 
str(datos_entrenamiento) 
unique(datos_entrenamiento$Category)

datos_prediccion <- datos_modelo %>% filter(Category %in% c("0=Blood Donor", "0s=suspect Blood Donor"))
unique(datos_prediccion$Category)
datos_prediccion$Category <- as.factor(datos_prediccion$Category)


set.seed(123)

modelo_arbol <- rpart(Category ~ Age + ALT + ALB +CHOL+ PROT +AST +BIL +ALP + GGT + CREA + CHE, data = datos_entrenamiento, method = "class")
rpart.plot(modelo_arbol)

#analisis de marcadores 
poder_prediccion <- varImp(modelo_arbol)
poder_prediccion %>% 
  arrange(desc(Overall))


prediccion_arbol_simple <- predict(modelo_arbol, newdata = datos_prediccion, type = "class")
#le agrego type porque es multiclase sino devuelve una matriz de probabilidades
plot(prediccion_arbol_simple)

confusion_arbol<- table(Real = datos_prediccion$Category, Predicho = prediccion_arbol_simple)
print(confusion_arbol)

#La mayoria de los donantes tienen alguna enfermedad, corrijo el arbol por ajuste y despues comparo
#ajuste del arbol
#veamos que representa cada sección:
#min.split: indica la cantidad minima de observaciones para realizar una división o split en cada nodo.
#minbucket: indica la cantidad minima de observaciones en nodos terminales, es decir la clasificación en si.
#maxdepth: indica la profundidad maxima de los nodos del arbol final. Dependiendo de los datos, esta profundidad puede alcanzarse o no.

#OJO CON OVERFITTING O UNDERFITTING, SI LA CONFIABILIDAD YA ES ALTA (MAS DE 95) puede que no necesiten ajustar, pero para probar que pasa ajusten.

control <- rpart.control(minsplit = 5, minbucket = 2, maxdepth = 2)

modelo_arbol_ajustado <- rpart( Category ~ Age + ALT + AST+ ALP + GGT + CREA+ CHE,data = datos_entrenamiento, method = "class", control = control )
rpart.plot(modelo_arbol_ajustado)

predicciones <- predict(modelo_arbol_ajustado , newdata=datos_prediccion)
prediccion_arbol_simple_ajustado <- predict(modelo_arbol_ajustado, newdata = datos_prediccion, type = "class")

confusion_arbol_ajustado<- table(Real = datos_prediccion$Category, Predicho = prediccion_arbol_simple_ajustado)
print(confusion_arbol_ajustado)
#mas casos clasificados con alguna enfermedad que antes del ajuste

#RANDOM FOREST
#como son muchas clases diferentes pruebo con un random forest

install.packages("randomForest")
library(randomForest)

#pasar a factor Category
datos_entrenamiento$Category <- as.factor(datos_entrenamiento$Category)

colSums(is.na(datos_entrenamiento))
modelo_random_forest<- randomForest(Category ~ Age + ALT + AST + ALP + GGT + CREA + CHE, data = datos_entrenamiento) 

poder_prediccion <- varImp(modelo_random_forest)
poder_prediccion %>% 
  arrange(desc(Overall))


prediccion_rf <- predict(modelo_random_forest, newdata = datos_prediccion)
plot(prediccion_rf)

#matriz de confusion
confusion_rf <- table(Real = datos_prediccion$Category, Predicho = prediccion_rf)
print(confusion_rf)

#ambos modelos interpretan que la mayoria de los donantes tienen una enfermedad

#Ajuste del RF

modelo_rf_ajustado <- randomForest(
  Category ~ Age + ALT + AST + ALP + GGT + CREA + CHE,
  data = datos_entrenamiento,
  ntree = 100, #arboles a constryir
  mtry = 3, #cantidad de variables en cada division
  nodesize = 7, #similar a minbucket
  maxnodes = 10, #limita la profundidad total
 
)

poder_prediccion_rf <- varImp(modelo_rf_ajustado)
poder_prediccion_rf %>% 
  arrange(desc(Overall))

prediccion_rf_ajustado <- predict(modelo_rf_ajustado, newdata = datos_prediccion)
confusion_rf_ajustado <- table(Real = datos_prediccion$Category, Predicho = prediccion_rf_ajustado)
print(confusion_rf_ajustado)

#sigue dando que la mayoria está enfermo
#el modelo no interpreta a los sanos, cambio la cantidad de personas de n=614
#agregue mas datos normales - no funciono
#agrego mas marcadores - no funciono

#---------------------------------------------------------------------------------
#ingreso los datos de las personas que ya se que son normales, como grupo control
#---------------------------------------------------------------------------------
length(datos_prediccion$Category)
parte_normales <- normales[sample(nrow(normales), 533), ]

prediccion_arbol_simple1 <- predict(modelo_arbol, newdata = parte_normales, type = "class")
plot(prediccion_arbol_simple1)

clase_real <- rep("00=Normal Values", nrow(parte_normales)) 

confusion_arbol1<- table(Real = clase_real, Predicho = prediccion_arbol_simple1)
print(confusion_arbol1)

#efectivamente toma todos los datos normales, como normales, por lo que la clasificacion
#parece estar bien hecha



