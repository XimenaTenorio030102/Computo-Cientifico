#Tenorio Espinal Ximena Elizabeth 

library(pacman)
p_load(haven, dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych)
require(psych)
library(haven)
library(readxl)
library(readr)

###############################Ejercicio 1#######################################################
data_p <- read.csv2("C:/Users/danie/Downloads/data_pca.csv")

View(data_p)
# Normalizar datos
data1 <- scale(data_p[,-16]) #[filas,columnas]

#datos normalizados
View(data1)

#Realizar pca
pca <- princomp(data1)

#diagnóstico
summary(pca)
#hasta el momento se observa que las principales componentes que aportaron mayor varianza son la 1, 2, 3, 4, 5, y 6
#considerar los componentes que tengan proporcion de varianza mayor al 5%

#revisar varianza y eigenvalores
fviz_eig(pca, choice = "variance")

fviz_eig(pca, choice = "eigenvalue")
#extraemos componentes 1, 2, 3, 4, 5 y 6.

#Análisis gráfico

#gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)
#Entre mas verde sea quiere decir que esta mejor representada, mientras que mas rojo sea esta peor representada en esas dos dimensiones.

#Gráfico de las cargas
fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)
#Son vectores: la magnitud puede cambiar pero el sentido y la dirección se mantienen

#Para visualizar puntuaciones se emplea un biplot
#La flecha indica la direccion en la que contribuyen las variables y donde se ubican los sujetos estudiados
fviz_pca_biplot(pca,
                col.var="red",
                col.ind = "black")
pca$loadings

x11()
psych::cor.plot(data1)

#análisis como lo proporciona spss
#Se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(data1))

pca2<- psych::principal(data1, nfactors=6, residuals= FALSE, rotate="varimax",
                        scores=TRUE, oblique.scores= FALSE, method="regression",
                        use="pairwise", cor="cor", weight= NULL)
pca2


#Matriz de coeficientes para las puntuaciones de los componentes
pca2$weights[,1]
pca2$weights[,2]
pca2$weights[,3]
pca2$weights[,4]
pca2$weights[,5]
pca2$weights[,6]

#Las variables son las siguientes:
pca2$scores

###########################EJERCICIO 2####################################################
################################COVID################################################################
data_pca2<- read_excel("C:/Users/danie/Downloads/Covid.xlsm")
View(data_pca2)

################Realizar pca año 2000####################################################
data_2000 <- data_pca2[, grepl("2000", names(data_pca2))]
View(data_2000)
# Normalizar datos
data2000 <- scale(data_2000[,-1]) #[filas,columnas]

#datos normalizados
View(data2000)

#Calcular factor de adecuación muestral de kaiser-Meyer
psych::KMO(data2000) #KMO > 0.5
#todas las variables poseen una msa mayor a 0.5, por lo que es pertinente el pca
#Diagnóstico para el PCA
pca2000 <- princomp(data2000)

#diagnóstico
summary(pca2000)
#hasta el momento se observa que las principales componentes que aportaron 
#mayor varianza son 1, 2 y 3

#revisar varianza y eigenvalores
fviz_eig(pca2000, choice = "variance")

fviz_eig(pca2000, choice = "eigenvalue")

#gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca2000,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)
#Entre mas verde sea quiere decir que esta mejor representada, mientras que mientras mas rojo sea esta peor representada en esas dos dimensiones.

#Gráfico de las cargas
fviz_pca_var(pca2000,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)
#Son vectores: la magnitud puede cambiar pero el sentido y la dirección se mantienen

#Para visualizar puntuaciones se emplea un biplot
#La flecha indica la direccion en la que contribuyen las variables y donde se ubican los sujetos estudiados
fviz_pca_biplot(pca2000,
                col.var="red",
                col.ind = "black")
pca2000$loadings

x11()
psych::cor.plot(data_2000)
#análisis como lo proporciona spss
#Se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(data_2000))
pca2000<- psych::principal(data_2000, nfactors=2, residuals= FALSE, rotate="varimax",
                        scores=TRUE, oblique.scores= FALSE, method="regression",
                        use="pairwise", cor="cor", weight= NULL)
pca2000

#Matriz de coeficientes para las puntuaciones de los componentes
pca2000$weights[,1]
pca2000$weights[,2]

#Las variables son las siguientes:
pca2000$scores

################################################Realizar pca año 2001###################################
data_2001 <- data_pca2[, grepl("2001", names(data_pca2))]
View(data_2001)
# Normalizar datos
data2001 <- scale(data_2001[,-1]) #[filas,columnas]

#datos normalizados
View(data2001)

#Calcular factor de adecuación muestral de kaiser-Meyer
psych::KMO(data2001) #KMO > 0.5
#todas las variables poseen una msa mayor a 0.5, por lo que es pertinente el pca
#Diagnóstico para el PCA
pca2001 <- princomp(data2001)

#diagnóstico
summary(pca2001)
#hasta el momento se observa que las principales componentes que aportaron 
#mayor varianza son 1, 2 y 3

#revisar varianza y eigenvalores
fviz_eig(pca2001, choice = "variance")

fviz_eig(pca2001, choice = "eigenvalue")
#solo dos componentes tienen un eigenvalor mayor a la unidad

#gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca2001,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)
#Entre mas verde sea quiere decir que esta mejor representada, mientras que mientras mas rojo sea esta peor representada en esas dos dimensiones.

#Gráfico de las cargas
fviz_pca_var(pca2001,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)
#Son vectores: la magnitud puede cambiar pero el sentido y la dirección se mantienen

#Para visualizar puntuaciones se emplea un biplot
#La flecha indica la direccion en la que contribuyen las variables y donde se ubican los sujetos estudiados
fviz_pca_biplot(pca2001,
                col.var="red",
                col.ind = "black")
pca2001$loadings


x11()
psych::cor.plot(data_2001)
#análisis como lo proporciona spss
#Se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(data_2001))
pca2001<- psych::principal(data_2001, nfactors=2, residuals= FALSE, rotate="varimax",
                           scores=TRUE, oblique.scores= FALSE, method="regression",
                           use="pairwise", cor="cor", weight= NULL)
pca2001

#Matriz de coeficientes para las puntuaciones de los componentes
pca2001$weights[,1]
pca2001$weights[,2]

#Las variables son las siguientes:
pca2001$scores
  
