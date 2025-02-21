library(insuranceData)
library(dplyr)
library(skimr)
library(visdat)
library(ggplot2)

#activar el data frame de dataCar
data(dataCar)
#para conocer la estructura del data frame
str(dataCar)
#puedo tener una vista de las medidas de tendencia central de cada una de las variables
summary(dataCar)

#resumen mas detallado de cada una de las variables
skim(dataCar)

#los primeros 7 valores de todo mi conjunto de datos 
head(dataCar)

#vemos los ultimos valores 
tail(dataCar)
#conocer la dimension del conjuunto de datos
dim(dataCar)

#puedes observar como esta definida exactamente cada variable (opcion diferente a str)
glimpse(dataCar)

#conocer el nombre de las columnas ambas (son iguales)  
names(dataCar)
colnames(dataCar)

#conocer si hay datos faltantes::: FALSE---significa que no hay datos faltantes 
any(is.na(dataCar))
#visualizar la estructura y el conjunto de datos 
vis_dat(dataCar)
#grafica los datos faltantes y te da el porcentaje de datos faltantes
vis_miss(dataCar)

#numero que incurren en reclamaciones nos fijamos en (numclaims)
sum(dataCar$numclaims!=0)
#porcentaje
((4624/67856)*100)
#top 5 de vehiculos con mayor numero de reclamaciones
claims_tipo<-dataCar%>%
  group_by(veh_body)%>%
  summarise(totclaims=sum(numclaims))%>%
  arrange(desc(totclaims))
#muestra solo los primeros 5 
head(claims_tipo)
 
#grafico de barras para vehiculos con mayor numero de relcamaciones
 ggplot(claims_tipo, aes(x=reorder(veh_body, -totclaims), y=totclaims))+
   geom_bar(stat= "identity",fill="blue", color="black")+ #stat es para que no se junten las barras
   labs(title="Numero de Reclamaciones por tipo de Vehiculo",
        x= "tipo de vehiculo",
        y="Total de reclamaciones")+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, hjust=1))
 
 
#top 10 de vehiculos con mayor monto de reclamaciones
 
 reclamaciones_tipo<-dataCar%>%
   group_by(veh_body)%>%
   summarise(total_claims=sum(claimcst0))%>%
   arrange(desc(total_claims))

#muestra solo los primeros 10
 head(reclamaciones_tipo, 10)
 
 # Visualización del Top 10 de vehículos con mayor monto reclamado
 ggplot(reclamaciones_tipo, aes(x=reorder(veh_body, -total_monto), y=total_monto)) +
   geom_bar(stat="identity", fill="red", color="black") +
   labs(title="Top 10 Vehículos con Mayor Monto de Reclamaciones",
        x="Tipo de Vehículo",
        y="Monto Total de Reclamaciones") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust=1))
 
 #analisis por genero 
 #contar el numero de reclamaciones por genero 
 claims_genero<-dataCar%>%
   group_by(gender)%>%
   summarise(numclaims=sum(numclaims))%>%
   arrange(desc(numclaims))
 
 #mostrar el genero con el mayor numero de reclamaciones
 print(claims_genero)

 #grafico de barras por genero 
 ggplot(claims_genero, aes(x = reorder(veh_body, -totclaims), y= totclaims, fill=gender))+
   geom_bar(stat="identity", position = "dodge")+ #dodge para que las barras salgan mas delgadas
   labs(title = "Tipo de vehiculos mas reclamaciones por genero ",
        x= "Tipo de vehiculos",
        y = "Numero de reclamaciones")+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 #el numero de reclamaciones por genero y vehiculo 
 claims_genero_vehiculo <- dataCar %>%
   group_by(gender, veh_body) %>%
   summarise(numclaims = sum(numclaims)) %>%
   arrange(desc(numclaims))
 
 print(claims_genero_vehiculo)
 
 # Visualización de u grafico de l frecuencia de relcamciones por genero y vehiculo
 ggplot(claims_genero_vehiculo, aes(x=reorder(veh_body, -numclaims), y=numclaims, fill=gender)) +
   geom_bar(stat="identity", position="dodge") +
   labs(title="Frecuencia de Reclamaciones por Género y Tipo de Vehículo",
        x="Tipo de Vehículo",
        y="Número de Reclamaciones") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 #tipo de vehiculo mas asegurado por genero 
 vehiculos_genero <- dataCar %>%
   group_by(gender, veh_body) %>%
   summarise(count = n()) %>%
   arrange(desc(count))
 
 ggplot(vehiculos_genero, aes(x=reorder(veh_body, -count), y=count, fill=gender)) +
   geom_bar(stat="identity", position="dodge") +
   labs(title="Tipos de Vehículos Más Asegurados por Género",
        x="Tipo de Vehículo",
        y="Cantidad de Seguros") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
 
 
    