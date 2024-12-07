setwd("C:/Users/victo/OneDrive/Escritorio/TFM_Tokyo")
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
install.packages("randomForest")
library(randomForest)
install.packages("jtools")
install.packages("ggstance")
install.packages("rpart")
library(jtools)
library(ggstance)
library(rpart)
data <- read_csv("Trafico_aereo.csv")

options(scipen = 999)

unique(data$`Operating Airline`)

datos_limpios <- na.omit(data)
air_china_data <- datos_limpios %>% filter(`Operating Airline` == "Air China")

Berlin_G <- datos_limpios %>% filter(`Operating Airline` == "Air Berlin" & `Boarding Area` == "G")

datos_limpios %>% 
  group_by(`Operating Airline`) %>% 
  summarise(media = mean(`Passenger Count`, na.rm = T))

datos_limpios %>% 
  group_by(`GEO Region`) %>% 
  summarise(maximo = max(`Passenger Count`, na.rm = T))
class(data$`Passenger Count`)

resultado_sd <- sd(datos_limpios$`Passenger Count`)

datos_temporadas <- datos_limpios %>%
  group_by(Month) %>%
  summarise(desviacion_estandar = sd(`Passenger Count`, na.rm = TRUE))

datos_limpios$mes <- NA
datos_limpios$mes[datos_limpios$Month == "January"] <- 1
datos_limpios$mes[datos_limpios$Month == "February"] <- 2
datos_limpios$mes[datos_limpios$Month == "March"] <- 3
datos_limpios$mes[datos_limpios$Month == "April"] <- 4
datos_limpios$mes[datos_limpios$Month == "May"] <- 5
datos_limpios$mes[datos_limpios$Month == "June"] <- 6
datos_limpios$mes[datos_limpios$Month == "July"] <- 7
datos_limpios$mes[datos_limpios$Month == "August"] <- 8
datos_limpios$mes[datos_limpios$Month == "September"] <- 9
datos_limpios$mes[datos_limpios$Month == "October"] <- 10
datos_limpios$mes[datos_limpios$Month == "November"] <- 11
datos_limpios$mes[datos_limpios$Month == "December"] <- 12

datos_limpios$mes <- factor(datos_limpios$mes,
                             levels =c(1,2,3,4,5,6,7,8,9,10,11,12),
                             labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
                             
                             
                             
datos_ano_mes <- datos_limpios %>%
  group_by(mes) %>%
  summarise(Media_pasajeros_mes = mean(`Passenger Count`, na.rm = TRUE)) %>%
  arrange(mes) #tambien podemos usar factor para convertir los meses en numeros.

datos_ano_mes$mes <- as.numeric(datos_ano_mes$mes)

datos_ano_mes %>% 
  group_by(mes) %>% 
  ggplot(aes(x = mes, y = Media_pasajeros_mes)) +
  geom_point() +
  geom_line(size = 1, color = "#347383") +
  scale_x_continuous(breaks = 1:12)

#grafico por aerolinea mes del año y pasajeros
#grafico aerolinea geo summary y pasajeros
#grafico aerolinea año y pasajeros 

datos_aerolinea <- datos_limpios %>% 
  group_by(`Operating Airline`, mes) %>%
  summarise(Media_pasajeros_mes_aerolinea = mean(`Passenger Count`, na.rm = TRUE)) %>%
  arrange(mes) 
  
datos_aerolinea%>%
  group_by(mes) %>%
  ggplot(aes(x = mes, y = Media_pasajeros_mes_aerolinea)) +
  geom_point() +
  geom_line(size = 1, color = "#347383") +
  facet_wrap(~ `Operating Airline`) +
  labs(title = "Media de Pasajeros por Mes para Cada Aerolínea",
       x = "Mes", 
       y = "Media de Pasajeros")

datos_aerolinea %>%
  filter(`Operating Airline` == "Air Berlin") %>%
  ggplot(aes(x = mes, y = Media_pasajeros_mes_aerolinea)) +
  geom_point() +
  geom_line(size = 1, color = "#347383") 
  
datos_aerolinea_ano <- datos_limpios %>% 
  group_by(Year, `Operating Airline`) %>%
  filter(`Operating Airline` == "American Airlines") %>%
  summarise(Media_pasajeros_ano_aerolinea = mean(`Passenger Count`, na.rm = TRUE)) %>%
  arrange(Year) 

datos_aerolinea_ano %>%
  filter(`Operating Airline` == "American Airlines") %>%
  ggplot(aes(x = Year, y = Media_pasajeros_ano_aerolinea)) +
  geom_point() +
  geom_line(size = 1, color = "#347383")

#comparacion por geo region

datos_aerolinea_region <- datos_limpios %>% 
  filter(`GEO Region` == "Europe" | `GEO Region` == "US") %>%
  group_by(`Operating Airline`, `GEO Region`) %>%
  summarise(Media_pasajeros_region_aerolinea = mean(`Passenger Count`, na.rm = TRUE)) %>%
  arrange(`Operating Airline`)

datos_region_europa_mes <- datos_limpios %>% 
  filter(`GEO Region` == "Europe") %>%
  group_by(mes) %>%
  summarise(Media_pasajeros_region_aerolinea = mean(`Passenger Count`, na.rm = TRUE)) %>%
  arrange(mes)

datos_max_region_pasajeros <- datos_limpios %>%
  group_by(`GEO Region`) %>%
  arrange(`GEO Region`, desc(`Passenger Count`)) %>%
  slice(1) # es un head pero que trabaja con cada categoria del group_by


unique(datos_limpios$`GEO Region`)
datos_limpios$hemisferio <- NA
datos_limpios$hemisferio[datos_limpios$`GEO Region` == "Asia" | datos_limpios$`GEO Region` == "Canada" | datos_limpios$`GEO Region` == "US" | datos_limpios$`GEO Region` == "Europe"] <- "Norte"
datos_limpios$hemisferio[datos_limpios$`GEO Region`== "Australia / Oceania" | datos_limpios$`GEO Region`== "South America"] <- "Sur"
datos_limpios$hemisferio[datos_limpios$`GEO Region`== "Central America" | datos_limpios$`GEO Region`== "Middle East" | datos_limpios$`GEO Region`== "Mexico" ] <- "Centro"


datos_limpios$hemisferio <- factor(datos_limpios$hemisferio,
                            levels =c("Norte", "Centro", "Sur"),
                            labels = c("Norte", "Centro", "Sur"))


data_2008_norte <- datos_limpios %>%
  filter(Year == 2008 & hemisferio == "Norte") %>%
  group_by(mes) %>%
  summarise(total_pasajeros_mes = sum(`Passenger Count`)) 

ggplot(data_2008_norte, aes(x = mes, y = total_pasajeros_mes, group = 1)) +
  geom_point(size = 3, color = "#347383") +        # Puntos
  geom_line(size = 1, color = "#347383") +         # Línea conectando los puntos
  theme_minimal() +                                # Usar un tema minimalista
  labs(x = "Mes", y = "Total Pasajeros", title = "Total de Pasajeros por Mes en 2008 (Norte)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_2008_norte$mes <- as.numeric(data_2008_norte$mes)
cor_pearson_año_pasajeros <- cor(data_2008_norte$mes, data_2008_norte$total_pasajeros_mes, method = "pearson")
print(cor_pearson_año_pasajeros)

data_2008_centro <- datos_limpios %>%
  filter(Year == 2008 & hemisferio == "Centro") %>%
  group_by(mes) %>%
  summarise(total_pasajeros_mes = sum(`Passenger Count`))

ggplot(data_2008_centro, aes(x = mes, y = total_pasajeros_mes, group = 1)) +
  geom_point(size = 3, color = "#347383") +        # Puntos
  geom_line(size = 1, color = "#347383") +         # Línea conectando los puntos
  theme_minimal() +                                # Usar un tema minimalista
  labs(x = "Mes", y = "Total Pasajeros", title = "Total de Pasajeros por Mes en 2008 (Centro)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_2008_sur <- datos_limpios %>%
  filter(Year == 2008 & hemisferio == "Sur") %>%
  group_by(mes) %>%
  summarise(total_pasajeros_mes = sum(`Passenger Count`))

ggplot(data_2008_sur, aes(x = mes, y = total_pasajeros_mes, group = 1)) +
  geom_point(size = 3, color = "#347383") +        # Puntos
  geom_line(size = 1, color = "#347383") +         # Línea conectando los puntos
  theme_minimal() +                                # Usar un tema minimalista
  labs(x = "Mes", y = "Total Pasajeros", title = "Total de Pasajeros por Mes en 2008 (Sur)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

datos_limpios$`Operating Airline` <- as.factor(datos_limpios$`Operating Airline`)
data_2008_norte$region <- as.factor(data_2008_norte$region)
data_2008_norte$mes <- as.factor(data_2008_norte$mes)
data_2008_norte$año <- as.factor(data_2008_norte$año)




colnames(datos_limpios)[colnames(datos_limpios) == "Operating Airline"] <- "Aerolineas"
colnames(datos_limpios)[colnames(datos_limpios) == "GEO Region"] <- "GEO_Region"
colnames(datos_limpios)[colnames(datos_limpios) == "Passenger Count"] <- "Cantidad_pasajeros"
modelo_rf <- randomForest(Cantidad_pasajeros ~ GEO_Region + Year + mes, data = datos_limpios)

print(modelo_rf)

predicciones <- predict(modelo_rf)

print(predicciones)

anova_result <- aov(Cantidad_pasajeros ~ GEO_Region, data = datos_limpios)
summary(anova_result) # utilizar analisis de onva para ver que variables influyen mas en cantidad de pasajeros

tabla_contingencia <- table(datos_limpios$mes, datos_limpios$Cantidad_pasajeros)
chi_cuadrado_resultado <- chisq.test(tabla_contingencia)
print(chi_cuadrado_resultado)