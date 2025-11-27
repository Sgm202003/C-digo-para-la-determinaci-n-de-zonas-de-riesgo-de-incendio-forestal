# Codigo-para-la-determinacion-de-zonas-de-riesgo-de-incendio-forestal
Este código ha sido desarrollado con la intención de establecer mediante análisis multivariado, las variables que permitan establecer mediante regresión logística, las zonas de riesgo de incendio potencial, espero pueda beneficiarles y ser mejorado, muchas gracias de antemano por sumarse al esfuerzo de proteger nuestros recursos naturales.

# 1. Instalar librerías necesarias para el desarrollo del análisis
#install.package (c("sf", "raster", "sp", "carData", "car"))
# 2. Cargar las librerias
library(sf) # Trabajar con objetos espaciales
library(raster) # Manipular datos raster
library(sp) # Libreria complementaria para datos espaciales
library(carData) # Es útil para practicar y probar funciones de análisis
# de datos y modelos.
library(car) # Proporciona herramientas para análisis de regresión
# 3. Especificar la ruta completa del archivo
insol <- "C:/Users/Asus/Desktop/Capas finales/03 Insolación/Insolacion.tif"
alti <- "C:/Users/Asus/Desktop/Capas finales/04 Altitud/DEM.tif"
pc <- "C:/Users/Asus/Desktop/Capas finales/06 Puntos de calor/Prueba.shp"
# 4. Convertir a objeto
pc <- st_read(pc)
insol <- raster(insol)
alti <- raster(alti)
# 5. Incorporar los valores de los raster al archivo de puntos
insola <- terra::extract(insol, pc)
pc$insolacion <- insola
69
al <- terra::extract(alti, pc)
pc$altitud <- al
str(pc)
# 6. Convertir a tipo factor la variable dependiente
pc$USOT <- as.factor(pc$USOT)
pc$TIPO <- as.numeric(pc$TIPO)
pc$Cobertura <- as.numeric(pc$Cobertura)
pc$Distance <- as.numeric(pc$Distance)
str(pc)
# 7. Calcular el modelo
potencial_incendios <- glm(USOT ~ TIPO + Distance + Temperatur + insolacion + altitud,
data = pc,
family = binomial())
# 8. Identificar los coeficientes
#log(p/1/p) = B0 + B1 (X)...Bn (X)
summary(potencial_incendios)
exp(potencial_incendios$coefficients)
# 9. Significancia del modelo
#Matriz de correlación:Si la correlación es cercana a 1 o -1, hay colinealidad
pc_numeric <- st_drop_geometry(pc)
pc_numeric <- pc_numeric[, c("TIPO", "Distance", "Temperatur", "insolacion", "altitud")]
pc_numeric <- na.omit(pc_numeric)
70
matriz_correlacion <- cor(pc_numeric)
#VIF: mide cuánto aumenta la varianza de un coeficiente debido a la colinealidad.
# Un VIF > 10 sugiere colinealidad problemática
vif(potencial_incendios)
# Tolerancia: La tolerancia es el inverso del VIF. Un valor de tolerancia < 0.1 indica colinealidad.
# 10. Modelo de regresión logística
#log-odds(Y)= 4.397 + (3.123e-04 * Distance) + (-6.601e-04 * insolacion) + (8.099e-04 * altitud)
# 11. Reajuste del modelo sin las variables que no guardan significancia
potencial_incendios <- glm(USOT ~ Distance + insolacion + altitud,
data = pc,
family = binomial())
summary(potencial_incendios)
# log-odds-ajustada (Y) = 5.037 + (3.172e-04 * Distance) + (-6.801e-04 * insolacion) + (8.162e-
04 * altitud)
10.2 Anexo 2. Código para estimación de la probabilidad de riesgo.
# 1. Cargar las librerías
library(terra)
library(sf)
# 2. Cargar el raster
insol <- "C:/Users/Asus/Desktop/Capas finales/03 Insolación/Insolacion.tif"
alti <- "C:/Users/Asus/Desktop/Capas finales/04 Altitud/DEM.tif"
71
cami <- "C:/Users/Asus/Desktop/Capas finales/07 Vectores/caminos.tif"
# 3. Optimizar los raster
insol <- rast(insol)
alti <- rast(alti)
cami <- rast(cami)
insol_reducido <- aggregate(insol, fact = 10)
altit_reducido <- aggregate(alti, fact = 10)
Insolación <- as.polygons(insol_reducido)
Altitud <- as.polygons(altit_reducido)
Caminos <- as.polygons(cami)
writeVector(Insolación, "C:/Users/Asus/Desktop/Capas finales/Insolacion_poligonos.shp",
overwrite = TRUE)
writeVector(Atitud, "C:/Users/Asus/Desktop/Capas finales/Altitud_poligonos.shp", overwrite =
TRUE)
writeVector(Caminos, "C:/Users/Asus/Desktop/Capas finales/Caminos_poligonos.shp", overwrite
= TRUE)
# 4. Cargar la librería sf para manejar shapefiles
library(sf)
# 5. Cargar el shapefile convinado en SIG
probabilidad <- st_read("C:/Users/Asus/Desktop/Capas finales/08 Probabilidad/FINAL.shp")
# 6. Verificar las columnas del shapefile
print(names(probabilidad))
72
# 7. Calcular la log-odds ajustada
# Fórmula: log-odds-ajustada (Y) = 5.037 + (3.172e-04 * Distance) + (-6.801e-04 * insolacion) +
(8.162e-04 * altitud)
probabilidad$log_odds_ajustada <- 5.037 + (3.172e-04 * probabilidad$caminos) + (-6.801e-04 *
probabilidad$Insolacion) + (8.162e-04 * probabilidad$FID_Altitu)
probabilidad$pra <- plogis(probabilidad$log_odds_ajustada)
# 8. Guardar el shapefile actualizado (opcional)
st_write(probabilidad, "C:/Users/Asus/Desktop/Probabilidd.shp", overwrite = TRUE)
