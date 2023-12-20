#' ---
#' title: 'AEDD: Trabajo Estadística Espacial'
#' author: 'Grupo 8'
#' date: "Curso 2023/2024"
#' output:
#'   html_document: default
#'   word_document: default
#'   pdf_document: default
#' ---
#' 
#' <!--
#' El siguiente comando genera el fichero .R equivalente en formato spin:
#'   knitr::purl("Trabajo_espacial.Rmd", documentation = 2)
#' Podemos revertir el proceso con el comando:
#'   knitr::spin("Trabajo_espacial.R",knit = FALSE)
#' Si hay problemas al generar directamente el informe en formato pdf, se puede 
#' generar en formato word y convertirlo después a pdf.
#' -->
#' 
#' Este trabajo debe entregarse en formato pdf, incluyendo el código R utilizado, 
#' las correspondientes salidas y los comentarios (o interpretaciones de los resultados) 
#' pertinentes (para ello se recomienda emplear RMarkdown, a partir de un fichero *.Rmd* 
#' o un fichero *.R* mediante spin, que también debe entregarse).
#' 
#' Se empleará el conjunto de datos `temp_andalu` almacenado en el archivo *temp_andalu_X.RData*, 
#' donde *X* es el número de grupo, que contiene temperaturas medias (`temp`) mensuales 
#' (correspondientes al mes `attr(temp_andalu, "fecha")`, seleccionado al azar de los 
#' últimos dos años) en las estaciones de la Red de Información Agroclimática de Andalucía. 
#' Las posiciones de las estaciones (`lon`, `lat`) están en coordenadas proyectadas 
#' UTM en kilómetros. 
#' El objetivo de este trabajo es realizar un análisis geoestadístico
#' de estos datos, utilizando las herramientas explicadas durante las clases.
#' 

library(sf)
setwd("C:/Users/marti/Desktop/UNI/AEDD/TrabajoXeo/AEDD") # Poned vuestra ruta

load("./datos_trabajo/temp_andalu_8.RData") 


#' 
#' La fecha límite de entrega de este trabajo es el **29 de diciembre de 2023** (para 
#' poder disponer de la corrección antes del periodo de exámenes). En la evaluación, se 
#' tendrá en cuenta la correcta resolución de los ejercicios, la claridad y corrección 
#' en la redacción del trabajo, la interpretación y discusión de los resultados y el
#' correcto funcionamiento de los scripts entregados. En caso de que se considere
#' oportuno, se podrá solicitar a los alumnos la presentación oral del trabajo.
#' 
#' 
#' # Ejercicios
#' 
#' 1.  Lectura de datos y análisis exploratorio
#' 
#'     a. Realizar un breve análisis descriptivo del conjunto de datos y generar un 
#'        histograma de la respuesta.
#' 
#'     b. Convertir el `data.frame` a un objeto `sf`, definiendo el CRS mediante la 
#'        cadena de texto `"+proj=utm +zone=30 +ellps=WGS84 +units=km"`, y representar 
#'        la distribución espacial de la respuesta. ¿Se observa algún patrón 
#'        (más o menos claro)?
#'        
#'     c. Completar el análisis descriptivo de la variabilidad de gran escala. 
#'        ¿Aparentemente hay tendencia espacial? En caso afirmativo, 
#'        sugerir un modelo lineal. 
#'        
#'     d. Volver a representar la distribución espacial de la respuesta (o solo las posiciones 
#'        de observación si surgen dificultades) junto con los límites administrativos de
#'        Andalucía (que se pueden obtener empleando el paquete `mapSpain`).

### a)
head(temp_andalu)
str(temp_andalu)
summary(temp_andalu)

# asignamos a una variable la temperatura de la que vamos a realizar una analisis descriptivo unidimensional 
t <- temp_andalu$temp 
summary(t)

# Histograma de la respuesta:
hist(t, xlab = "temperatura", main = "", freq = FALSE) 
lines(density(t), col = 'blue')
# la distribución no es una normal, hay una ligera asimetría


### b)
# Vamos a convertir los datos a un objeto sf
crs <- "+proj=utm +zone=30 +ellps=WGS84 +units=km"

temp_andalu_sf <- st_as_sf(temp_andalu, coords = c("lon", "lat"), remove = FALSE, agr = "constant", crs = crs)
# Mantenemos lan y lot como posibles variables expicativas.


# Representación de la distribución espacial de la respuesta.
plot(temp_andalu_sf["temp"], pch = 20, cex = 2, breaks = "quantile", nbreaks = 4)

# Se observa en general temperatura mayor en en los puntos del suroeste y una 
# menor en los puntos del noreste.


### c) 
# Completamos el analisis descriptivo de la variabilidad de gran escala con unos 
# gráficos de dispersión de la respuesta frente a coordenadas.

x <- temp_andalu_sf$lon
y <- temp_andalu_sf$lat
old.par <- par(mfrow = c(1, 2), omd = c(0.05, 0.95, 0.01, 0.95))
plot(x, t) # lon frente a temperatura
lines(lowess(x, t), lty = 2, lwd = 2, col = 'blue')
plot(y, t) # lat frente a temperatura
lines(lowess(y, t), lty = 2, lwd = 2, col = 'blue')

par(mfrow = c(1,1))

# Se observa como las componentes espaciales tienen un efecto en la respuesta por lo que es coherente
# pensar que existe dependencia espacial.
# Sugerimos un modelo lineal temp ~ lon + lat

# Ajustamos el modelo por ols
temp.ols <- lm(temp ~ lon + lat, data = temp_andalu_sf)
summary(temp.ols)

# Analizamos los residuos
res <- residuals(temp.ols)
summary(res)

# Hacemos un histograma de los residuos
hist(res, xlab = "ols residuals", main = "", freq = FALSE)
lines(density(res), col = 'blue')
# Los residuos se asemejan bastante a una distribución normal

### d)

library(mapSpain)
library(ggplot2)

# Creamos los cuantiles para la distribuir los colores
quantiles <- quantile(temp_andalu_sf$temp, probs = c(0, 0.25, 0.5, 0.75, 1))

# Hacemos el grafico usando ggplot juntando los limites de andalucía junto a 
# la representación de la distribución espacial de la respuesta
ggplot() +
  geom_sf(data = andalucia_limites, fill = "transparent", color = "black", lwd = 1) +
  geom_sf(data = temp_andalu_sf, aes(color = temp_andalu_sf$temp), pch = 20, cex = 5) +
  scale_color_gradient(breaks = quantiles, low = "blue", high = "red", name = "Temperature")


#' 2. Modelado de la dependencia espacial
#' 
#'     a. Analiza la variabilidad de pequeña escala empleando el estimador clásico 
#'        del semivariograma, considerando 20 saltos hasta un salto máximo de 290.
#' 
#'     b. Ajustar a las estimaciones piloto un modelo (isotrópico) de semivariograma
#'        exponencial y uno esférico, mediante WLS. ¿Con cuál se obtiene un mejor ajuste?
#'        Interpretar las estimaciones obtenidas de los parámetros del semivariograma.
#'        
#'     c. Emplear medidas de validación cruzada, considerando 10 grupos, para seleccionar 
#'        el modelo final (establecer la semilla igual al número de grupo multiplicado 
#'        por 10; no es necesario generar gráficos). 
#'        
#' 3. Predicción espacial
#' 
#'     a. Crear una rejilla de predicción de dimensiones 150x75 que cubra los límites 
#'        de Anadalucía e intersecarla con dicha región (o una rejilla que cubra las 
#'        posiciones de observación si surgen dificultades).
#'        
#'     b. Empleando el modelo obtenido en el ejercicio anterior, calcular las 
#'        prediccies y varianzas kriging en la rejilla de predicción, y representarlas.
