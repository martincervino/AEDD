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
#' ---
#' 
#' #### **1.  Lectura de datos y análisis exploratorio**
#' 
#' 
#' **_a) Realizar un breve análisis descriptivo del conjunto de datos y generar un 
#' histograma de la respuesta._**

head(temp_andalu)
str(temp_andalu)
summary(temp_andalu)
attr(temp_andalu, "fecha") # Mes de las observaciones
#' Las temperaturas se tomaron en enero de 2022. <br>

#' Asignamos a una variable a la temperatura de la que vamos a realizar un análisis descriptivo unidimensional.
t <- temp_andalu$temp 
summary(t)

#' Histograma de la respuesta:
hist(t, xlab = "temperatura", main = "", freq = FALSE) 
lines(density(t), col = 'blue')
#' La distribución no es una normal, hay una ligera asimetría.

qqnorm(t)
qqline(t)

#' Test de normalidad de Shapiro-Wilk:
shapiro.test(t)
#' Según el test no hay normalidad. 
#' 
#' ***
#' 
#' **_b) Convertir el `data.frame` a un objeto `sf`, definiendo el CRS mediante la 
#' cadena de texto `"+proj=utm +zone=30 +ellps=WGS84 +units=km"`, y representar 
#' la distribución espacial de la respuesta. ¿Se observa algún patrón (más o menos claro)?_**
#'

#' Vamos a convertir los datos a un objeto sf.
#' Mantenemos lan y lot como posibles variables expicativas.
crs <- "+proj=utm +zone=30 +ellps=WGS84 +units=km" # Definimos el CRS

temp_andalu_sf <- st_as_sf(temp_andalu, coords = c("lon", "lat"), remove = FALSE, agr = "constant", crs = crs)



#' Representación de la distribución espacial de la respuesta, asignando un color dependiendo de la temperatura medida en cada observación.
plot(temp_andalu_sf["temp"], pch = 20, cex = 2, breaks = "quantile", nbreaks = 4)

#' Se observa en general temperatura mayor en en los puntos del sur y suroeste y una menor en los puntos del norte y noreste.
#' Estos patrones nos hacen pensar que la tendencia no es constante.
#' 
#' ***
#'     
#' **_c) Completar el análisis descriptivo de la variabilidad de gran escala. 
#' ¿Aparentemente hay tendencia espacial? En caso afirmativo, sugerir un modelo lineal._**
#'        

#' Completamos el analisis descriptivo de la variabilidad de gran escala con unos 
#' gráficos de dispersión de la respuesta frente a coordenadas.

x <- temp_andalu_sf$lon
y <- temp_andalu_sf$lat
old.par <- par(mfrow = c(1, 2), omd = c(0.05, 0.95, 0.01, 0.95))
plot(x, t) # lon frente a temperatura
lines(lowess(x, t), lty = 2, lwd = 2, col = 'blue')
plot(y, t) # lat frente a temperatura
lines(lowess(y, t), lty = 2, lwd = 2, col = 'blue')

par(mfrow = c(1,1))

#' Se observa como las dos componentes espaciales, tanto como la longitud como la latitud
#' tienen un efecto en la respuesta por lo que es lógico pensar que existe dependencia espacial.
#' Una tendencia lineal parece adecuada, sugerimos un modelo lineal temp ~ lon + lat

#' Ajustamos el modelo por ols
temp.ols <- lm(temp ~ lon + lat, data = temp_andalu_sf)
summary(temp.ols)

#' Analizamos los residuos
res <- residuals(temp.ols)
summary(res)

#' Hacemos un histograma de los residuos
hist(res, xlab = "ols residuals", main = "", freq = FALSE)
lines(density(res), col = 'blue')
#' Los residuos se asemejan bastante a una distribución normal

qqnorm(res)
qqline(res)

#' Test de normalidad de Shapiro-Wilk
shapiro.test(res)
#' Hay normalidad en los residuos
#' 
#' ***
#'    
#' **_d) Volver a representar la distribución espacial de la respuesta (o solo las posiciones 
#' de observación si surgen dificultades) junto con los límites administrativos de
#' Andalucía (que se pueden obtener empleando el paquete `mapSpain`)._**
#'        


library(mapSpain)
library(ggplot2)
andalucia_limites <- mapSpain::esp_get_ccaa(ccaa="andalucia")


#' Creamos los cuantiles para la distribuir los colores
quantiles <- quantile(temp_andalu_sf$temp, probs = c(0, 0.25, 0.5, 0.75, 1))

#' Hacemos el grafico usando ggplot juntando los límites de Andalucía junto a 
#' la representación de la distribución espacial de la respuesta
#' 
ggplot() +
  geom_sf(data = andalucia_limites, fill = "transparent", color = "black", lwd = 1) +
  geom_sf(data = temp_andalu_sf, aes(color = temp_andalu_sf$temp), pch = 20, cex = 5) +
  scale_color_gradient(breaks = quantiles, low = "blue", high = "red", name = "Temperatura") +
  ggtitle("Temperatura en el mes de enero de 2022 en la comunidad de Andalucía")



#' #### **2. Modelado de la dependencia espacial**
#' 
#' **_a) Analiza la variabilidad de pequeña escala empleando el estimador clásico 
#' del semivariograma, considerando 20 saltos hasta un salto máximo de 290._**
#'        

library(gstat)

maxlag <- 290 # Salto máximo de 290
# Estimador clásico del semivariograma-> cressie = False
vario <- variogram(temp ~ lon + lat, temp_andalu_sf, cutoff = maxlag, width = maxlag/20)

#' Represantación de las estimaciones junto con el número de aportaciones:
plot(vario, plot.numbers = TRUE)

#' Observamos que las tres primeras estimaciones tienen una semivarianza baja
#' comparada con el resto. A partir de la tercera estimación hay un salto de semivarianza.
#' 
#' ***
#' 
#' **_b. Ajustar a las estimaciones piloto un modelo (isotrópico) de semivariograma
#' exponencial y uno esférico, mediante WLS. ¿Con cuál se obtiene un mejor ajuste?
#' Interpretar las estimaciones obtenidas de los parámetros del semivariograma._**
#'
#'
#' Primero definimos el modelo exponencial con _vgm()_:
modelo_exp <- vgm(model = "Exp", nugget = NA) # Valores iniciales por defecto
#' Ajustamos el modelo con _fit.variogram()_ empleando pesos inversamente proporcionales a la varianza (_fit.method = 2_)
fit_exp <- fit.variogram(vario, modelo_exp, fit.method = 2)
fit_exp
#' Asignamos a variables para trabajar con ellas posteriormente
nugget_exp <- fit_exp$psill[1]
sill_exp <- nugget_exp + fit_exp$psill[2]
#' En el caso de un variograma exponencial, el parámetro que aparece como range 
#' es un parámetro de escala proporcional al verdadero rango práctico (tres veces ese valor).
#' Asi que multiplicamos por 3:
range_exp <- 3*fit_exp$range[2]
#' Parámetros del semivariograma:
params_exp <- matrix(c("nugget_exp", "sill_exp", "range_exp", nugget_exp, sill_exp, range_exp), 2, 3, byrow = TRUE)
params_exp
#' INTERPRETAR PARÁMETROS 


plot(vario, fit_exp)

plot(vario$dist, vario$gamma, xlab = "distance", ylab = "semivariance",
     xlim = c(0, max(range_exp*1.1, maxlag)), ylim = c(0, sill_exp*1.2))
lines(variogramLine(fit_exp, maxdist = max(range_exp*1.1, maxlag)))
abline(v = 0, lty = 3)
abline(v = range_exp, lty = 3)
abline(h = nugget_exp, lty = 3)
abline(h = sill_exp, lty = 3)

#' Ahora definimos el modelo esférico:
modelo_sph <- vgm(model = "Sph", nugget = NA) # Valores iniciales por defecto
#' Ajustamos el modelo con _fit.variogram()_ empleando pesos inversamente proporcionales a la varianza (_fit.method = 2_)
fit_sph <- fit.variogram(vario, modelo_sph, fit.method = 2)
fit_sph
#' Asignamos a variables para trabajar con ellas posteriormente
nugget_sph <- fit_sph$psill[1]
sill_sph <- nugget_sph + fit_sph$psill[2]
#' En el caso de un variograma esférico el parametro _range_ ya es el verdadero rango práctico.
range_sph <- fit_sph$range[2]
#' Parámetros del semivariograma:
params_sph <- matrix(c("nugget_sph", "sill_sph", "range_sph", nugget_sph, sill_sph, range_sph), 2, 3, byrow = TRUE)
params_sph 

#' INTERPRETAR PARÁMETROS 


plot(vario, fit_sph)

plot(vario$dist, vario$gamma, xlab = "distance", ylab = "semivariance",
     xlim = c(0, max(range_sph*1.1, maxlag)), ylim = c(0, sill_sph*1.2))
lines(variogramLine(fit_sph, maxdist = max(range_sph*1.1, maxlag)))
abline(v = 0, lty = 3)
abline(v = range_sph, lty = 3)
abline(h = nugget_sph, lty = 3)
abline(h = sill_sph, lty = 3)


#' Vamos a comparar los dos modelo a través del valor mínimo de la función objetivo WLS. <br>
#' Modelo exponencial:
attr(fit_exp, "SSErr")
#' Modelo esférico:
attr(fit_sph, "SSErr")
#' Parece que el modelo esférico obtiene un mejor ajuste, acontinuación haremos un validación
#' cruzada, (mas recomendable). 


#' 
#' ***
#'        
#' **_c) Emplear medidas de validación cruzada, considerando 10 grupos, para seleccionar 
#'    el modelo final (establecer la semilla igual al número de grupo multiplicado 
#'    por 10; no es necesario generar gráficos). _**
#'        
#' #### **3. Predicción espacial**
#' 
#' **_a. Crear una rejilla de predicción de dimensiones 150x75 que cubra los límites 
#'    de Anadalucía e intersecarla con dicha región (o una rejilla que cubra las 
#'    posiciones de observación si surgen dificultades)._**
#'        



#'        
#'  **_b. Empleando el modelo obtenido en el ejercicio anterior, calcular las 
#'    prediccies y varianzas kriging en la rejilla de predicción, y representarlas._**
