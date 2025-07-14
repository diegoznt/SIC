# Crearemos un vector (en R el igual = es <-)
numeros <- c(10,20,30,40,50)
numeros
View(numeros)
mas_cinco=numeros+5
mas_cinco

# Promedio
media_num <- mean(numeros)
media_num_mas <- mean(mas_cinco)

media_num_mas 
media_num

#Suma
suma_num <- sum(numeros)
suma_num_mas <- sum(mas_cinco)

suma_num_mas 
suma_num

# Cat es como el print
cat("La media:", media_num, "\nSuma:",suma_num) # El n es para que se vea en otra linea abajo

# DataFrame (no hay que llamar a ninguna libreria)

#Mas abajo es con igual como para diferenciar las jerarquias
estudiantes <- data.frame(
  Nombre = c("Ana","Luis","Carlos","Marta","Sofia"),
  Edad = c(23,22,21,23,22),
  Calificacion = c(95,88,92,85,90)
  )

View(estudiantes)

# Estudiantes con calificacion >90 (Filtro)

estudiantes_dest <- estudiantes[estudiantes$Calificacion>90,] # Ojo a la coma de al final
estudiantes_dest

# Visualizacion

#install.packages("ggplot2") # Instalamos paquete
library(ggplot2) # con esto llamamos a la libreria

data <- data.frame(
  Categoria = c("A","B","C","D"),
  Valores = c(10,23,17,9)
)

data

ggplot(data, aes(x=Categoria, y= Valores))+
  geom_bar(stat = "identity", fill= "steelblue")+
  labs(title = "Grafico de Barras", x = "Categorias", y = "Valores")

# Analisis estadistico simple

grupo_1 <- c(85, 90, 78, 92, 88, 91, 85, 87)
grupo_2 <- c(82, 89, 77, 91, 85, 86, 86, 88)

result_ttest=t.test(grupo_1,grupo_2)

result_ttest # Esto significa que, con 95% de confianza, la diferencia verdadera entre las medias está entre -3.24 y 6.24.


# Generar 100 numeros aleatorios distribucion normal

# la std va a tomar cada muestra individua la va a comparar con la media (es una especie de promedio de la diferencia entre las muestras y el mean)
datos_aleatorios <- rnorm(100,mean=50,sd=10) 
datos_aleatorios

hist(datos_aleatorios,
     main = "Histograma de datos aleatorios",
     xlab = "Valores",
     col = "darkblue",
     border = "black"
     )

# Me da algunas estadisticas
summary(datos_aleatorios)

# Simulacion lanzamiento de moneda

set.seed(123) # Fijamos la semilla
# El resultado es un vector de longitud 100 con una secuencia aleatoria de "Cara" y "Sello".
lanzamientos <- sample(c("Cara","Sello"), size = 100, replace = TRUE) 
#Si replace fuera falso no se permitiria repeticion y da error xq solo tenemos 2 valores y pedimos 100

# Nos da una tabla de cuantas caras y sellos hay
tabla <- table(lanzamientos)
tabla

barplot(tabla, 
        main = "Frecuencia de Cara y Sello", 
        col = c("skyblue", "salmon"), 
        ylab = "Frecuencia")

# Secuencia de numero del 1 al 9 (MATRICES) 
secuencia <- 1:9
matriz <- matrix(secuencia, nrow = 3, ncol = 3)
matriz

# Traspuesta de la Matriz
traspuesta <- t(matriz)
traspuesta

# VAMOS A TRABAJAR CON LA DATA

# Cargamos un vector de paquetes para ingresar varios al mismo tiempo
#install.packages(c("nycflights13", "tidyverse", "lubridate", "ggridges", "maps", "ggrepel", "tidyr"))
# Cuando ya instalemos una vez las comentamos para no estar instalandonas again and again

library(nycflights13)
library(tidyverse)
library(lubridate)
library(ggridges)
library(maps)
library(ggrepel)
library(tidyr)

# Vamos a asignar nombres a tablas
flights <- nycflights13::flights
airlines <- nycflights13::airlines
airports <- nycflights13::airports
planes <- nycflights13::planes
weather <- nycflights13::weather

View(weather) # vemos la tabla

glimpse(flights) # Nos muestra caracteristicas de la tabla, num de filas y columnas, tipo de datos, chr es caracter

summary(flights) # nos da un resumen estaditisco de cada variable numerica de la tabla

# Número de vuelos

flights %>% # Al parecer es como dentro de flights
  count(month) %>% # %>% trabaja por jerarquiaz e indentacion
  ggplot(aes(factor(month),n))+  # El + concatena un grafico o agrega uno dentro del ggplot
  geom_col()+ 
  labs(x = "Mes", y = "Numero de vuelos", title="Cantidad de vuelos por mes")

## Notas
#  El eje X serán los meses, pero tratados como categorías (factores)
#n xq count te da por defecto el n como la cuenta o numero de meses
#Aes es como para definir cosas del plot, como transp, color, como van lo ejes, size, etc

  ## Retrasos promedio por aerolinea
flights %>%
  group_by(carrier) %>%
  summarise(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%# na.rm elimina los valores NA y sigue con el calculo
  arrange(desc(mean_dep_delay)) %>% #Ordena en este caso la media de forma descendiente
 
  # Quise hacer el grafico jeje con un ejemplo del profe
  ggplot(aes(factor(carrier),mean_dep_delay))+ 
  geom_col()+ 
  labs(x = "Mes", y = "Numero de vuelos", title="Retrasos promedio segun aerolinea")


  ## Retrasos promedio por dia de la semana

flights %>%
  mutate(wday = wday(make_date(year, month, day), label=TRUE)) %>% # modifica con mutate
  group_by(wday) %>%
  summarise(mean_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x=wday,y=mean_dep_delay,group=1)) +
  geom_line()+ # Hacemos un grafico de linea
  labs(x="Dia de la semana",y="Retraso promedio")

#----Notas
# make crea una fecha con year, month, day y wday extrae el dia en Mon, Tue, etc.
# Esto crea una nueva columna llamada wday en el dataset. Ahora, cada fila (vuelo) tiene asociado el día de la semana en que ocurrió (por ejemplo "Mon", "Tue", etc.).
# con grup_by Ahora agrupa todos los vuelos que ocurrieron el mismo día de la semana
#Para cada grupo de vuelos (por ejemplo, todos los lunes), calcula el retraso promedio

## Distribución de los retrasos

flights %>%
  filter(!is.na(dep_delay)) %>% # filtra los delay sin datos
  ggplot(aes(dep_delay)) +
  geom_histogram(binwidth = 50, fill="skyblue", color="black")+
  labs(x="Retraso (min)",y="Frecuencia",title="Histograma retrasos de salida")

# ! significa "no", entonces esta expresión selecciona solo las filas donde dep_delay NO es NA.

# Obtenemos +estadisticas (MEDIANA, MEDIA Y QUARTILES)

flights %>%
  summarise(
    mean = mean(dep_delay, na.rm=TRUE), # coma para ir colocando en otra linea
    mediana = median(dep_delay, na.rm=TRUE),
    q25 = quantile(dep_delay, 0.25,na.rm=TRUE),
    q75 = mean(dep_delay, 0.75,na.rm=TRUE)
  )

## --- Resumenes bases cruzadas ---

#Aerolineas mas frecuentes

flights %>%
  count(carrier, sort=TRUE) %>%
  left_join(airlines, by = "carrier") %>%
  slice_max(n, n=10) %>%
  ggplot(aes(reorder(name,n),n))+
  geom_col(fill="steelblue")+
  coord_flip()+
  labs(x = "Aerolinea", y="Numero de vuelos", title="Top 10 Aerolineas por cantidad de vuelos")

# ---Nota-- 
# Primero entra a flights obtiene Carrier por vuelos, los cuenta y ordena (nos da una tabla carrier y n), luego
# une airlines con el carrier que obtuvimos y hace una tabla, carrier, nombre(aerolinea) y n
# luego slice selecciona lo primeros 10 aerolineas con mas numeros de vuelo.

# Destinos principales

flights %>%
  count(dest, sort=TRUE) %>%
  left_join(airports, by = c("dest"="faa")) %>% # aqui le dice que reemplace dest por faa (os que son iguales xq hay dest igual a faa)
  slice_max(n, n=10) %>% # muestra los n maximos
  select(dest, name, n) # Select me entrega en tabla o muestra solo las variables que quiero


# Retrasos promedio por aeropuerto de origen
flights %>%
  filter(origin %in% c("JFK", "LGA", "EWR")) %>%
  #count(origin) %>% no me sirve porque solo deja una tabla con n y el nombre sin delay
  group_by(origin) %>%
  summarise(mean_dep_delay = mean(dep_delay, na.rm = TRUE))
#- Notas-
# toma todos los valores de la columna dep_delay que pertenecen al grupo actual 
#(por ejemplo, todos los vuelos que salieron desde JFK) y calcula su promedio.

# EDAD DE AVIONES VS RETRASOS
planes_edad <- planes %>% 
    select(tailnum, plane_year = year) # le cambia el nombre de year a plane_year y
# arma una tabla solo con tailnum

flights %>% left_join(planes_edad, by= "tailnum") %>% # une flights con la columna tailnum y planes edad
  mutate(age=2013 - plane_year) %>% # Agrega la variable age
  filter(!is.na(age), age >= 0) %>%
  group_by(age) %>%
  summarise(mean_dep_delay= mean(dep_delay, na.rm=TRUE), n=n()) %>% # queremos que tenga un n (n cualquiera)
  ggplot(aes(age,mean_dep_delay))+
  geom_line(color = "darkred")+
  labs(x="Edad del avion (años)", y= "Retraso promedio de salida (min)",
  title="Retrasos según edad")
  

# CORRELACION RETRASOS SEGUN EL CLIMA
weather_joined <- flights %>%
  left_join(weather, by = c("origin","time_hour")) # ve las filas con el mismo hora y origen y los une
# Hasta aca tenemos el flights y weather cominados

# corr entre retraso y temp, windspeed, precip
corr_arr_wind <-cor(weather_joined$arr_delay, weather_joined$wind_speed, use="complete.obs")
corr_arr_precip <-cor(weather_joined$arr_delay, weather_joined$precip, use="complete.obs")
corr_arr_tem <-cor(weather_joined$arr_delay, weather_joined$temp, use="complete.obs")


cat("Correlacion entre arr_delay y wind_speed:",corr_arr_wind)
cat("Correlacion entre arr_delay y precip:",corr_arr_precip)
cat("Correlacion entre arr_delay y temp:",corr_arr_tem)

# cat("La media:", media_num, "\nSuma:",suma_num) ASI sería para colocar todo en un mismo Cat
# complete.obs Le indica a cor() que ignore las filas con valores NA en cualquiera de
# las dos columnas.



## -- PLOTEAMOS CORRELACION --
#install.packages("corrplot")
library(corrplot)

#Seleccionamos solo columnas numericas para hacer la corr
numericas <- weather_joined[sapply(weather_joined, is.numeric)]
numericas
# Aplica la función is.numeric a cada columna del data frame weather_joined.
#Devuelve un vector lógico (TRUE/FALSE) indicando qué columnas son numéricas.
# Usa ese vector lógico para seleccionar solo las columnas que son numéricas.

cor_matriz <- cor(numericas, use="complete.obs", method="pearson")

# Plot de la matriz
corrplot(cor_matriz, method="color", type = "upper", tl.cex = 0.8, addCoef.col = "black")

View(cor_matriz)
View(flights)

# Retrasos vs distancia por aerolinea
flights %>%
  ggplot(aes(distance,arr_delay, color = carrier))+ # color segun la variable carrier
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se=FALSE)+  # nos da la regresion lineal
  labs(title="Retrasos vs Distancia", x="Distancia",y="Retrasos llegada (min)")
# se = FALSE: evita que se dibuje la banda de confianza alrededor de la línea.
# Retrasos promedio por hora
# VEMOS CURVA DE REGRESION LINEAL

flights %>%
  group_by(hour) %>%
  summarise(prom = mean(dep_delay, na.rm=TRUE)) %>%
  ggplot(aes(hour,prom))+
  geom_point(alpha = 0.2)+
  geom_line(color="red")+
  geom_smooth(method = "lm", se=TRUE)+ # Te da la regresion lineal!  
  # Diferencias entre TRUE y FALSE
  #Dado este valor de X, estamos 95% seguros de que la media de Y estará dentro de esta franja
  labs(title="Retrasos promedio por hora", x="Hora",y="Retraso mean (min)")

# --- MAPAS  destinos principales ---

world_map <- map_data("world") 

top_dest <- flights %>%
  count(dest) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  slice_max(n, n = 50)

ggplot()+
  geom_map(data=world_map, map=world_map,
           aes(long, lat, map_id = region), fill = "gray80", color = "gray50")+
  geom_point(data = top_dest, aes(lon,lat, size=n), color = "red", alpha = 0.2)+
  coord_quickmap()+
  labs(title = "Top 50 destinos desde NYC")

# Retrasos diarios a lo largo del año

flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarise(media = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(date, media))+
  geom_line(color = "darkgreen")
  labs(title = "Retrasos promedio diarios", x = "Fecha", y="Retraso promedio (min)")

#---BOXPLOT ---# 
# Retrasos por aerolinea, top 5
  
  top5 <- flights %>%
    count(carrier, sort = TRUE) %>%
    slice_max(n, n=5) %>%
    pull(carrier)
  # Pull extrae solo la columna carrier como un vector simple 
  
  flights %>%
    filter(carrier %in% top5) %>%
    ggplot(aes(carrier, arr_delay))+
    geom_boxplot()+
    coord_flip()+ # Con esto damos vuelta los boxplot eje y en x y viceversa
    labs(title="Distribucion de retrasos por aerolinea (Top5)", x= "Aerolinea",
         y= "Retraso llegada (min)")

  
# Modelo Lineal predictivo
  
  df_model <- flights %>%
    select(year, month, day, dep_delay, distance, hour, origin, time_hour) %>%
    left_join(weather %>% select(origin, time_hour, temp, wind_speed),
              by = c("origin", "time_hour")) %>%
    filter(!is.na(dep_delay), !is.na(temp), !is.na(wind_speed))

  names(df_model)[names(df_model) == "hour"] <- "hour_"
  # Esto cambia el nombre de la columna hour a hour_ para evitar conflictos 
  
  head(names(df_model))
  
  mod <- lm(dep_delay ~ distance + hour_ + temp + wind_speed, data = df_model)
  mod
  summary(mod) # hay que ver el p value para ver si son significativos
  
  # Hace un left_join con weather para obtener: Temperatura (temp),Velocidad del viento (wind_speed)
  # usando las columnas en común: origin y time_hour
  # se ve que la HORA afecta mas al retraso de los aviones.
  












