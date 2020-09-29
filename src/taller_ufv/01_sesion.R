#' 
#' MÓDULO DE INTRODUCCIÓN A R
#' 
#' Minería de datos I - UFV
#' 
#' pablo.hidalgo@ufv.es
#' 


# LECTURA DE DATOS --------------------------------------------------------

#' El conjunto de datos tiene como extensión .csv. 
#' El formato CSV (comma-separated values) es un tipo
#' de documento en formato abierto sencillo para
#' representar datos en forma de tabla en las que las
#' columnas se separan, habitualmente, por comas y las filas
#' por saltos de línea.
#' 
#' PASO 1. Abre el archivo .csv pulsando sobre él en la pestaña "files".

#' Para trabajar con el archivo desde R, es necesario importarlo
#' a la memoria del programa

spain <- read.csv("data/taller_ufv/spain.csv", stringsAsFactors = FALSE)



# ESTRUCTURA DEL CONJUNTO DE DATOS ----------------------------------------


#' Podemos ver los datos de dos formas:
#'   1. Escribiendo en la consola el nombre del data frame.

spain

#'   2. Escribiendo el comando View()

View(spain)

#'   
#' Para ver rápidamente la estructura del data frame, 
#' podemos pedir a R que nos enseñe las primeras filas

head(spain)

#' Aunque este conjunto de datos tiene pocas columnas y
#' se pueden ver fácilmente, esto no siempre tiene por qué ser así.
#' A veces podemos tener cientos de columnas.
#' Podemos ver el nombre de las columnas:

names(spain)

#' Para saber el volumen de información que manejamos, 
#' es útil conocer el número de filas y el número de columnas.

nrow(spain)
ncol(spain)
dim(spain)

#' Mismo resultado

nrow(spain)
dim(spain)[1]

ncol(spain)
dim(spain)[2]

#' Como vemos a simple vista, el conjunto de datos
#' recoge el número de casos confirmados y de fallecimientos
#' por coronavirus en España a lo largo del tiempo.
#' Cabe preguntarse: ¿desde cuándo hasta cuándo tenemos datos?

#' Podemos acceder a una variable como

spain$date

#' y a un valor particular

spain$date[25]


#' Y podemos hacer operaciones sobre ella. Por ejemplo, 
#' el valor mínimo de la fecha

min(spain$date)

#' Nos preguntamos si esta variable está almacenada con su tipo adecuado
typeof(spain$date)

#' Ya que es una fecha, lo ideal es que tenga este formato.
#' Podemos convertir su formato como

spain$date <- as.Date(spain$date)
spain$date

#' La ventaja es que podemos hacer operaciones

spain$date[1]
spain$date[1] + 1

#' EJERCICIO 1: ¿Cuál es el número total de casos confirmados?¿Y de fallecidos?
#' 

sum(spain$confirmed)
sum(spain$deaths)

#' ¿Son razonables los resultados?
#' 
#' Sabemos por nuestra experiencia que el número de fallecidos es superior a 0.
#' Si te fijas en la columna spain$deaths, hay datos negativos, lo cual es imposible.
#' 

#' Como esta variable debe contener errores, vamos a eliminarla 
#' de nuestro conjunto de datos.

spain$deaths <- NULL

spain

#' R está preparado para trabajar de forma vectorial por lo que podemos
#' hacer operaciones sobre vectores de forma fácil.
#' Por ejemplo, queremos saber si hay datos de la variable confirmed
#' menores que 0.
#' 

spain$confirmed < 0

# Muesta los valores menores que 0
spain$confirmed[spain$confirmed < 0]

# Qué posición ocupan
which(spain$confirmed < 0)

#' De nuevo, es imposible que los casos confirmados sean negativos.
#' 
#' Vamos a hacer lo siguiente:
#'   1. Creamos una variable confirmed2 igual que confirmed

spain$confirmed2 <- spain$confirmed
spain

#'   2. Aquellos valores negativos de la variable confirmed2,
#'      los intercambiamos por 0.

spain$confirmed2[spain$confirmed2 < 0] <- 0

#' EJERCICIO. Crea una nueva variable confirmed_acum que sea la suma 
#' acumulada de la variable confirmed2. Para ello utiliza la función
#' cumsum() (recuerda que puedes consultar la ayuda escribiendo ?cumsum).

spain$confirmed_acum <- cumsum(spain$confirmed2)


# GRÁFICOS ----------------------------------------------------------------

#' Una parte central de la minería de datos es entender bien el conjunto
#' de datos con el que estamos trabajando.
#' Para ello, algo fundamental es saber hacer gráficos.
#' 
#' Hay varios paquetes para hacer gráficos en R, 
#' nosotros vamos a utilizar ggplot2.
#' 
#' Si no has usado antes este paquete necesitarás instalarlo primero.
#' Tenemos dos formas:
#'     1. Ve a la pestaña "Packages", pulsa sobre "Install" y escribe ggplot2.
#'     2. Ejecuta en la consola: install.packages("ggplot2")
#'     
#' NOTA: esto solo es necesario hacerlo la primera vez; este paquete se
#' quedará instalado en nuestro ordenador.
#' 

# Cargamos el paquete para poder utilizarlo

library(ggplot2)

# Veamos un ejemplo de cómo utilizarlo

ggplot(data = spain, aes(x = date, y = confirmed2)) +
  geom_line()

#' EJERCICIO: discute las ideas que puedes obtener viendo el gráfico.

#' EJERCICIO: repite el gráfico anterior pero esta vez 
#' utilizando la variable confirmed. 
#' ¿Crees que hemos hecho bien en eliminar los valores negativos?

#' Para hacer un gráfico con ggplot2, siempre comenzamos escribiendo
#' la función ggplot() y después vamos añadiendo capas con +.
#' Los elementos geométricos que podemos utilizar siempre empiezan por geom_

ggplot(data = spain, aes(x = date, y = confirmed2)) +
  geom_point()

ggplot(data = spain, aes(x = date, y = confirmed2)) +
  geom_line() +
  geom_point()

#' Para estos datos, ¿qué gráfico crees que tiene más sentido de los que hemos hecho?

#' Si te fijas en el eje de la izquierda, puede ser algo complicado leer tantos 0.
#' Podemos expresar los casos confirmados en miles.
#' 

spain$confirmed2 <- spain$confirmed2/1000

#' Además, si el propósito de nuestro gráfico es compartirlo,
#' lo ideal es que tenga un título que ayude a entender qué se
#' está representando.

ggplot(data = spain, aes(x = date, y = confirmed2)) +
  geom_line() +
  labs(
    title = "Casos confirmados por coronavirus en España",
    subtitle = "Miles de casos"
  )

#' Si te fijas en los datos a paritr de julio, presentan una restructura curiosa.
#' Se debe a cómo comunican los datos las comunidades (por ejemplo, en fin de semana no se publican).
#' 
#' Parece tener sentido agrupar las observaciones por semana.
#' Nos ayudaremos del paquete lubridate (recuerda que debes instalarlo primero)

spain$week <- lubridate::week(spain$date)

#' Vamos a agregar los datos utilizando un bucle:

# Ejemplo

sum(spain$confirmed2[spain$week == 36])

conf_week <- c()

for (w in unique(spain$week)) {
  conf_week <- c(conf_week, sum(spain$confirmed2[spain$week == w]))
}

spain_week <- data.frame(country = "Spain",
                         week = unique(spain$week), 
                         confirmed = aggr_semanas,
                         stringsAsFactors = FALSE)

ggplot(data = spain_week, aes(x = week, y = confirmed)) +
  geom_line() +
  labs(
    title = "Casos confirmados por coronavirus en España",
    subtitle = "Miles de casos"
  )

#' ¿A qué crees que puede deberse la caída al final del gráfico?
#' 

spain_week <- spain_week[spain_week$week < max(spain_week$week), ]

ggplot(data = spain_week, aes(x = week, y = confirmed)) +
  geom_line() +
  labs(
    title = "Casos confirmados por coronavirus en España",
    subtitle = "Miles de casos",
    x = "Semana",
    y = ""
  )

#' EJERCICIO. Busca en la ayuda la función geom_area() y utilízala en el gráfico.
#' Busca cómo cambiar el color del área por "orange3".
#' 

ggplot(data = spain_week, aes(x = week, y = confirmed)) +
  geom_area(fill = "orange3") +
  labs(
    title = "Casos confirmados por coronavirus en España",
    subtitle = "Miles de casos",
    x = "Semana",
    y = ""
  )

# Vamos a agregar datos de Italia para poder compararnos

italy_week <- read.csv("data/taller_ufv/italy_week.csv", stringsAsFactors = FALSE)

countries_week <- rbind(spain_week, italy_week)

ggplot(data = countries_week, aes(x = week, y = confirmed, group = country, color = country)) +
  geom_line() +
  labs(
    title = "Casos confirmados por coronavirus en España",
    subtitle = "Miles de casos",
    x = "Semana",
    y = ""
  )

#' Para hacer una comparación más justa, podemos hacer la comparación por cada 1000 habitantes


population_italy <- 60541000
population_spain <- 47329981

countries_week$confirmed <- ifelse(countries_week$country == "Spain", 
                                   countries_week$confirmed*1000/population_spain*100000,
                                   countries_week$confirmed*1000/population_italy*100000)

ggplot(data = countries_week, aes(x = week, y = confirmed, group = country, color = country)) +
  geom_line() +
  labs(
    title = "Casos confirmados por coronavirus en España",
    subtitle = "Miles de casos",
    x = "Semana",
    y = ""
  )

# EJERCICIO. Agrega los datos de Portugal

population_portugal <- 10562178

portugal_week <- read.csv("data/taller_ufv/portugal_week.csv", stringsAsFactors = FALSE)

countries_week <- rbind(countries_week, portugal_week)

countries_week$confirmed <- ifelse(countries_week$country == "Portugal", 
                                   countries_week$confirmed*1000/population_portugal*100000,
                                   countries_week$confirmed)

ggplot(data = countries_week, aes(x = week, y = confirmed, group = country, color = country)) +
  geom_line() +
  labs(
    title = "Casos confirmados semanales por coronavirus\nen España, Italia y Portugal",
    subtitle = "Por cada 100.000 habitantes",
    x = "Semana",
    y = ""
  )

#' EJERCICIO. Ya que el título está en castellano, nos gustaría que
#' aparezca España, Italia y Portugal. Busca la forma de hacerlo.

countries_week$country[countries_week$country == "Spain"] <- "España"
countries_week$country[countries_week$country == "Italy"] <- "Italia"

ggplot(data = countries_week, aes(x = week, y = confirmed, group = country, color = country)) +
  geom_line() +
  labs(
    title = "Casos confirmados semanales por coronavirus\nen España, Italia y Portugal",
    subtitle = "Por cada 100.000 habitantes",
    x = "Semana",
    y = ""
  )
