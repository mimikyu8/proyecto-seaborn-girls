
datos <- read.csv("C:/Users/cinti/OneDrive/Escritorio/Proyecto/airbnb_def.v0.csv", sep= ";")

#Comprovamos que se haya cargado correctamente
dim(datos)
nombre_columnas <-colnames(datos)
print(nombre_columnas)

#Renombramos los nombres de las columnas que queramos i comprovamos:

colnames(datos)[colnames(datos)=="ï..ID"] <- "ID"
nombre_columnas <-colnames(datos)
print(nombre_columnas)

#Revisamos el tipo de variables(columnas) que tenemos, los datos introducidos
#i modificamos el tipo de variable segun los datos que contiene
#Comprovamos como ha quedado la variable(que no haya fallos)
#Comprovamos de nuevo el tipo de variable (verificar que se haya cambiado):

class(datos$ID)
datos$ID
datos$ID <- as.numeric(datos$ID)
datos$ID
class(datos$ID)

class(datos$Host.Since)
datos$Host.Since
datos$Host.Since <- as.Date(datos$Host.Since, format = "%d/%m/%y")
datos$Host.Since
class(datos$Host.Since)

class(datos$Host.Response.Rate)
datos$Host.Response.Rate
datos$Host.Response.Rate <- as.numeric(datos$Host.Response.Rate)
datos$Host.Response.Rate
class(datos$Host.Response.Rate)

class(datos$Neighbourhood.Cleansed)
datos$Neighbourhood.Cleansed
datos$Neighbourhood.Cleansed <- as.factor(datos$Neighbourhood.Cleansed)
datos$Neighbourhood.Cleansed
class(datos$Neighbourhood.Cleansed)

class(datos$Neighbourhood.Group.Cleansed)
datos$Neighbourhood.Group.Cleansed
datos$Neighbourhood.Group.Cleansed <- as.factor(datos$Neighbourhood.Group.Cleansed)
datos$Neighbourhood.Group.Cleansed
class(datos$Neighbourhood.Group.Cleansed)

class(datos$City)
datos$City
datos$City <- as.factor(datos$City)
datos$City
class(datos$City)

class(datos$Zipcode)
datos$Zipcode

class(datos$Country)
datos$Country
datos$Country <- as.factor(datos$Country)
datos$Country
class(datos$Country)

class(datos$Latitude)
datos$Latitude

class(datos$Longitude)
datos$Longitude

class(datos$Property.Type)
datos$Property.Type
datos$Property.Type <- as.factor(datos$Property.Type)
datos$Property.Type
class(datos$Property.Type)

class(datos$Room.Type)
datos$Room.Type
datos$Room.Type <- as.factor(datos$Room.Type)
datos$Room.Type
class(datos$Room.Type)

class(datos$Accommodates)
datos$Accommodates
datos$Accommodates <- as.numeric(datos$Accommodates)
datos$Accommodates
class(datos$Accommodates)

class(datos$Bathrooms)
datos$Bathrooms

class(datos$Bedrooms)
datos$Bedrooms
datos$Bedrooms <- as.numeric(datos$Bedrooms)
datos$Bedrooms
class(datos$Bedrooms)

class(datos$Beds)
datos$Beds
datos$Beds <- as.numeric(datos$Beds)
datos$Beds
class(datos$Beds)

class(datos$Bed.Type)
datos$Bed.Type
datos$Bed.Type <- as.factor(datos$Bed.Type)
datos$Bed.Type
class(datos$Bed.Type)

class(datos$Price)
datos$Price
datos$Price <- as.numeric(datos$Price)
datos$Price
class(datos$Price)

class(datos$Security.Deposit)
datos$Security.Deposit
datos$Security.Deposit <- as.numeric(datos$Security.Deposit)
datos$Security.Deposit
class(datos$Security.Deposit)

class(datos$Cleaning.Fee)
datos$Cleaning.Fee
datos$Cleaning.Fee <- as.numeric(datos$Cleaning.Fee)
datos$Cleaning.Fee
class(datos$Cleaning.Fee)

class(datos$Number.of.Reviews)
datos$Number.of.Reviews
datos$Number.of.Reviews <- as.numeric(datos$Number.of.Reviews)
datos$Number.of.Reviews
class(datos$Number.of.Reviews)

class(datos$Review.Scores.Rating)
datos$Review.Scores.Rating
datos$Review.Scores.Rating <- as.numeric(datos$Review.Scores.Rating)
datos$Review.Scores.Rating
class(datos$Review.Scores.Rating)

class(datos$Calculated.host.listings.count)
datos$Calculated.host.listings.count
datos$Calculated.host.listings.count <- as.numeric(datos$Calculated.host.listings.count)
datos$Calculated.host.listings.count
class(datos$Calculated.host.listings.count)

#Analizamos las variables individualmente para ver el numero de respuestas y de NA
#asi como su distribución, por si hay que modificarlas o arreglarlas

#Analizamos los NA de las variables numericas i cremos nuevas variables omitiendolos

install.packages("dplyr")
library(dplyr)

sum(is.na(datos$ID)) #0 Na

#en este caso analizamos si hay valores repetidos, ya que la variable ID
#es una variable de control
any(duplicated(datos$ID)) #No hay valores repetidos = no se incluye el mismo inmueble 2 veces

summary(is.na(datos$Host.Response.Rate)) #1899 Na y 12881 complete

#Como hay muchos NA y es una columna importante, imputamos valores a los NA
summary(datos$Host.Response.Rate)
mean_host_response_rate <- mean(datos$Host.Response.Rate, na.rm = TRUE)
datos$Host.Response.Rate_sinNA <- ifelse(is.na(datos$Host.Response.Rate), mean_host_response_rate, datos$Host.Response.Rate)

#Introducimos los valores de la nueva columna en la columna original
datos$Host.Response.Rate <- NULL
datos$Host.Response.Rate <- datos$Host.Response.Rate_sinNA
datos$Host.Response.Rate_sinNA <- NULL
class(datos$Host.Response.Rate)

#ordenamos los valores
datos$Host.Response.Rate <- sort(datos$Host.Response.Rate)

#Analizamos los principales indicadores
summary(datos$Host.Response.Rate)
sd(datos$Host.Response.Rate) #14
boxplot(datos$Host.Response.Rate)

summary(is.na(datos$Accommodates)) #0 Na
summary(datos$Accommodates)
sd(datos$Accommodates) #2,09

#Ordenamos la variable
datos$Accommodates[!is.na(datos$Accommodates)] <- sort(datos$Accommodates[!is.na(datos$Accommodates)])

summary(is.na(datos$Bathrooms)) #55 Na y 14725 complete
summary(datos$Accommodates)

#Ordenamos la variable
datos$Bathrooms[!is.na(datos$Bathrooms)] <- sort(datos$Bathrooms[!is.na(datos$Bathrooms)])
median(datos$Bathrooms, na.rm =TRUE)

#Creamos una nueva variable sin NA, imputando datos de la mediana
median_bathrooms <- median(datos$Bathrooms, na.rm = TRUE)
datos$Bathrooms_sinNA <- ifelse(is.na(datos$Bathrooms), median_bathrooms, datos$Bathrooms)

#Introducimos los valores de la nueva columna en la columna original
datos$Bathrooms <- NULL
datos$Bathrooms <- datos$Bathrooms_sinNA
datos$Bathrooms_sinNA <- NULL
class(datos$Bathrooms)

##En el caso de los baños, R reconoce los . como decimal
##Si se pasa a separar los decimales con , se tiene que quedar como character
#Porque sino detecta los valores separados con , como NA
#Creamos esta nueva variable separada por , por si en otros momentos
#del analisis es necesaria.

datos$Bathrooms_decimal <- gsub("\\.", ",",datos$Bathrooms)
class(datos$Bathrooms_decimal)


summary(is.na(datos$Bedrooms)) #25 NA y 14755 complete
summary(datos$Bedrooms)
sd(datos$Bedrooms, na.rm = TRUE) #0,9

#Ordenamos la variable
datos$Bedrooms[!is.na(datos$Bedrooms)] <- sort(datos$Bedrooms[!is.na(datos$Bedrooms)])

#Creamos una nueva variable sin NA, imputando datos de la mediana
median_Bedrooms <- median(datos$Bedrooms, na.rm = TRUE)
datos$Bedrooms_sinNA <- ifelse(is.na(datos$Bedrooms), median_Bedrooms, datos$Bedrooms)

#Introducimos los valores de la nueva columna en la columna original
datos$Bedrooms <- NULL
datos$Bedrooms <- datos$Bedrooms_sinNA
datos$Bedrooms_sinNA <- NULL
class(datos$Bedrooms)

summary(is.na(datos$Beds)) #49 NA y 14731 complete
summary(datos$Beds)
sd(datos$Beds, na.rm = TRUE) #1,61
boxplot(datos$Beds)

#Ordenamos la variable
datos$Beds[!is.na(datos$Beds)] <- sort(datos$Beds[!is.na(datos$Beds)])

#Creamos una nueva variable sin NA, imputando datos de la mediana
median_Beds <- median(datos$Beds, na.rm = TRUE)
datos$Beds_sinNA <- ifelse(is.na(datos$Beds), median_Beds, datos$Beds)

#Introducimos los valores de la nueva columna en la columna original
datos$Beds <- NULL
datos$Beds <- datos$Beds_sinNA
datos$Beds_sinNA <- NULL
class(datos$Beds)

#Analizamos el numero de respuestas,de NA y los principales indicadores
summary(is.na(datos$Price)) #17 NA y 1463 complete
summary(datos$Price)
sd(datos$Price, na.rm = TRUE) #desviación de 72 (muy elevada)

#Analizamos los outliers
boxplot(datos$Price) #Outliers a partir de 150€/noche

#Creamos una nueva variable que contenga solo los datos =< a 150
#y analizamos el numero de respues que nos quedaria
#también revisamos de nuevo el boxplot y la desviación estandar
Price_noOutliers <- datos$Price[datos$Price<= 150]
summary(is.na(Price_noOutliers)) #Quedan 13683 casos de 14780 (aceptable)
boxplot(Price_noOutliers)
sd(Price_noOutliers, na.rm = TRUE) #desviación se ha reducido a 32,5

#Ordenamos la variable
datos$Price[!is.na(datos$Price)] <- sort(datos$Price[!is.na(datos$Price)])

#Pasamos los valores >150 a NA y despues los eliminamos
datos$Price_sinNA <- ifelse(datos[, "Price"] > 150, NA, datos[, "Price"])
summary(is.na(datos$Price_sinNA))
datos <- datos[!is.na(datos[, "Price_sinNA"]), ]
dim(datos)

#Introducimos los valores de la nueva columna en la columna original
datos$Price <- NULL
datos$Price <- datos$Price_sinNA
datos$Price_sinNA <- NULL
class(datos$Price)

#Analizamos los principales indicadores
summary(datos$Price)
sd(datos$Price) #32
boxplot(datos$Price)

summary(is.na(datos$Security.Deposit)) #7870 NA y 5813 complete
summary(datos$Security.Deposit)
sd(datos$Security.Deposit, na.rm = TRUE) #114,4
boxplot(datos$Security.Deposit)

#Ordenamos la variable
datos$Security.Deposit[!is.na(datos$Security.Deposit)] <- sort(datos$Security.Deposit[!is.na(datos$Security.Deposit)])

#Imputamos valores 0 a los NA y volvemos a ejecutar los dos puntos anteriores
datos$Security.Deposit[is.na(datos$Security.Deposit)] <- 0

summary(is.na(datos$Security.Deposit)) #7870 NA y 5813 complete
summary(datos$Security.Deposit)
sd(datos$Security.Deposit, na.rm = TRUE) #114,4
boxplot(datos$Security.Deposit)

#Ordenamos la variable
datos$Security.Deposit[!is.na(datos$Security.Deposit)] <- sort(datos$Security.Deposit[!is.na(datos$Security.Deposit)])

summary(is.na(datos$Security.Deposit)) #7870 NA y 5813 complete
summary(datos$Security.Deposit)
sd(datos$Security.Deposit, na.rm = TRUE) #114,4
boxplot(datos$Security.Deposit)

#Ordenamos la variable
datos$Security.Deposit[!is.na(datos$Security.Deposit)] <- sort(datos$Security.Deposit[!is.na(datos$Security.Deposit)])


summary(is.na(datos$Cleaning.Fee)) #5623 NA y 8060 complete
summary(datos$Cleaning.Fee)
sd(datos$Cleaning.Fee, na.rm = TRUE) #29,2
boxplot(datos$Cleaning.Fee)

#Ordenamos la variable
datos$Cleaning.Fee[!is.na(datos$Cleaning.Fee)] <- sort(datos$Cleaning.Fee[!is.na(datos$Cleaning.Fee)])

#Imputamos valores 0 a los NA y volvemos a ejecutar los dos puntos anteriores
datos$Cleaning.Fee[is.na(datos$Cleaning.Fee)] <- 0

summary(is.na(datos$Cleaning.Fee)) #5623 NA y 8060 complete
summary(datos$Cleaning.Fee)
sd(datos$Cleaning.Fee, na.rm = TRUE) #29,2
boxplot(datos$Cleaning.Fee)

#Ordenamos la variable
datos$Cleaning.Fee[!is.na(datos$Cleaning.Fee)] <- sort(datos$Cleaning.Fee[!is.na(datos$Cleaning.Fee)])

summary(is.na(datos$Cleaning.Fee)) #5623 NA y 8060 complete
summary(datos$Cleaning.Fee)
sd(datos$Cleaning.Fee, na.rm = TRUE) #29,2
boxplot(datos$Cleaning.Fee)

#Ordenamos la variable
datos$Cleaning.Fee[!is.na(datos$Cleaning.Fee)] <- sort(datos$Cleaning.Fee[!is.na(datos$Cleaning.Fee)])

#De las variables cleaning fee y Security Deposit creamos dos variables booleanas
#Que contengan 0 y 1 segun si tienen o no esas fianzas

datos$Security.Deposit_bool <- ifelse(datos$Security.Deposit == 0,0,1)
datos$Cleaning.fee_bool <- ifelse(datos$Cleaning.Fee == 0,0,1)

summary(is.na(datos$Number.of.Reviews)) #0 NA 
summary(datos$Number.of.Reviews)
sd(datos$Number.of.Reviews, na.rm = TRUE) #38,04
boxplot(datos$Number.of.Reviews)

#Ordenamos la variable
datos$Number.of.Reviews[!is.na(datos$Number.of.Reviews)] <- sort(datos$Number.of.Reviews[!is.na(datos$Number.of.Reviews)])

summary(is.na(datos$Review.Scores.Rating)) #3054 NA 10629 complete
summary(datos$Review.Scores.Rating)
sd(datos$Review.Scores.Rating, na.rm = TRUE) #9,01
boxplot(datos$Review.Scores.Rating)

#Ordenamos la variable
datos$Review.Scores.Rating[!is.na(datos$Review.Scores.Rating)] <- sort(datos$Review.Scores.Rating[!is.na(datos$Review.Scores.Rating)])

#Imputamos valores 0 a los NA y volvemos a ejecutar los dos puntos anteriores
datos$Review.Scores.Rating[is.na(datos$Review.Scores.Rating)] <- 0

summary(is.na(datos$Calculated.host.listings.count)) #3 NA 13680 complete
summary(datos$Calculated.host.listings.count)
sd(datos$Calculated.host.listings.count, na.rm = TRUE) #23,84
boxplot(datos$Calculated.host.listings.count)

#Ordenamos la variable
datos$Calculated.host.listings.count[!is.na(datos$Calculated.host.listings.count)] <- sort(datos$Calculated.host.listings.count[!is.na(datos$Calculated.host.listings.count)])

#Hacemos una prueba para ver cuantos casos se pierden si eliminamos outliers
Listing <- datos$Calculated.host.listings.count[datos$Calculated.host.listings.count<= 20]
boxplot(Listing)
summary(Listing) #12281 complete, se pierden muchos, la dejamos así

#Creamos una nueva variable sin NA, imputando datos de la media redondeada
media_list <- round(mean(datos$Calculated.host.listings.count, na.rm = TRUE))
datos$Calculated.host.listings.count_med <- ifelse(is.na(datos$Calculated.host.listings.count), media_list, datos$Calculated.host.listings.count)

#Introducimos los valores de la nueva columna en la columna original
datos$Calculated.host.listings.count <- NULL
datos$Calculated.host.listings.count <- datos$Calculated.host.listings.count_med
datos$Calculated.host.listings.count_med <- NULL
class(datos$Calculated.host.listings.count)

summary(datos$Calculated.host.listings.count)
sd(datos$Calculated.host.listings.count, na.rm = TRUE) #23,84
boxplot(datos$Calculated.host.listings.count)

#Variable Character

summary(is.na(datos$Zipcode)) #721 Na y 14059 complete
#Esta variable servirà para la representación gràfica, pero tiene pocos NA

summary(is.na(datos$Latitude)) #0 Na's

summary(is.na(datos$Longitude)) #0 Na's

#Variable data
summary(is.na(datos$Host.Since)) #2NA y 113681 complete
summary(datos$Host.Since)
boxplot(datos$Host.Since)

#Eliminamos esos 2 casos que no contienen fechas
datos <- datos[!is.na(datos$Host.Since), ]

#Ordenamos la variable
datos$Host.Since <- sort(datos$Host.Since, decreasing = FALSE)

#Creamos una nueva variable que solo contenga el año
datos$ano_Host_Since <- as.numeric(substr(datos$Host.Since, 1, (regexpr("-", datos$Host.Since)-1)))
class(datos$ano_Host_Since)

#Variables factor

table(datos$Country)
levels(datos$Country)

#Eliminamos las filas que pertencen a otro país y no nos interesan
datos <- datos[datos$Country %in% "Spain", ]
datos$Country <- droplevels(datos$Country)

#Comprobamos cambios
table(datos$Country)
levels(datos$Country)


table(datos$City)
levels(datos$City)

#Normalizamos los valores correspondientes a Madrid
datos$City <- as.factor(recode(toupper(datos$City), Madid = "Madrid", Madri = "Madrid", madrid = "Madrid", MADRID = "Madrid"))
datos$City <- as.factor(recode(toupper(datos$City), MADID = "Madrid", MADRI = "Madrid"))
datos$City <- as.factor(sub("^([^,]+).*", "\\1", datos$City))

#Eliminamos las filas que pertencen a otra ciudad y no nos interesan
datos <- datos[datos$City %in% "MADRID", ]
datos$City <- droplevels(datos$City)

table(datos$Neighbourhood.Cleansed)
levels(datos$Neighbourhood.Cleansed)
sum(is.na(datos$Neighbourhood.Cleansed))

#eliminamos las categorias con 0 registros
datos$Neighbourhood.Cleansed <- droplevels(datos$Neighbourhood.Cleansed)
levels(datos$Neighbourhood.Cleansed)

#Normalizamos los valores 
datos$Neighbourhood.Cleansed <- as.factor(recode(toupper(datos$Neighbourhood.Cleansed),
                                                 "ARGÃ¼ELLES" = 'ARGUELLES',
                                                 "CÃ¡RMENES" = 'CARMENES',
                                                 "CASCO HISTÃ³RICO DE BARAJAS" = 'CASCO HISTORICO DE BARAJAS',
                                                 "CASCO HISTÃ³RICO DE VALLECAS" = 'CASCO HISTORICO DE VALLECAS',
                                                 "CASCO HISTÃ³RICO DE VICÃ¡LVARO" = 'CASCO HISTORICO DE VICALVARO',
                                                 "CIUDAD JARDÃ­N" = 'CIUDAD JARDIN',
                                                 "CONCEPCIÃ³N" = 'CONCEPCION',
                                                 "EL PLANTÃ­O" = 'EL PLANTIO',
                                                 "ENTREVÃ­AS" = 'ENTREVIAS',
                                                 "FONTARRÃ³N" = 'FONTARRON',
                                                 "HELLÃ­N" = 'HELLIN',
                                                 "HISPANOAMÃ©RICA" = 'HISPANOAMERICA',
                                                 "JERÃ³NIMOS" = 'JERONIMOS',
                                                 "MOSCARDÃ³" = 'MOSCARDO',
                                                 "NIÃ±O JESÃºS" = 'NINO JESUS',
                                                 "NUEVA ESPAÃ±A" = 'NUEVA ESPANA',
                                                 "OPAÃ±EL" = 'OPANEL',
                                                 "PACÃ­FICO" = 'PACIFICO',
                                                 "PEÃ±AGRANDE" = 'PENAGRANDE',
                                                 "SAN ANDRÃ©S" = 'SAN ANDRES',
                                                 "SAN FERMÃ­N" = 'SAN FERMIN',
                                                 "TIMÃ³N" = 'TIMON',
                                                 "VALDEMARÃ­N" = 'VALDEMARIN',
                                                 "ZOFÃ­O" = 'ZOFIO'))



levels(datos$Neighbourhood.Cleansed)

#ordenamos la variable por orden alfabetico --> despues de pasarla a numerica
#datos$Neighbourhood.Cleansed <- reorder(datos$Neighbourhood.Cleansed, datos$Neighbourhood.Cleansed, FUN=sort)

#Pasar la variable a numerica
datos$Neighbourhood.Cleansed_num <- as.numeric(recode(datos$Neighbourhood.Cleansed, "ABRANTES" = 1,
                                                                                    "ACACIAS" = 2, 
                                                                                    "ADELFAS" = 3,
                                                                                    "AEROPUERTO" = 4,
                                                                                    "AGUILAS" = 5,
                                                                                    "ALAMEDA DE OSUNA"= 6,
                                                                                    "ALMAGRO" = 7,
                                                                                    "ALMENARA" = 8,
                                                                                    "ALMENDRALES" = 9,
                                                                                    "ALUCHE" = 10,
                                                                                    "AMBROZ" = 11,
                                                                                    "AMPOSTA" = 12,
                                                                                    "APOSTOL SANTIAGO" = 13,
                                                                                    "ARAPILES" = 14,
                                                                                    "ARAVACA" = 15,
                                                                                    "ARCOS" = 16,
                                                                                    "ARGUELLES" = 17,
                                                                                    "ATOCHA" = 18,
                                                                                    "BELLAS VISTAS" = 19,
                                                                                    "BERRUGUETE" = 20,
                                                                                    "BUENAVISTA" = 21,
                                                                                    "BUTARQUE" = 22,
                                                                                    "CAMPAMENTO" = 23,
                                                                                    "CANILLAS" = 24,
                                                                                    "CANILLEJAS" = 25,
                                                                                    "CARMENES" = 26,
                                                                                    "CASA DE CAMPO" = 27,
                                                                                    "CASCO HISTORICO DE BARAJAS" = 28,
                                                                                    "CASCO HISTORICO DE VALLECAS" = 29,
                                                                                    "CASCO HISTORICO DE VICALVARO" = 30,
                                                                                    "CASTELLANA" = 31,
                                                                                    "CASTILLA" = 32,
                                                                                    "CASTILLEJOS" = 33,
                                                                                    "CHOPERA" = 34,
                                                                                    "CIUDAD JARDIN" = 35,
                                                                                    "CIUDAD UNIVERSITARIA" = 36,
                                                                                    "COLINA" = 37,
                                                                                    "COMILLAS" = 38,
                                                                                    "CONCEPCION" = 39,
                                                                                    "CORRALEJOS" = 40,
                                                                                    "CORTES" = 41,
                                                                                    "COSTILLARES" = 42,
                                                                                    "CUATRO CAMINOS" = 43,
                                                                                    "CUATRO VIENTOS" = 44,
                                                                                    "DELICIAS" = 45,
                                                                                    "EL GOLOSO" = 46,
                                                                                    "EL PLANTIO" = 47,
                                                                                    "EL VISO" = 48,
                                                                                    "EMBAJADORES" = 49,
                                                                                    "ENTREVIAS" = 50,
                                                                                    "ESTRELLA" = 51,
                                                                                    "FONTARRON" = 52,
                                                                                    "FUENTE DEL BERRO" = 53,
                                                                                    "FUENTELAREINA" = 54,
                                                                                    "GAZTAMBIDE" = 55,
                                                                                    "GOYA" = 56,
                                                                                    "GUINDALERA" = 57,
                                                                                    "HELLIN" = 58,
                                                                                    "HISPANOAMERICA" = 59,
                                                                                    "IBIZA" = 60,
                                                                                    "IMPERIAL" = 61,
                                                                                    "JERONIMOS" = 62,
                                                                                    "JUSTICIA" = 63,
                                                                                    "LA PAZ" = 64,
                                                                                    "LEGAZPI" = 65,
                                                                                    "LISTA" = 66,
                                                                                    "LOS ANGELES" = 67,
                                                                                    "LOS ROSALES" = 68,
                                                                                    "LUCERO" = 69,
                                                                                    "MARROQUINA" = 70,
                                                                                    "MEDIA LEGUA" = 71,
                                                                                    "MIRASIERRA" = 72,
                                                                                    "MOSCARDO" = 73,
                                                                                    "NINO JESUS" = 74,
                                                                                    "NUEVA ESPANA" = 75,
                                                                                    "NUMANCIA"  = 76,
                                                                                    "OPANEL" = 77,
                                                                                    "ORCASITAS" = 78,
                                                                                    "ORCASUR" = 79,
                                                                                    "PACIFICO" = 80,
                                                                                    "PALACIO" = 81,
                                                                                    "PALOMAS" = 82,
                                                                                    "PALOMERAS BAJAS" = 83,
                                                                                    "PALOMERAS SURESTE" = 84,
                                                                                    "PALOS DE MOGUER" = 85,
                                                                                    "PAVONES" = 86,
                                                                                    "PENAGRANDE" = 87,
                                                                                    "PILAR" = 88,
                                                                                    "PINAR DEL REY" = 89,
                                                                                    "PIOVERA" = 90,
                                                                                    "PORTAZGO" = 91,
                                                                                    "PRADOLONGO" = 92,
                                                                                    "PROSPERIDAD" = 93,
                                                                                    "PUEBLO NUEVO" = 94,
                                                                                    "PUERTA BONITA" = 95,
                                                                                    "PUERTA DEL ANGEL" = 96,
                                                                                    "QUINTANA" = 97,
                                                                                    "RECOLETOS" = 98,
                                                                                    "REJAS" = 99,
                                                                                    "RIOS ROSAS" = 100,
                                                                                    "ROSAS" = 101,
                                                                                    "SALVADOR" = 102,
                                                                                    "SAN ANDRES" = 103,
                                                                                    "SAN CRISTOBAL" = 104,
                                                                                    "SAN DIEGO" = 105,
                                                                                    "SAN FERMIN" = 106,
                                                                                    "SAN ISIDRO" = 107,
                                                                                    "SAN JUAN BAUTISTA" = 108,
                                                                                    "SAN PASCUAL" = 109,
                                                                                    "SANTA EUGENIA" = 110,
                                                                                    "SIMANCAS" = 111,
                                                                                    "SOL" = 112,
                                                                                    "TIMON" = 113,
                                                                                    "TRAFALGAR" = 114,
                                                                                    "UNIVERSIDAD" = 115,
                                                                                    "VALDEACEDERAS" = 116,
                                                                                    "VALDEFUENTES" = 117,
                                                                                    "VALDEMARIN" = 118,
                                                                                    "VALDEZARZA" = 119,
                                                                                    "VALLEHERMOSO" = 120,
                                                                                    "VALVERDE" = 121,
                                                                                    "VENTAS" = 122,
                                                                                    "VINATEROS" = 123,
                                                                                    "VISTA ALEGRE" = 124,
                                                                                    "ZOFIO" = 125))
                                                                                                        
class(datos$Neighbourhood.Cleansed_num)


table(datos$Neighbourhood.Group.Cleansed)
levels(datos$Neighbourhood.Group.Cleansed)
sum(is.na(datos$Neighbourhood.Group.Cleansed))

#eliminamos las categorias con 0 registros
datos$Neighbourhood.Group.Cleansed <- droplevels(datos$Neighbourhood.Group.Cleansed)
levels(datos$Neighbourhood.Group.Cleansed)

#Normalizamos los valores 
datos$Neighbourhood.Group.Cleansed <- as.factor(recode(toupper(datos$Neighbourhood.Group.Cleansed),
                                                 "CHAMARTÃ­N" = 'CHAMARTIN',
                                                 "CHAMBERÃ­" = 'CHAMBERI',
                                                 "TETUÃ¡N" = 'TETUAN',
                                                 "VICÃ¡LVARO" = 'VICALVARO'))


levels(datos$Neighbourhood.Group.Cleansed)

#Pasar la variable a numerica
datos$Neighbourhood.Group.Cleansed_num <- as.numeric(recode(datos$Neighbourhood.Group.Cleansed, "ARGANZUELA" = 1,
                                                      "BARAJAS" = 2, 
                                                      "CARABANCHEL" = 3,
                                                      "CENTRO" = 4,
                                                      "CHAMARTIN" = 5,
                                                      "CHAMBERI" = 6,
                                                      "CIUDAD LINEAL" = 7,
                                                      "FUENCARRAL - EL PARDO" = 8,
                                                      "HORTALEZA" = 9,
                                                      "LATINA" = 10,
                                                      "MONCLOA - ARAVACA" = 11,
                                                      "MORATALAZ" = 12,
                                                      "PUENTE DE VALLECAS" = 13,
                                                      "RETIRO" = 14,
                                                      "SALAMANCA" = 15,
                                                      "SAN BLAS - CANILLEJAS" = 16,
                                                      "TETUAN" = 17,
                                                      "USERA" = 18,
                                                      "VICALVARO" = 19,
                                                      "VILLA DE VALLECAS" = 20,
                                                      "VILLAVERDE" = 21,))



table(datos$Property.Type)
levels(datos$Property.Type)
sum(is.na(datos$Property.Type))
datos$Property.Type <- as.factor(toupper(datos$Property.Type))

#Agrupar las categorias con poca representación
datos$Property.Type_Rec <- recode(datos$Property.Type, 
                                "CASA PARTICULAR" = "HOUSE", 
                                "VILLA" = "HOUSE", 
                                "SERVICED APARTMENT" = "APARTMENT",
                                "BOAT" = "OTHER",
                                "BOUTIQUE HOTEL" = "OTHER",
                                "BUNGALOW" = "OTHER",
                                "CAMPER/RV" = "OTHER",
                                "EARTH HOUSE" = "OTHER",
                                "GUEST SUITE" = "OTHER",
                                "TENT" = "OTHER",
                                "TIMESHARE" = "OTHER",
                                "TOWNHOUSE" = "OTHER")

levels(datos$Property.Type_Rec)
table(datos$Property.Type_Rec)
datos$Property.Type_Rec <- as.factor(toupper(datos$Property.Type_Rec))

#Pasar la variable a numerica
datos$Property.Type_RecNum <- as.numeric(recode(datos$Property.Type_Rec, "APARTMENT" = 1,
                                                            "BED & BREAKFAST" = 2, 
                                                            "OTHER" = 3,
                                                            "HOUSE" = 4,
                                                            "CHALET" = 5,
                                                            "CONDOMINIUM"  = 6,
                                                            "DORM" = 7,
                                                            "GUESTHOUSE" = 8,
                                                            "HOSTEL"  = 9,
                                                            "LOFT" = 10))

table(datos$Property.Type_RecNum)

table(datos$Room.Type)
levels(datos$Room.Type)
sum(is.na(datos$Room.Type))
datos$Room.Type <- as.factor(toupper(datos$Room.Type))

#pasar a numerica
datos$Room.Type_Num <- as.numeric(recode(datos$Room.Type, "ENTIRE HOME/APT" = 1,
                                                "PRIVATE ROOM" = 2, 
                                                "SHARED ROOM" = 3))
                                              
table(datos$Room.Type_Num)


table(datos$Bed.Type)
levels(datos$Bed.Type)
sum(is.na(datos$Bed.Type))
datos$Bed.Type <- as.factor(toupper(datos$Bed.Type))

#Agrupamos las categorias
datos$Bed.Type_REC <- as.factor(recode(toupper(datos$Bed.Type),
                                                       "AIRBED" = 'REAL BED',
                                                       "COUCH" = 'SOFA OR SIMILAR',
                                                       "FUTON" = 'SOFA OR SIMILAR',
                                                       "PULL-OUT SOFA" = 'SOFA OR SIMILAR'))
levels(datos$Bed.Type_REC)

#Pasar a numerica
datos$Bed.Type_ReCNum <- as.numeric(recode(datos$Bed.Type_REC, "REAL BED" = 1,
                                         "SOFA OR SIMILAR" = 2))

table(datos$Bed.Type_ReCNum)


#Exportamos la matriz en formato csv
write.csv(datos, file = "airbnb_def.csv")

#Hacemos la matriz de correlaciones para ver las correlaciones entre las variables
datos[, -which(names(datos) == "Price")] <- lapply(datos[, -which(names(datos) == "Price")], as.numeric)
cor(datos[, -which(names(datos) == "Price")], datos$Price)

#Realizamos gràficos para visualizar la relacion entre la variable Price
#y las variables con más correlación
library(ggplot2)

# Creamos el scatterplot

ggplot(datos, aes(x = Accommodates, y = Price, color = Accommodates)) +
  geom_point()

ggplot(datos, aes(x = ano_Host_Since, y = Price, color = ano_Host_Since)) +
  geom_point()

ggplot(datos, aes(x = Number.of.Reviews, y = Price, color = Number.of.Reviews)) +
  geom_point()

ggplot(datos, aes(x = Bedrooms, y = Price, color = Bedrooms)) +
  geom_point()

ggplot(datos, aes(x = Calculated.host.listings.count, y = Price, color = Calculated.host.listings.count)) +
  geom_point()



