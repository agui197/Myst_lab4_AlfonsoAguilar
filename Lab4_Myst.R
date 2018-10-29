# -- Borrar todos los elementos del environment
rm(list=ls())
mdir <- getwd()




################################

# Import data
(library(readxl))
data=read.csv('data.csv')

# Filter data
indicators=array(unique(data$Name))
filter=data[data$Name==indicators[2],1:7]

################################

# Clasification
a=filter$Actual>=filter$Consensus & filter$Consensus>=filter$Previous
b=filter$Actual>=filter$Consensus & filter$Consensus<filter$Previous
c=filter$Actual<filter$Consensus & filter$Consensus>=filter$Previous
d=filter$Actual<=filter$Consensus & filter$Consensus<filter$Previous

t=data.frame("Name"=filter$Name,"Class"=0)
t$Class[a]=1
t$Class[b]=2
t$Class[c]=3
t$Class[d]=4

data=data.frame(append(filter,t))

#



# -- Establecer el sistema de medicion de la computadora
Sys.setlocale(category = "LC_ALL", locale = "")

# -- Huso horario
Sys.setenv(tz="America/Monterrey", TZ="America/Monterrey")
options(tz="America/Monterrey", TZ="America/Monterrey")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 17
# Uso horario
OA_Ta <- "America/Mexico_City"
# Instrumento
OA_In <- "EUR_USD"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
# S5, S10, S30, M1, M5, M15, M30, H1, H4, H8, D, M
OA_Pr <- "M1"
# Multiplicador de precios para convertir a PIPS
MultPip_MT1 <- 10000

# Esta se obtiene de cada observacion en el archivo xls que descargas. Este fue
# un ejemplo de una fecha que podrias leer de tal archivo una vez que lo descargas
Fecha_Ejemplo <- "10/01/2018 14:00:00"

# Opcion 1 para convertir a "YYYY-MM-DD"
F1 <- as.Date(strptime(x = Fecha_Ejemplo, format = "%m/%d/%Y %H:%M:%S",tz = "GMT"))

# Opcion 2 para convertir a "YYYY-MM-DD"
F2 <- as.Date(substr(Fecha_Ejemplo,1 ,10),format = "%m/%d/%Y")

# Ejemplo Si el comunicado fue en domingo, que pida precios desde viernes a domingo
F3 <- as.Date("2018-09-30")

# Condicion por si acaso toca en domingo el comunicado, no se podria restar un dia y 
# que terminara siendo sabado, porque ese dia no hay precios.
if(wday(F3) != 1){
  
  Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                             DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                             Instrument = OA_In, 
                             Start = F1-1, End = F1, Count = NULL)
} else {
  Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                             DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                             Instrument = OA_In, 
                             Start = F3-2, End = F1, Count = NULL)
}

# Para convertir las fechas del xlsx con historico de indicadores al mismo formato
# de fecha que entrega Oanda los precios
fecha <- as.character(as.POSIXct(Fecha_Ejemplo, format = "%m/%d/%Y %H:%M:%S"))

# Para encontrar el registro donde coincide la fecha y hora del comunicado de 
# indicador y los historicos de precios descargados
ind <- which(Precios_Oanda$TimeStamp == fecha)

# Para visualizar un data frame con los 31 precios deseados. 15 antes del comunicad, 1 
# durante el comunicado y 15 después del comunicado
df <- Precios_Oanda[(ind-15):(ind+15),]