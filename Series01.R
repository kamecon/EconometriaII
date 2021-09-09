library(tidyverse)

# Proceso estocástico y serie temporal ------------------------------------

#Caso particular
phi= -0.7048          
z=rep(0,100)                                 
e=rnorm(n=100,0,0.7)                       
cons=2.1                                     
z[1]=4.1
for (i in 2:100) z[i]=cons+phi*z[i-1]+e[i]   
plot(ts(z), col=2)  

#Caso general

## Parámetros
phi= -0.7048                                 
y=rep(0,100)
cons=2.1                                     
y[1]=4.1

#--Creamos un data frame vacio que luego rellenamos con distintas realizaciones de series temporales
datos <- matrix(0, nrow = 100, ncol = 10) %>% data.frame()

for (i in 1:10) {
  datos[1,i]=4.1
  e=rnorm(n=100,0,0.7) 
  for (j in 2:100) datos[j,i]=cons+phi*datos[j-1,i]+e[j]   
}

#--Ejemplo con 3 realizaciones
plot(datos[,1] %>% ts(), col=2, lwd=3, main="Proceso Estocástico", xlab="Tiempo", ylab="Serie")
lines(datos[,2] %>% ts(), col=3)
lines(datos[,3] %>% ts(), col=4)

#--Ejemplo con 10 realizaciones
plot(datos[,1] %>% ts(), col=2, lwd=5, main="Proceso Estocástico", xlab="Tiempo", ylab="Serie")
for (i in 2:10) {
  lines(datos[,i] %>% ts(), col=i+1)
}

sample(x = datos, size = 1) %>% head()

#Ruido blanco (white noise)

w = rnorm(500,0,1)

plot(w %>% ts, main="Ruido Blanco", xlab="Tiempo", ylab="Serie")

#Con menos datos para que se vea mejor

w = rnorm(100,0,1)

plot(w %>% ts, main="Ruido Blanco", xlab="Tiempo", ylab="Serie", type="b", col=2)

#Media movil

v = stats::filter(w, sides=2, rep(1/3,3))

plot(v %>% ts, main="Media Móvil", xlab="Tiempo", ylab="Serie", type="b", col=2)

##Ambos procesos

par(mfrow=c(2,1))
plot(w %>% ts, main="Ruido Blanco", xlab="Tiempo", ylab="Serie", type="b", col=2)
plot(v %>% ts, main="Media Móvil", xlab="Tiempo", ylab="Serie", type="b", col=2)

w = rnorm(500,0,1)
v = stats::filter(w, sides=2, rep(1/3,3))

par(mfrow=c(2,1))
plot(w %>% ts, main="Ruido Blanco", xlab="Tiempo", ylab="Serie", type="l", col=2)
plot(v %>% ts, main="Media Móvil", xlab="Tiempo", ylab="Serie", type="l", col=4)

#Random walk (Paseo aleatorio)

## set random number seed
set.seed(123)
## length of time series
TT <- 100
## initialize {x_t} and {w_t}
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)
## compute values 2 thru TT
for (t in 2:TT) {
  xx[t] <- xx[t - 1] + ww[t]
}

## plot line
plot(xx %>% ts, type="b" ,ylab = expression(italic(x[t])))

###Datos Entsoe (Demanda electrica) ------------------------

pacman::p_load(entsoeapi)


#Seleccionamos los paises. Creamos dos vectores: uno con el nombre de los paises y otro con los códigos usados por Entsoe

paises <- en_eic() %>% 
  filter(AreaTypeCode == "CTY" & AreaName %in% c("Spain")) %>% 
  select(AreaCode, AreaName)

paises_cod <- paises %>% 
  pull(1)

paises_name <- paises %>% 
  pull(2)


#Creamos una lista para rellenar con la información de los paises

EntsoePaises <- list()

#Aplicamos un bucle (loop) para descargar los datos de Entsoe y guardarlos en la lista

for (i in seq_along(paises_cod)) {
  
  EntsoePaises[[paises_name[i]]] <- en_load_actual_total_load(eic = paises_cod[i],
                                                              period_start = lubridate::ymd(as.character(Sys.Date() - 370), tz = "UTC"),
                                                              period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC"),
                                                              security_token = "cd15b9e9-ac50-4a2e-b949-1fe1583859f7"
  )
  
}

#Guardamos los datos para poder acceder a ellos luego

save(EntsoePaises, file = "EntsoePaises.RData")

