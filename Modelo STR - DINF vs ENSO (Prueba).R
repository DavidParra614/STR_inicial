##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________
#                           BANCO DE LA REPÚBLICA
#                           UNIDAD DE ECONOMETRÍA
##                MODELO STR INFLACIÓN ALIMENTOS VS ENSO
##                      David Steven Parra Almeyda 
##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________

#CARGA Y DESCARGA DE PAQUETES---------------------------------------------------

install.packages("pacman")

pacman :: p_load(
  vars,         # Para usar modelos VAR
  urca,         # Para realizar pruebas de raíz unitaria
  tidyverse,    # Paquete que incluye ggplot2 y dplyr
  ggfortify,    # Para graficar series de tiempo
  gridExtra,    # Para concatenar gráficas en un solo plot
  forecast,     # Para hacer pronóstico con los modelos ARIMA
  lmtest,       # Obtener la significancia individual de los coeficientes ARIMA
  urca,         # Prueba de raíz unitaria (estacionariedad)
  tseries,      # Para estimar modelos de series de tiempo y pruebas de supuestos 
  readxl,       # Para leer archivos de excel
  stargazer,    # Presentación de resultados y tablas más estéticos
  psych,        # Para hacer estadística descriptiva 
  seasonal,     # Para desestacionalizar las series de tiempo
  aTSA,         # Para hacer prueba de efectos ARCH
  astsa,        # Para estimar, validar y pronosticar en modelos ARIMA/SARIMA
  xts,          # Para utilizar objetos de tipo xts
  car,          # Función qqplot
  fable,        # Forma moderna de hacer pronóstiocs en R (se recomienda su uso)  
  tsibble,      # Para poder emplear objetos de series de tiempo tsibble
  feasts,       # Provee una colección de herramientas para el análisis de datos de series de tiempo 
  xtable,       # Paquete xtable
  ARDL,         # Para estimar modelos lineales con rezagos distribuidos
  dynlm,        #Para estimar MCO dinámicos
  tsDyn,        #Para estimar series no lineales
  CADFtest      #Prueba de raiz unitaria no lineal
  )
#Limpiar el Espacio de Trabajo
rm(list = ls())

#1. Datos Inflación de Alimentos y ENSO (Mar 1962-Dic 2018)-----
#Base de Datos 
DINF_ENSO <- read_excel(file.choose()) 

#1.1 Serie de Inflación de Alimentos----------------------------
INF<-ts(DINF_ENSO$INF, start = 1962, frequency = 12)
View(INF)

#1.2 Serie de ENSO----------------------------------------------
ENSO<-ts(DINF_ENSO$ENSO, start = 1962, frequency = 12)
View(ENSO)

INFvsENSO <- data.frame(
  Fecha=seq(as.Date("1962-03-01"), as.Date("2018-12-01"), by="month"),
ENSO=DINF_ENSO$ENSO,
INF=DINF_ENSO$INF
  )

#2. Gráfico de INF vs ENSO---------------------------------------
ggplot(INFvsENSO, aes(x = Fecha)) +
geom_line(aes(y = ENSO, color = "ENSO"), linewidth = 0.8) +

  geom_line(aes(y = INF * (max(ENSO, na.rm = TRUE)/max(INF, na.rm = TRUE)), color = "INF"), linewidth = 0.8) +
            scale_y_continuous(
                name = "ENSO",
                sec.axis = sec_axis(~ . / (max(ENSO, na.rm = TRUE)/max(INF, na.rm = TRUE)), name = "Inflación de alimentos (INF)")
              ) +
          
              scale_color_manual(values = c("ENSO" = "blue2", "INF" = "blue4")) +
              labs(
                title = "Inflación de alimentos y ENSO en Colombia (1962-2018)",
                x = "Año",
                color = "Series"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(hjust = 0.5, face = "bold"),
                legend.position = "bottom"
              )
#3. Verificación del grado de integración de las series----------

#3.1 Grado de Integración de la serie INF------------------------

#Prueba no lineal de raíz unitaria para INF


INF_ksstest<-CADFtest(INF, type="trend", max.lag.y=12, kerne1="ba",
                       criterion="AIC", method="OLS")
print(INF_ksstest)

#Como el p-value de 0.338 es mayor al nivel de significancia alfa=0.05, NO se rechaza la hipótesis nula que plantea la existencia de raíz unitaria.
#La serie INF NO es estacionaria

#3.1.1 Serie INF diferenciada-----------------------------------
DINF<-diff(INF) #Primera diferencia de INF
View(DINF)

#Prueba de raíz unitaria según Hansen (1995) para DINF
DINF_ksstest<-CADFtest(DINF, type="drift", max.lag.y=12, kerne1="ba",
                      criterion="AIC", method="OLS")
print(DINF_ksstest)

#Como el p-value de 2.2e-16 es menor que el el nivel de signifcancia alfa=0.05, se rechaza la hipótesis nula que plantea la existencia de raíz unitaria en la serie DINF.
#La serie DINF es estacionaria y la serie INF es I(1)

#3.2 Grado de integración de la serie ENSO-------------------

#Prueba de raíz unitaria según Hansen (1995) para ENSO
ENSO_ksstest<-CADFtest(ENSO, type="drift", max.lag.y=12, kerne1="ba",
                       criterion="AIC", method="OLS")
print(ENSO_ksstest)


#Como el p-value de 3.57e-08 es menor que el el nivel de signifcancia alfa=0.05, se rechaza la hipótesis nula que plantea la existencia de raíz unitaria en la serie ENSO.
#La serie ENSO es estacionaria, es decir, es I(0)

#Se agrega crea un data.frame con DINF y ENSO
ENSO<-ENSO[-1] #Se quta la primera observación para hacerlo compatible con la seri DINF
DINFvsENSO <- data.frame(
 ENSO=ENSO,
 DINF=DINF)
  

#4. Estimación de un modelo STR para ENSO-------------------

#4.1 Selección de rezagos de ENSO como variable explicativa p1-----

auto.arima(ENSO, max.p=20, max.q=0, ic="aic")

#La cantidad máxima de rezagos para p3 es de 20, de los cuales se selecionan 5 rezagos para ENSO como variable explicativa

#4.2 Estimar distintos STR para ENSO teniendo como variables de transición candidatas ENSO_t-i para i=1,...,5

installed.packages("tsDyn")
library(tsDyn)


terasvirta_testNL <- function(y, x, rez_y, rez_x) {
  #y: serie explicada
  #x: serie explicativa
  #rez_y: número de rezagos de la variable endógena explicativa y
  #rez_x: número de rezagos de la variable exógena explicativa x
  #NOTA: Tanto los rezagos de y como los de x, conforman la lista de variables de transición candidatas
  
  #Función para crear el vector de variables explicativas respetando sus rezagos
  
  rezy_max <- max(rez_y) #máximo rezago de y
  rezx_max <- max(rez_x) #máximo rezago de x
  rez_max <- max(rezx_max, rezy_max) #rezago máximo general
  
  #Matriz de variables explicativas hasta el rezago máximo
  base_explicativas <- embed(cbind(y,x), rez_max+1)
  colnames(base_explicativas) <-c(
   paste0("y_L", 0:rez_max),
   paste0("x_L", 0:rez_max)
  )
  
  #Variable explicada
  y_dep <- base_explicativas[, "y_L0"]
  
  #Variables explicativadas deseadas según sus rezagos
  explicativas <- c(
    paste0("y_L", rez_y),
    paste0("x_L", rez_x)
  )
  
  #Matriz de variables explicativas candidatas a ser variable de transición
  X <- as.data.frame(base_explicativas[, explicativas])
  data_explicativas <- as.data.frame(base_explicativas)
  
  #Modelo lineal base
  model_lm <- lm(y_dep ~ ., data = X)
  resid_lm <- residuals(model_lm)
  
  #Lista de variables de transición candidatas
  z_cand <- c(
    paste0("y_L", rez_y),
    paste0("x_L", rez_x)
  )
  
  resultados <- list()
  
  #Correr test de Teräsvirta para cada candidato
  for (s_name in z_cand) {
    if (!(s_name %in% names(data_explicativas)))  {
     warning(paste("No se encuentra", s_name, "En la base. Se omite.")) 
    next
    }
    s=data_explicativas[[s_name]]
    s2=s^2
    s3=s^3
    s_poltaylor <- data.frame(s = s, s2 = s2, s3 = s3) #Polinomio de Taylor de orden 3 para la variable de transición
    
    #Modelos asociados
    # H01: b1=0 | b2=b3=0 --> Su rechazo se asocia con LSTR
    mod0<-lm(resid_lm~ 1)
    mod1<-lm(resid_lm~ s)
    
    # H02: b2=0 | b3=0 --> Su rechazo se asocia con ESTR
    mod2 <- lm(resid_lm~ s + s2)
    
    # H03: b3=0 --> Su rechazo se asocia con LSTR
    mod3 <- lm(resid_lm~ s + s2 + s3)
    
    test_H01 <- anova(mod0, mod1)
    test_H02 <- anova(mod1, mod2)
    test_H03 <- anova(mod2, mod3)
    
    p_H01 <-test_H01$`Pr(>F)`[2]
    p_H02 <-test_H02$`Pr(>F)`[2]
    p_H03 <-test_H03$`Pr(>F)`[2]
    
if (any(is.na(c(p_H01,p_H02, p_H03)))){
  warning(paste("Modelo inválido para,", s_name, "-se omite"))
next
  }   
  
#p-value total del modelo auxiliar (significancia global)
f_estad <- summary(mod3)$fstatistic
p_total <- pf(f_estad[1], f_estad[2], f_estad[3], lower.tail =FALSE)
    
  #Secuencia de decisión
  min_rechazo <- min(p_H01, p_H02, p_H03) #Hipótesis con mayor rechazo
    if (min_rechazo==p_H03 & min_rechazo<0.05) {
      recomendado <-"LSTR" 
    } else if (min_rechazo==p_H02 & min_rechazo < 0.05) {
      recomendado <- "ESTR"
    } else if (min_rechazo==p_H01 & min_rechazo < 0.05) {
      recomendado <- "LSTR"
    } else {
      recomendado <- "Modelo Lineal"
    }
    
    resultados[[s_name]] <- list(
      variable_transicion = s_name,
      p_H01 = p_H01,
      P_H02 = p_H02,
      p_H03 = p_H03,
      p_total = p_total,
      recomendado = recomendado
    )
  }
  
resultados <- do.call(rbind, lapply(resultados, as.data.frame))
rownames(resultados) <- NULL

return(resultados)

}

ENSO_NLtest <- terasvirta_testNL(DINF, ENSO, 1:25, 1:5)
ENSO_NLtest


pruebaARDL <- auto_ardl(DINF~ENSO, data=DINFvsENSO, max_order= 30,selection = "AIC")
pruebaARDL$top_orders






