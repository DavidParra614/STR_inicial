##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________
#                           BANCO DE LA REPÚBLICA
#                           UNIDAD DE ECONOMETRÍA
##                MODELO STR INFLACIÓN ALIMENTOS VS ENSO
##                      David Steven Parra Almeyda 
##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________

#CARGA Y DESCARGA DE PAQUETES---------------------------------------------------
if(0){
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
}
#Limpiar el Espacio de Trabajo
rm(list = ls())
require(readxl)

#Definir directorio de trabajo
Ruta.Sv = 'C:/Users/ASUS X507UA/OneDrive/Documentos/STR_inicial/STR_inicial'
Ruta.LF = '~/Archivos/STR2025/Steven/STR_Inicial'
ifelse(Sys.info()[1]=='Darwin', setwd(Ruta.LF), setwd(Ruta.Sv))

#1. Datos Inflación de Alimentos y ENSO (Mar 1962-Dic 2018)-----
#Base de Datos
datos <- 'Datos_ENSO_DINF.xlsx'
DINF_ENSO <- read_excel(datos) 

#1.1 Serie de Inflación de Alimentos----------------------------
INF<-ts(DINF_ENSO$INF, start = 1962, frequency = 12)

#1.2 Serie de ENSO----------------------------------------------
ENSO<-ts(DINF_ENSO$ENSO, start = 1962, frequency = 12)

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

#Prueba de raíz unitaria según Hansen (1995) para INF

INF_ksstest<-CADFtest(INF, type="drift", max.lag.y=12, kerne1="ba",
                       criterion="AIC", method="OLS")
print(INF_ksstest)

#Como el p-value de 0.349 es mayor al nivel de significancia alfa=0.05, NO se rechaza la hipótesis nula que plantea la existencia de raíz unitaria.
#La serie INF NO es estacionaria

#3.1.1 Serie INF diferenciada-----------------------------------
DINF<-diff(INF) #Primera diferencia de INF

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


#Como el p-value de 3.39e-08 es menor que el el nivel de signifcancia alfa=0.05, se rechaza la hipótesis nula que plantea la existencia de raíz unitaria en la serie ENSO.
#La serie ENSO es estacionaria, es decir, es I(0)

#Se agrega crea un data.frame con DINF y ENSO
ENSO<-ENSO[-1] #Se quta la primera observación para hacerlo compatible con la seri DINF
DINFvsENSO <- data.frame(
 ENSO=ENSO,
 DINF=DINF)
  
#4. Creación de función para el test de NO linealidad, Terarsvirta, (1995)------

terasvirta_testNL <- function(y=DINF, x=ENSO, rez_y=5, rez_x=3) 
{
  #y: serie explicada
  #x: serie explicativa
  #rez_y: número de rezagos de la variable endógena explicativa y
  #rez_x: número de rezagos de la variable exógena explicativa x
  #NOTA: Tanto los rezagos de y como los de x, conforman la lista de variables de transición candidatas
  
  #Función para crear el vector de variables explicativas respetando sus rezagos
  
  
  rez_max <- max(rez_x, rez_y) #rezago máximo general
  
  #Matriz de variables explicativas hasta el rezago máximo
  base_explicativas <- embed(cbind(y,x), rez_max+1)
  colnames(base_explicativas) <-c(
    paste0('y_L', c('',1:rez_max)),
    paste0('x_L', c('',1:rez_max))
  )
  
  #Variable explicada
  y_dep <- base_explicativas[, 'y_L']
  
  #Variables explicativadas deseadas según sus rezagos
  explicativas <- c(
    paste0("y_L", 1:rez_y),
    paste0("x_L", 1:rez_x)
  )
  
  #Matriz de variables explicativas candidatas a ser variable de transición
  X <- (base_explicativas[, explicativas])
  
  
  #Lista de variables de transición candidatas
  z_cand <- c(
    paste0('y_L', 1:rez_y),
    paste0('x_L', 1:rez_x)
  )
  
  resultados <- list()
  
  #Correr test de Teräsvirta para cada candidata a ser variable de transición
  for (s_name in z_cand)
  {
    if (!(s_name %in% names(data_explicativas)))  {
      warning(paste("No se encuentra", s_name, "En la base. Se omite.")) 
      next
    }
    s=data_explicativas[[s_name]]
    
    #Modelo H0 --> Modelo Lineal
    Mod_H0 <- lm(y_dep~X) #Modelo bajo la Hipótesis Nula H0 que plantea un modelo lineal 
    SSR_0 <- sum((residuals(Mod_H0))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Nula H0
    
    #Modelo H1 --> Modelo NO lineal completo 
    Mod_H1 <- lm(y_dep ~ X + X*s + X*(s^2) + X*(s^3)) #Modelo bajo la Hipótesis Alternativa H1 que plantea Modelo NO lineal con la variable de transición s, s^2 y s^3 
    SSR_1 <- sum((residuals(Mod_H1))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H1
    
    #Estadístico LM_1 para probar la no linealidad del modelo según distribución F
    LM_1 <- ((SSR_0-SSR_1)/(length(Mod_H1$coef)-length(Mod_H0$coef)))/(SSR_0/(length(y_dep)-length(Mod_H1$coef))) #Estadístico LM_1 calculado según distribución F
    pvalue.LM1 <- 1-pf(LM_1, df1=length(Mod_H1$coef)-length(Mod_H0$coef), df2=length(y_dep)-length(Mod_H1$coef))  #p_value arrojado para mirar no linealidad
    
    #Modelo H02 --> Modelo bajo la Hipótesis Nula H02 que plantea que b1=0 | b2=b3=0, es decir, el modelo es lineal
    #NOTA: La Hipótesis Nula H02 es la misma hipótesis H0 que plantea linealidad
    Mod_H2 <- lm(y_dep ~ X + X*s) #Modelo bajo la Hipótesis Alternativa H2 que plantea b1≠0 | b2=b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 1
    SSR_2 <- sum((residuals(Mod_H2))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H2
    
    #Estadístico LM_2 para probar la Hipótesis Nula H02 del modelo según distribución F
    LM2_2 <- ((SSR_0-SSR_2)/(length(Mod_H2$coef)-length(Mod_H0$coef)))/(SSR_0/(length(y_dep)-length(Mod_H2$coef))) #Estadístico LM_2 caclulado según distribución F
    pvalue.LM2 <- 1-pf(LM_2, df1=length(Mod_H2$coef)-length(Mod_H0$coef), df2=length(y_dep)-length(Mod_H2$coef))   #p_value arrojado del modelo H02
    
    #Modelo H03 --> Modelo bajo la Hipótesis Nula H03 que plantea que b2=0 | b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 1
    #NOTA: La Hipótesis Nula H03 es la misma hipótesis H2 que plantea que el polinomio de Taylor de la variable de transición s, es de orden 1
    Mod_H3 <- lm(y_dep ~ X + X*s + X(s^2)) #Modelo bajo la Hipótesis Alternativa H3 que plantea b2≠0 | b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 2
    SSR_3 <- sum((residuals(Mod_H3))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H3
    
    #Estadístico LM_3 para probar la Hipótesis Nula H03 del modelo según distribución F
    LM2_3 <- ((SSR_2-SSR_3)/(length(Mod_H3$coef)-length(Mod_H2$coef)))/(SSR_2/(length(y_dep)-length(Mod_H3$coef))) #Estadístico LM_3 caclulado según distribución F
    pvalue.LM3 <- 1-pf(LM_3, df1=length(Mod_H3$coef)-length(Mod_H2$coef), df2=length(y_dep)-length(Mod_H3$coef))   #p_value arrojado del modelo H03
    
    #Modelo H04 --> Modelo bajo la Hipótesis Nula H04 que plantea que b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 2
    #NOTA: La Hipótesis Nula H04 es la misma hipótesis H3 que plantea que el polinomio de Taylor de la variable de transición s, es de orden 2
    Mod_H4 <- Mod_H1 #Modelo bajo la Hipótesis Alternativa H4 que plantea b3≠0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 3
    #NOTA: La Hipótesis Alternativa H1 es la misma hipótesis H1, que plantea el modelo no lineal completo
    SSR_4 <- sum((residuals(Mod_H4))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H4
    
    #Estadístico LM_4 para probar la Hipótesis Nula H04 del modelo según distribución F
    LM2_3 <- ((SSR_3-SSR_4)/(length(Mod_H4$coef)-length(Mod_H3$coef)))/(SSR_3/(length(y_dep)-length(Mod_H4$coef))) #Estadístico LM_3 caclulado según distribución F
    pvalue.LM4 <- 1-pf(LM_4, df1=length(Mod_H4$coef)-length(Mod_H3$coef), df2=length(y_dep)-length(Mod_H4$coef))   #p_value arrojado del modelo H03
    
    if(0){
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
      } else recomendado <- "Modelo Lineal"
    } 
    
    resultados[[s_name]] <- list(
      variable_transicion = s_name,
      p_H0 = pvalue.LM0
      #  p_H01 = p_H01,
      #  P_H02 = p_H02,
      #  p_H03 = p_H03,
      #  p_total = p_total,
      # recomendado = recomendado
    )
    
  }
  
  resultados <- do.call(rbind, lapply(resultados, as.data.frame))
  rownames(resultados) <- NULL
  
  return(resultados)
  
}

#5. Estimación de un modelo STR para ENSO-------------------

#5.1 Selección de rezagos de ENSO como variable explicativa p1-----

auto.arima(ENSO, max.p=20, max.q=0, ic="aic")

#La cantidad máxima de rezagos para p3 es de 20, de los cuales se selecionan 5 rezagos para ENSO como variable explicativa

#4.2 Estimar distintos STR para ENSO teniendo como variables de transición candidatas ENSO_t-i para i=1,...,5




ENSO_NLtest <- terasvirta_testNL(DINF, ENSO, 1:25, 1:5)
ENSO_NLtest


pruebaARDL <- auto_ardl(DINF~ENSO, data=DINFvsENSO, max_order= 30,selection = "AIC")
pruebaARDL$top_orders


