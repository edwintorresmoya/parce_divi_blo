# parce_divi_blo
INtento2
######### Diseño parcelas divididas por bloques


# Revision de datos
datos <- read.csv("Datos.csv")
str(datos)
datos$BLOQUE <- as.factor(datos$BLOQUE)
datos2 <- datos
mean(datos[5:ncol(datos)], na.rm = T)
mean(datos$RENDIMIENTO, na.rm = T)
sapply(datos, mean)
mean(datos$X2flores, na.rm = T)

# Modelo
fx <- function(variable){
    print("##############################################################################")
    #modelo
    modelo <- aov((variable ~ datos$BLOQUE + (datos$VARIEDAD*datos$TRATAMIENTO) + Error(datos$FINCA)))
    resumen <- summary(modelo)
    print(resumen)
    print("##############################################################################")
    ###Shapiro
    shap = shapiro.test(modelo$`datos$FINCA`$residuals)
    print("Test de Shapiro para Fincas")
    print(shap)
    print("##############################################################################")
    print("Test de Shapiro para Tratamiento")
    print(shapiro.test(modelo$Within$residuals))
    #grados de libertad
    gl.error.a <- modelo$`datos$FINCA`$df.residual
    gl.error.b <- modelo$Within$df.residual
    #residuales
    x <- modelo$`datos$FINCA`$residuals
    y <- modelo$Within$residuals
    #suma de cuadrado
    sc.error.a <- sum(x^2)
    sc.error.b <- sum(y^2)
    #Cuadrado medio error
    print("##############################################################################")
    cm.a <- sc.error.a/gl.error.a
    cm.b <- sc.error.b/gl.error.b
    promedio_total <- mean(variable)
    print("Promedio Total")
    print(promedio_total)
    print("Promedio por Tratamiento")
    print(tapply(variable, datos$TRATAMIENTO, mean))
    
    a <- length(levels(datos$VARIEDAD))
    b <- length(levels(datos$TRATAMIENTO))
    
    cv.a <- sqrt(cm.a/b)*100/promedio_total
    cv.b <- sqrt(cm.b)*100/promedio_total
    print("##############################################################################")
    print("Coeficientes de Variacion de parcela y tratamiento")
    print(cv.a)
    print(cv.b)
    
    print("##############################################################################")
    
    compara.a <- LSD.test(variable, datos$VARIEDAD, gl.error.a, cm.a)
    compara.b <- LSD.test(variable, datos$TRATAMIENTO, gl.error.b, cm.b)
    print("LSD TEST")
    print(compara.a)
    print(compara.b)
    
    print("##############################################################################")
    
    
}


modelo <- aov((datos$RENDIMIENTO ~ datos$BLOQUE + (datos$VARIEDAD*datos$TRATAMIENTO) + Error(datos$FINCA)))
w1<- summary(modelo)
fx(datos$RENDIMIENTO)
fx(datos$TUBERCULOS)
fx(datos$X1tallos)
fx(datos$X1hojas)
fx(datos$X2tallos)
fx(datos$X2hojas)
fx(datos$X2flores)

