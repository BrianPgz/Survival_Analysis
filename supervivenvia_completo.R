#####################################################################
################## Analisis de supervivencia ########################
##################      Primera parte        ########################
#####################################################################

library(foreign)
library(survival)
library(KMsurv)
library(muhaz)
library(TH.data)
library(ggplot2)
library(ggfortify)
library(proto)
library(survminer)

#############################################################################
# Diecisiete sujetos son reclutados para un estudio clinico de un nuevo 
# tratamiento para la cirrosis.  Se cree que un nuevo
# tratamiento puede desaparecer casi por completo la enfermedad , si 
# es causada por el abuso del alcohol, H

# Las covariables son la edad de un sujeto (en años) y el indicador de abuso
# actual de alcohol (si = 1, no = 0). El tiempo de supervivencia es el tiempo 
# hasta "que esta curado" (en semanas). Si un sujeto murio durante el ensayo se 
# considera censurado. Si un sujeto estaba a la espera de curarse, y 
# continua hasta al final del estudio, tambien se considera censurado.

############################################################################

                                # 1 Datos 

############################################################################

# Informacion aportada por el ejercicio:
tiempo = c(0.2, 1.7, 1.6, 1.4, 2.4, 3.5, 2.8, 2.2, 4.5, 3.6, 
           5.1, 3.4, 2.4, 5.3, 2.6, 3.8, 5.8) # Tiempo en semanas 
evento = c(rep(1,7), 0, rep(1,6), 0, 1, 1) # 0 = censura por la derecha,
# 1 = falla
edad = c(42, 45, 47, 49, 51, 53, 54, 55, 57, 57, 58, 61, 62, 
         67, 68, 68, 69) # Edad, anios
abuso = c(1, 0, 0 ,1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1) # 1 = si, 0 = no
n = length(tiempo) # = 17 sujetos

############################################################################

              # 1.1 Datos y analisis descriptivo de los datos

############################################################################


# Definimos un dataframe con la informacion
data = data.frame(edad, abuso, tiempo, evento) 
head(data)
summary(data)


# Las edades de nuestra poblacion
(tabla1 = table(data$edad))
col = colorRampPalette(c("cornflowerblue", "plum")) # para los colores
hist(data$edad, col = col(dim(tabla1)[1]), ylab = "Frecuencia absoluta", 
        main = "Edades", border = "white", las=1, xlab="Edad")

# Aunque hay que notar todos son personas adultas (adultas no tan jovenes).
# En promedio, ya consideradas de tercera edad.


# En cuanto al abuso de alcohol...
tabla2 = table(data$abuso)
tabla2
names(tabla2) = c("No","Si")
barplot(tabla2, col = "plum", ylab = "Frecuencia absoluta", 
        main = "Abuso de alcohol", border = "white", las=1)
# Tenemos una poblacion "uniforme" en cuanto al abuso del alcohol. Es
# decir, casi cantidades iguales.


# Las censuras y fallas
tabla3 = table(data$evento)
tabla3
names(tabla3) = c("Censura","Falla")
barplot(tabla3, col = "violet", ylab = "Frecuencia absoluta", 
        main = "Censuras y fallas", border = "white", las=1)

# Separaremos los intervalos por semanas, la columna intervalo de data
# contendra en que semana ocurrio el evento. El sujeto i habra sido 
# censurado o presentado falla en el intervalo: [intervalo[i], intervalo[i]+1]
data$intervalo <- floor(data$tiempo)
max(data$intervalo) # = 5, entonces nuestro periodo de observacion es [0,6]
head(data)


##################################

# Tabla de vida de toda la poblacion

##################################

library("KMsurv")
table(data$intervalo, data$evento)
fallas = table(data$intervalo, data$evento)[,2] # Fallas
censuras = table(data$intervalo, data$evento)[,1] # Censuras
# Ahora, obtenemos la tabla de vida
vida = lifetab(tis = 0:6, ninit = n, nlost = censuras, nevent = fallas)
vida


# Grafico de la funcion de supervivencia estimada en la tabla de vida
plot(0:8, c(vida$surv, rep(0,3)), col = "cornflowerblue", 
     type = "s", xlab = "t", main = "Funcion de supervivencia de toda la poblacion", 
     ylab = "S(t)", lwd = 3,las=1)

# Grafico de la funcion de riesgo estimada en la tabla de vida
plot(0:4, vida$hazard[-length(vida$hazard)], col = "plum", 
     type = "s", xlab = "t", main = "Funcion de riesgo para toda la poblacion", 
     ylab = "h(t)", lwd = 3)


##################################

# Estimador Kaplan - Meier toda la poblacion 

##################################

library("survival")
ajuste_KM = survfit(Surv(data$tiempo, data$evento, type = "right") ~ 1,
                    type = "kaplan-meier", conf.type = "plain", 
                    conf.int = 0.95, data = data)
ajuste_KM
summary(ajuste_KM)

# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM, main = "Estimador Kaplan-Meier de toda la poblacion", xlab =  "t",
     col = "plum", ylab = "S(t)", lwd = 3,
     conf.int = TRUE, las=1)

ggsurvplot(ajuste_KM,data = data, col="purple", title="Estimador Kaplan-Meier de toda la poblacion")

############################################################################

# 1.2 Tiempo de remision si existe o no abuso de alcohol

############################################################################

##################################

# Comparacion de las estimaciones
# entre las poblaciones que tienen y
# no abuso el alcohol (tabla de vida)

##################################


# Realizamos exactamente el mismo procedimiento para obtener la tabla
# de vida de la poblacion general, pero para cada subconjunto de ella.

# Para la poblacion que ***no reporta*** abuso del alcohol
data_no = subset(data, data$abuso == 0)
table(data_no$intervalo, data_no$evento) # No hay censuras
fallas_no = table(data_no$intervalo, data_no$evento)[,1] # Fallas
censuras_no = rep(0,5) # Censuras
# Ahora, obtenemos la tabla de vida
vida_no = lifetab(tis = 0:5, ninit = 8, nlost = censuras_no, nevent = fallas_no)
vida_no

# Para la poblacion que ***reporta*** abuso del alcohol
data_si = subset(data, data$abuso == 1)
table(data_si$intervalo, data_si$evento) 
fallas_si = table(data_si$intervalo, data_si$evento)[,2] # Fallas
censuras_si = table(data_si$intervalo, data_si$evento)[,1] # Censuras
# Ahora, obtenemos la tabla de vida
vida_si = lifetab(tis = 0:5, ninit = 9, nlost = censuras_si, nevent = fallas_si)
vida_si

library(scales)
# Comparacion de la funcion de supervivencia
plot(0:6, c(vida_no$surv, 0, 0), type = "s", xlab = "t",
     col = alpha("cornflowerblue",0.8), ylab = "S(t)", 
     lwd = 3)
par(new = TRUE)
plot(0:6, c(vida_si$surv, 0, 0), col = alpha("plum", 0.8), 
     type = "s", xlab = "t", ylab = "S(t)", lwd = 3)
title("Funciones de supervivencia (tabla de vida)")
legend(x = "bottomleft", c("Sin abuso", "Con abuso"), lty = 1,
       col = c("cornflowerblue", "plum"), bty = "n") 


##################################

# Estimador Kaplan - Meier por
# abuso del alcohol

##################################

# Para la poblacion con abuso del alcohol:
ajuste_KM_si = survfit(Surv(data_si$tiempo, data_si$evento,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = data)
ajuste_KM_si
summary(ajuste_KM_si)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_si, main = "Estimador Kaplan-Meier abuso", xlab =  "t",
     col = "purple", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)


# Para la poblacion sin abuso del alcohol:
ajuste_KM_no = survfit(Surv(data_no$tiempo, data_no$evento,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = data_no)
ajuste_KM_no
summary(ajuste_KM_no)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_no, main = "Estimador Kaplan-Meier no", xlab =  "t",
     col = "darkorange", ylab = "S(t)", lwd = 3,
     conf.int = TRUE, las=1)

# Comparamos los estimadores para cada poblacion:
plot(ajuste_KM_no, xlab =  "t", col = "green", 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE, xlim = c(0,6))
par(new = TRUE)
plot(ajuste_KM_si, xlab =  "t", col = "red", xlim = c(0,6), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
title("Estimadores Kaplan - Meier")
legend(x = "bottomleft", c("Sin abuso", "Con abuso"), lty = 1,
       col = c("green", "red"), bty = "n") 

##################################

# Pruebas de hipotesis 
#Ho:Superviviencia con abuso =Supervivencia sin abuso 
#vs
#H1: son diferentes
##################################

# Realizando tests para comprobar si las dos poblaciones
# (que registran y no abuso del alcohol) son iguales, estaremos
# probando si el tiempo de remision es el mismo si existe o no
# abuso del alcohol.

# Entonces, nos interesa contrastar:
# Ho: So(t) = S1(t) para todo t > 0       vs.
# H1: So(t) != S1(t) para algun t > 0; donde So corresponde
# a la funcion de supervivencia de la poblacion que no resgistra
# abuso del alcohol (data_no) y S1 corresponde a la funcion de
# supervivencia de la poblacion que resgistra abuso del alcohol,
# i.e., data_si.

# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(data$tiempo, data$evento) ~ data$abuso, rho = 0, data = data)
# Obtenemos que p-value = 0.3 > 0.05 = nivel de significancia.
# Por lo que no se cuenta con evidencia suficiente para rechazar Ho
# a un nivel de confianza del 0.95. 

# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(data$tiempo, data$evento) ~ data$abuso, rho = 1, data = data)
# Obtenemos que p-value = 0.6 > 0.05 = nivel de significancia.
# Por lo que no se cuenta con evidencia suficiente para rechazar Ho
# a un nivel de confianza del 0.95.

# Por lo tanto, concluimos que el tiempo de remision no es diferente 
# si existe o no abuso del alcohol. 


#####################################################################
##################      Segunda parte        ########################
#####################################################################

############################################################################

# 1.3 Ajuste del modelo de riesgos proporcionales de Cox

############################################################################
#el objeto surv 
Surv(5, 0)#0
Surv(5, 1)#
Surv(5, 3)#
########################################################################
attach(data) 
##Los argumentos de la funcion son: 
args(coxph)
##y arroja 
#coef: Coeficiente estimado de la beta.
#exp(coef): Exponencial elevado al coeficiente beta estimado.
#se(coef): La desviación estándar de la estimación.
#z: Valor del estadístico para prueba Wald de beta igual a cero.
#p: P-valor de la prueba Wald, beta igual a cero.

# Realizamos exploracion en cada covariable:
head(data)
summary(coxph(Surv(tiempo, evento) ~ edad))
summary(coxph(Surv(tiempo, evento) ~ factor(abuso)))

# Notemos que con la covariable abuso tenemos un p-value = 0.3 > 0.2
# Asi que observemos el modelo incluyendo esa variable.  Uno pensaria
# que el echo de que un individuo abuse o no del alcohol seria relevante,
# pero debemos checar si verdaderamente aporta informacion al modelo.

# Ajustamos el modelo
cox = coxph(Surv(tiempo, evento) ~ edad + factor(abuso))
cox
summary(cox)
# Ambas covariables son significantes: notemos que  abuso si
# es una variable significativa. Entonces nuestro modelo
# ajustado de Cox esta dado por 1.7804 abuso - 0.3386 edad

# Veamos si las variables edad y abuso interactuan
survdiff(Surv(tiempo, evento) ~ edad + factor(abuso), rho = 0, 
         data = data)
survdiff(Surv(tiempo, evento) ~ edad + factor(abuso), rho = 1, 
         data = data)
# Asi es, es decir, el hecho de que el inidividuo abuse o no del alcohol
# tiene que ver en cierta medida con su edad.


############################################################################

# 1.4 Verificacion de supuestos

##############################################################################

# Supuesto de riesgos proporcionales

##############################################################################

# Utilizamos la funcion cox.zph para hacer el test al modelo
#Ho: los riesgos son proporcionales vs 
#H1: los riesgos no son proporcionales .

sup = cox.zph(cox)
sup
#Ya que el pvalor > alpha para cada una de las covariables se 
#concluye que no existen pruebas suficientes para rechazar Ho. 
#Es decir el cociente de riesgos es independiente del tiempo.


# Se verifican los supuestos para ambas variables, todos los p-value con
# muy "altos". Ademas,
plot(sup, var = "edad", xlab = "t", main = "Variable 'edad' en el tiempo",
     col = c("cornflowerblue", "plum"))
# Y, finalmente,
plot(sup, var = "factor(abuso)", xlab = "t", 
     main = "Variable 'abuso' en el tiempo", col = c("cornflowerblue", "plum")) 
ggcoxzph(sup)#verificacion grafica

##################################
#Analisis de residuales 
##################################

# Ajuste global de los datos
# (residuos Cox - Snell)

ggcoxdiagnostics(cox, type = "schoenfeld")
## Esta función nos da todos....
??ggcoxdiagnostics
##################################

# Linealidad de las covariables
# (Residuos de martingalas)

residuales = residuals(cox, type = "martingale")

# Graficamos para cada variable
# Edad
plot(edad, residuales, xlab = "Edad", ylab = "Residuos de Martingalas",
     main = "Linealidad covariables", col ="red", pch = 20)
lines(smooth.spline(residuales ~ edad, df = 6), col = "cornflowerblue")
lines(edad,fitted(lm(residuales ~ edad)), col = "plum", lwd = 2)

# Abuso del alcohol
plot(abuso, residuales, xlab = "Abuso", ylab = "Residuos de Martingalas",
     main = "Linealidad covariables", col ="red", pch = 20)
lines(smooth.spline(abuso ~ residuales, df = 6), col = "cornflowerblue",
      lwd = 2)
lines(abuso,fitted(lm(residuales ~ abuso)), col = "plum", lwd = 2)
##PRUEBA BINARIA



##################################

# Residuos de devianza
##################################

dev = residuals(cox, type = "deviance")
min(dev)
max(dev)
summary(dev)

# Graficamos
plot(dev, col = "purple", main = "Residuos de devianza",
     xlab = "ID", ylab = "dev", pch = 20, ylim = c(-3,3))
abline(h = c(-1, 1), col = "plum")
abline(h = c(-2.5, 2.5), col = "lightsalmon")

# Casos extremos:
dev[abs(dev) >= 1] # Posicion
data[c(13, 16),]


##################################

# Residuos DfBeta

##################################
#La especificación del argumento type = dfbeta produce una matriz 
#de cambios estimados en los coeficientes de regresión al eliminar cada 
#observación a su vez; asimismo, type = dfbetas produce los cambios estimados 
#en los coeficientes divididos por sus errores estándar 

dfbeta = resid(cox, type = "dfbeta")

# graficamos para cada covariable
plot(dfbeta[,1], col ="purple", pch = 20, main = "Dfbeta - Edad",
     ylab = "Dfbeta.edad")

plot(dfbeta[,2], col ="purple", pch = 20, main = "Dfbeta - Abuso",
     ylab = "Dfbeta.abuso")
#Tenemos un valor influyente 

############################################################################

# 1.5 Interpretacion coeficientes de regresion

############################################################################

# Tenemos nuestro modelo...
cox

# Es decir, el coeficiente de regresion para la edad es
cox$coefficients[1]
# Es decir, aparentemente el riesgo de remision tiene una relacion
# inversamente proporcional a la edad. A mayor edad, menor riesgo
# de remision y viceversa.

# Mientras que el coeficiente correspondiente al abuso del alcohol es
cox$coefficients[2]
# Por lo que tiene una relacion directamente proporcional: a mayor
# abuso del alcohol, mayor riesgo.

# Graficamos
# Lineal:
library(scatterplot3d)
scatterplot3d(edad, abuso, (cox$coefficients[1] * edad) + 
                (cox$coefficients[2] * abuso),highlight.3d = TRUE,
              xlab = "Edad", ylab = "Abuso", zlab = "Riesgo", pch = 20, 
              main = "Modelo de riesgos proporcionales de Cox")

# Sin la transformacion:
scatterplot3d(edad, abuso, exp((cox$coefficients[1] * edad) + 
                                 (cox$coefficients[2] * abuso)), highlight.3d = TRUE, 
              xlab = "Edad", ylab = "Abuso", zlab = "Riesgo", pch = 20, 
              main = "Modelo de riesgos proporcionales de Cox")

##Como la exp(Bk)=0.5 entonces el riesgo de morir o presentar la falla es 0.5 veces mayor a omenor si se aumenta una unidad la variable k
############################################################################

# Los sujetos que tienen un abuso del alcohol
# tienen mayor riesgo de entrar en remision?

############################################################################

##################################

# Funcion de riesgo por abuso
# del alcohol con el modelo de Cox

##################################

info0 = data.frame(abuso = 0, edad = mean(edad))
info1 = data.frame(abuso = 1, edad = mean(edad))

fit0 = survfit(cox, newdata = info0)
fit1 = survfit(cox, newdata = info1)

plot(fit0, conf.int = FALSE, col = "cornflowerblue", lwd = 3,
     main = "Comparacion de supervivencias por abuso del alcohol")
par(new = TRUE)
plot(fit1, conf.int = FALSE, col = "purple", lwd = 3, lty = 3)
legend(x = "bottomleft", legend = c("So","S1"), lty = c(1, 3), 
       col = c("cornflowerblue","purple"), lwd=3)


# Por lo tanto, aunado al resultado obtenido en el inciso 1.2,
# los sujetos que registran abuso del alcohol no tienen mayor
# riesgo de entrar en remision. De hecho, tienen menor riesgo
# de entrar en remision. Lo cual hace sentido con el hecho de
# que remision es la desaparicion de los sintomas de la cirrosis.

