
# LIMPIAR DATOS
rm(list = ls())

# LIBRERIAS UTILIZADAS
library(psych)
library(readr)
library(dplyr)

# FUNCIONES IMPLEMENTADAS
clc <- function(){
  cat("\014")
}

area = function(x=0){
  z=seq(-3,3,0.01)
  fdp = dnorm(z)
  plot(z,fdp,type="l")
  polygon(c(z[z<=x],x),c(fdp[z<=x],fdp[z==-3]),col="red")
  polygon(c(z[z>=x],x),c(fdp[z>=x],fdp[z==-3]),col="blue")
}

################################################################################


clc()

alpha = 0.05

# IMPORTAR DATASETS

# DATASET 2015-II
datos_2015_2 = read.csv(file.path("/home/oscarvch03/Desktop/Proyecto_Estadistica", "Saber_11__2015-2.csv"))
datos_2015_2_clc = na.omit(data.frame(filter(datos_2015_2, ESTU_DEPTO_RESIDE != "-")))
head(datos_2015_2_clc)
len2 = length(datos_2015_2_clc$ESTU_GENERO)

# Diagrama de Barras por Departamento
plot(x = datos_2015_2_clc$ESTU_DEPTO_RESIDE, xlab = "Departamento" , las = 3, main = "2015-II")


################################################################################
# 2) PRUEBAS DE HIPOTESIS ######################################################

# a) H vs M - Cola Superior (¿A LOS HOMBRES LES VA MEJOR?)
#    H_0: mu_h = mu_m
#    H_a: mu_h > mu_m 

mujeres = data.frame(filter(datos_2015_2_clc, ESTU_GENERO == "F"))
hombres = data.frame(filter(datos_2015_2_clc, ESTU_GENERO == "M"))
n_m = dim(mujeres)[1]
n_h = dim(hombres)[1]

med_m = mean(mujeres$PUNT_GLOBAL)
med_h = mean(hombres$PUNT_GLOBAL)
var_m = var(mujeres$PUNT_GLOBAL)
var_h = var(hombres$PUNT_GLOBAL)

err_std1 = sqrt((var_h / n_h) + (var_m / n_m))
Z1 = (med_h - med_m) / err_std1  # ¿Como graficar Z?

Z1_alpha = qnorm(alpha, lower.tail = F)
p1 = pnorm(Z1, lower.tail = F) # Valor p
area(Z1_alpha)

# Efectivamente a los HOMBRES les va mejor que a las MUJERES ya que el 
# estadistico Z cae en la region de rechazo, y el valor p tiende a 0


# b) ESTRATO 1 VS ESTRATO 6 (¿LE VA MEJOR AL ESTRATO 6?)
#    H_0: mu_E2 = mu_E1 
#    H_a: mu_E2 > mu_E1  

est_1 = data.frame(filter(datos_2015_2_clc, FAMI_ESTRATOVIVIENDA == "Estrato 1"))
est_6 = data.frame(filter(datos_2015_2_clc, FAMI_ESTRATOVIVIENDA == "Estrato 6"))
n_E1 = dim(est_1)[1]
n_E6 = dim(est_6)[1]
random1 = sample(2:n_E1, n_E6, replace = F)
new_est_1 = est_1[random1, ]
n_new_est_1 = dim(new_est_1)[1] 

med_E1 = mean(new_est_1$PUNT_GLOBAL)
med_E6 = mean(est_6$PUNT_GLOBAL)
var_E1 = var(new_est_1$PUNT_GLOBAL)
var_E6 = var(est_6$PUNT_GLOBAL)

err_std2 = sqrt((var_E1 / n_new_est_1) + (var_E6 / n_E6))
Z2 = (med_E6 - med_E1) / err_std2  # ¿Como graficar Z?

Z2_alpha = qnorm(alpha, lower.tail = F)
p2 = pnorm(Z2, lower.tail = F) # Valor p
area(Z2_alpha)

# c) Bogota vs Antioquia (¿LE VA PEOR A ANTIOQUIA?)
#    H_0: mu_A = mu_B
#    H_a: mu_A < mu_B

Ant = data.frame(filter(datos_2015_2_clc, ESTU_DEPTO_RESIDE == "ANTIOQUIA"))
Bog = data.frame(filter(datos_2015_2_clc, ESTU_DEPTO_RESIDE == "BOGOTA"))
n_A = dim(Ant)[1]
n_B = dim(Bog)[1]

med_A = mean(Ant$PUNT_GLOBAL)
med_B = mean(Bog$PUNT_GLOBAL)
var_A = var(Ant$PUNT_GLOBAL)
var_B = var(Bog$PUNT_GLOBAL)

err_std3 = sqrt((var_B / n_B) + (var_A / n_A))
Z3 = (med_A - med_B) / err_std3  # ¿Como graficar Z?

Z3_alpha = qnorm(alpha, lower.tail = T)
p3 = pnorm(Z3, lower.tail = T) # Valor p
area(Z3_alpha)

# Efectivamente a ANTIOQUIA le va peor que a BOGOTA ya que el 
# estadistico Z cae en la region de rechazo, y el valor p tiende a 0


# d) Bogota vs Choco (¿LE VA PEOR A CHOCO?)
#    H_0: mu_C = mu_B
#    H_a: mu_C < mu_B

Cho = data.frame(filter(datos_2015_2_clc, ESTU_DEPTO_RESIDE == "CHOCO"))
n_C = dim(Cho)[1]
random2 = sample(2:n_B, n_C, replace = F)
new_Bog = Bog[random2, ]
n_new_B = dim(new_Bog)[1] 

med_new_B = mean(new_Bog$PUNT_GLOBAL)
med_C = mean(Cho$PUNT_GLOBAL)
var_new_B = var(new_Bog$PUNT_GLOBAL)
var_C = var(Cho$PUNT_GLOBAL)

err_std4 = sqrt((var_new_B / n_new_B) + (var_C / n_C))
Z4 = (med_C - med_new_B) / err_std4  # ¿Como graficar Z?

Z4_alpha = qnorm(alpha, lower.tail = T)
p4 = pnorm(Z4, lower.tail = T) # Valor p
area(Z4_alpha)

# Efectivamente a CHOCO le va peor que BOGOTA ya que el 
# estadistico Z cae en la region de rechazo, y el valor p tiende a 0


# e) ANTIOQUIA VS VALLE (¿HAY DIFERENCIA ENTRE AMBOS?)
#    H_0: mu_A = mu_V
#    H_a: mu_A != mu_V

Vall = data.frame(filter(datos_2015_2_clc, ESTU_DEPTO_RESIDE == "VALLE"))
n_V = dim(Vall)[1]
random3 = sample(2:n_A, n_V, replace = F)
new_Ant = Ant[random3, ]
n_new_A = dim(new_Ant)[1]

med_new_A = mean(new_Ant$PUNT_GLOBAL)
med_V = mean(Vall$PUNT_GLOBAL)
var_new_A = var(new_Ant$PUNT_GLOBAL)
var_V = var(Vall$PUNT_GLOBAL)

err_std5 = sqrt((var_new_A / n_new_A) + (var_V / n_V))
Z5 = (med_V - med_new_A) / err_std5  # ¿Como graficar Z?

Z5_alpha = qnorm(alpha / 2, lower.tail = F)
p5 = pnorm(Z5, lower.tail = F) # Valor p
area(Z5_alpha)

# Hay evidencia suficiente para decir que a ANTIOQUIA le va mejor que
# al VALLE ya que el estadistico Z cae en la region de rechazo y el valor p tiende a 0


################################################################################
## 3) MODELOS DE REGRESION 2019-II #############################################


# a) x = MATEMATICAS, y = CIENCIAS NATURALES

x1 = datos_2015_2_clc$PUNT_MATEMATICAS
y1 = datos_2015_2_clc$PUNT_C_NATURALES

plot(x1, y1, main = "CIENCIAS NATURALES ~ MATEMATICAS")
mod1 = lm(y1 ~ x1)
abline(mod1, col = 3)
x_ast = data.frame(x1 = seq(0, 100))
ic1 = predict(mod1, x_ast, interval = "confidence")
lines(x_ast$x1, ic1[, 2], lty = 2, col = "blue")
lines(x_ast$x1, ic1[, 3], lty = 2, col = "blue")
ic1 = predict(mod1, x_ast, interval = "prediction")
lines(x_ast$x1, ic1[, 2], lty = 2, col = "red")
lines(x_ast$x1, ic1[, 3], lty = 2, col = "red")

betas1 = mod1$coefficients
# y1_gorrito = beta0 + beta1 * x1 = 17.63 + 0.6472 * x1
summary(mod1)

x1_b = mean(x1)
y1_b = mean(y1)
s1_xx = sum((x1 - x1_b) ^ 2)
s1_yy = sum((y1 - y1_b) ^ 2)
beta1_1 = mod1$coefficients[2]
r1 = beta1_1 * sqrt(s1_xx / s1_yy)
r1_cuad = r1 ^ 2


# b) x = LECTURA CRITICA, y = INGLES

x2 = datos_2015_2_clc$PUNT_LECTURA_CRITICA
y2 = datos_2015_2_clc$PUNT_INGLES

plot(x2, y2, main = "INGLES ~ LECTURA CRITICA")
mod2 = lm(y2 ~ x2)
abline(mod2, col = 3)
x_ast = data.frame(x2 = seq(0, 100))
ic2 = predict(mod2, x_ast, interval = "confidence")
lines(x_ast$x2, ic2[, 2], lty = 2, col = "blue")
lines(x_ast$x2, ic2[, 3], lty = 2, col = "blue")
ic2 = predict(mod2, x_ast, interval = "prediction")
lines(x_ast$x2, ic2[, 2], lty = 2, col = "red")
lines(x_ast$x2, ic2[, 3], lty = 2, col = "red")

betas2 = mod2$coefficients
# y2_gorrito = beta0 + beta1 * x2 = 15.8861 + 0.6934 * x2
summary(mod2)

x2_b = mean(x2)
y2_b = mean(y2)
s2_xx = sum((x2 - x2_b) ^ 2)
s2_yy = sum((y2 - y2_b) ^ 2)
beta1_2 = mod2$coefficients[2]
r2 = beta1_2 * sqrt(s2_xx / s2_yy)
r2_cuad = r2 ^ 2


# c) x = MATEMATICAS, y = LECTURA CRITICA

x3 = datos_2015_2_clc$PUNT_MATEMATICAS
y3 = datos_2015_2_clc$PUNT_LECTURA_CRITICA

plot(x3, y3, main = "LECTURA CRITICA ~ MATEMATICAS")
mod3 = lm(y3 ~ x3)
abline(mod3, col = 3)
x_ast = data.frame(x3 = seq(0, 100))
ic3 = predict(mod3, x_ast, interval = "confidence")
lines(x_ast$x3, ic3[, 2], lty = 2, col = "blue")
lines(x_ast$x3, ic3[, 3], lty = 2, col = "blue")
ic3 = predict(mod3, x_ast, interval = "prediction")
lines(x_ast$x3, ic3[, 2], lty = 2, col = "red")
lines(x_ast$x3, ic3[, 3], lty = 2, col = "red")

betas3 = mod3$coefficients
# y3_gorrito = beta0 + beta1 * x3 = 22.3876 + 0.5453 * x3
summary(mod3)

x3_b = mean(x3)
y3_b = mean(y3)
s3_xx = sum((x3 - x3_b) ^ 2)
s3_yy = sum((y3 - y3_b) ^ 2)
beta1_3 = mod3$coefficients[2]
r3 = beta1_3 * sqrt(s3_xx / s3_yy)
r3_cuad = r3 ^ 2


# d) x = MATEMATICAS, y = INGLES

x4 = datos_2015_2_clc$PUNT_MATEMATICAS
y4 = datos_2015_2_clc$PUNT_INGLES

plot(x4, y4, main = "INGLES ~ MATEMATICAS")
mod4 = lm(y4 ~ x4)
abline(mod4, col = 3)
x_ast = data.frame(x4 = seq(0, 100))
ic4 = predict(mod4, x_ast, interval = "confidence")
lines(x_ast$x4, ic4[, 2], lty = 2, col = "blue")
lines(x_ast$x4, ic4[, 3], lty = 2, col = "blue")
ic4 = predict(mod4, x_ast, interval = "prediction")
lines(x_ast$x4, ic4[, 2], lty = 2, col = "red")
lines(x_ast$x4, ic4[, 3], lty = 2, col = "red")

betas4 = mod4$coefficients
# y4_gorrito = beta0 + beta1 * x4 = 23.1451 + 0.5430 * x4
summary(mod4)

x4_b = mean(x4)
y4_b = mean(y4)
s4_xx = sum((x4 - x4_b) ^ 2)
s4_yy = sum((y4 - y4_b) ^ 2)
beta1_4 = mod4$coefficients[2]
r4 = beta1_4 * sqrt(s4_xx / s4_yy)
r4_cuad = r4 ^ 2


# e) x = LECTURA CRITICA VS y = SOCIALES CIUDADANAS

x5 = datos_2015_2_clc$PUNT_LECTURA_CRITICA
y5 = datos_2015_2_clc$PUNT_SOCIALES_CIUDADANAS

plot(x5, y5, main = "SOCIALES CIUDADANAS ~ LECTURA CRITICA")
mod5 = lm(y5 ~ x5)
abline(mod5, col = 3)
x_ast = data.frame(x5 = seq(0, 100))
ic5 = predict(mod5, x_ast, interval = "confidence")
lines(x_ast$x5, ic5[, 2], lty = 2, col = "blue")
lines(x_ast$x5, ic5[, 3], lty = 2, col = "blue")
ic5 = predict(mod5, x_ast, interval = "prediction")
lines(x_ast$x5, ic5[, 2], lty = 2, col = "red")
lines(x_ast$x5, ic5[, 3], lty = 2, col = "red")

betas5 = mod5$coefficients
# y5_gorrito = beta0 + beta1 * x5 = 4.9585 + 0.9019 * x5
summary(mod5)

x5_b = mean(x5)
y5_b = mean(y5)
s5_xx = sum((x5 - x5_b) ^ 2)
s5_yy = sum((y5 - y5_b) ^ 2)
beta1_5 = mod5$coefficients[2]
r5 = beta1_5 * sqrt(s5_xx / s5_yy)
r5_cuad = r5 ^ 2
