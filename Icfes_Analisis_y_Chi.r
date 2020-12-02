
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

maxs <- function(dataframe){
    conts = c(0, 0, 0, 0, 0) 
    k = seq(1, dim(dataframe)[1])
    for(i in k){
      # cat(i, "\n")
      row = c(dataframe[i, ]$PUNT_LECTURA_CRITICA, dataframe[i, ]$PUNT_MATEMATICAS, 
              dataframe[i, ]$PUNT_C_NATURALES, dataframe[i, ]$PUNT_SOCIALES_CIUDADANAS, 
              dataframe[i, ]$PUNT_INGLES)
      # cat(row, "\n")
      ind = which.max(row)
      # cat(ind, "\n")
      conts[ind] = conts[ind] + 1
    }
    return(conts)
}

################################################################################

clc()

# DATASET 2019-II
datos_2019_2 = read.csv(file.path("/home/oscarvch03/Desktop/Proyecto_Estadistica", "Saber_11__2019-2.csv"))
datos_2019_2_clc = na.omit(data.frame(filter(datos_2019_2, ESTU_DEPTO_RESIDE != "")))
head(datos_2019_2_clc)
len19 = length(datos_2019_2_clc$ESTU_GENERO)

# DATASET 2015-II
datos_2015_2 = read.csv(file.path("/home/oscarvch03/Desktop/Proyecto_Estadistica", "Saber_11__2015-2.csv"))
datos_2015_2_clc = na.omit(data.frame(filter(datos_2015_2, ESTU_DEPTO_RESIDE != "-")))
head(datos_2015_2_clc)
len15 = length(datos_2015_2_clc$ESTU_GENERO)

################################################################################
# 1) ANALISIS DESCRIPTIVO DE LOS DATOS #########################################

# a) DISTRIBUCION POR DEPARTAMENTOS 

par(mar=c(10, 6, 4, 4))
a = datos_2015_2_clc$ESTU_DEPTO_RESIDE
barplot(table(a)[2:length(summary(a))], ylab = "Frecuencia", las = 3, 
        main = "Departamentos 2015-II", col = rainbow(length(summary(a))))
b = datos_2019_2_clc$ESTU_DEPTO_RESIDE
barplot(table(b)[2:length(summary(b))], ylab = "Frecuencia", las = 3, 
        main = "Departamentos 2019-II", col = rainbow(length(summary(b))))


# b) DISTRIBUCION POR GENERO

x1 = summary(datos_2015_2_clc$ESTU_GENERO)
labels1 = round(100 * x1 / sum(x1), 2)
leg1 = names(summary(datos_2015_2_clc$ESTU_GENERO))
pie(x1, labels = labels1, radius = 1.4, main = "DISTRIBUCION POR GENERO 2015-II", col = rainbow(length(x1)))
legend("topright", leg1, cex = 0.8, fill = rainbow(length(x1)))

x2 = summary(datos_2019_2_clc$ESTU_GENERO)
labels2 = round(100 * x2 / sum(x2), 2)
leg2 = names(summary(datos_2019_2_clc$ESTU_GENERO))
pie(x2, labels = labels2, radius = 1.4, main = "DISTRIBUCION POR GENERO 2019-II", col = rainbow(length(x2)))
legend("topright", leg2, cex = 0.8, fill = rainbow(length(x2)))


# c) DISTRIBUCION POR ESTRATO

x3 = summary(datos_2015_2_clc$FAMI_ESTRATOVIVIENDA)
x3_n = x3[2:length(x3)]
labels3 = round(100 * x3_n/ sum(x3_n), 2)
leg3 = names(summary(datos_2015_2_clc$FAMI_ESTRATOVIVIENDA))[2:length(x3)]
pie(x3_n, labels = labels3, radius = 1.4, main = "DISTRIBUCION POR ESTRATO 2015-II", col = rainbow(length(x3_n)))
legend("topright", leg3, cex = 0.8, fill = rainbow(length(x3_n)))

x4 = summary(datos_2019_2_clc$FAMI_ESTRATOVIVIENDA)
x4_n = x4[3:8]
labels4 = round(100 * x4_n / sum(x4_n), 2)
leg4 = names(summary(datos_2019_2_clc$FAMI_ESTRATOVIVIENDA))[3:8]
pie(x4_n, labels = labels4, radius = 1.4, main = "DISTRIBUCION POR GENERO 2019-II", col = rainbow(length(x4_n)))
legend("topright", leg4, cex = 0.8, fill = rainbow(length(x4_n)))


# d) PUNTAJES >= 300 Y PUNTAJES < 300

may1 = data.frame(filter(datos_2015_2_clc, PUNT_GLOBAL >= 300))
men1 = data.frame(filter(datos_2015_2_clc, PUNT_GLOBAL < 300))
len1 = dim(may1)[1]
len2 = dim(men1)[1]
x5 = c(len1, len2)
labels5 = round(100 * x5 / sum(x5), 2)
leg5 = c("Puntajes >= 300", "Puntajes < 300")
pie(x5, labels = labels5, radius = 1.4, main = "PUNTAJES GLOBALES 2015-II", col = rainbow(length(x5)))
legend("topright", leg5, cex = 0.65, fill = rainbow(length(x5)))
    
may2 = data.frame(filter(datos_2019_2_clc, PUNT_GLOBAL >= 300))
men2 = data.frame(filter(datos_2019_2_clc, PUNT_GLOBAL < 300))
len3 = dim(may2)[1]
len4 = dim(men2)[1]
x6 = c(len3, len4)
labels6 = round(100 * x6 / sum(x6), 2)
leg6 = c("Puntajes >= 300", "Puntajes < 300")
pie(x6, labels = labels6, radius = 1.4, main = "PUNTAJES GLOBALES 2019-II", col = rainbow(length(x6)))
legend("topright", leg6, cex = 0.65, fill = rainbow(length(x6)))


################################################################################
# 4) PRUEBAS CHI-SQUARE ########################################################

# NO SE REALIZARON YA QUE LOS RESULTADOS SON EVIDENTES


# a) PUNTAJES POR AREA 2015-II VS 2019-II

# conts1_area = maxs(datos_2015_2_clc)
E1_i = c(100434, 124661, 84707, 99285, 132144)
p1_i = E1_i / sum(E1_i)

random = sample(2:len19, len15, replace = F)
new_2019_2 = datos_2019_2_clc[random, ]
# conts2_area = maxs(new_2019_2)
n1_i = c(209895, 148474, 49880, 35247, 97735)

X1_2 = sum(((n1_i - E1_i) ^ 2) / E1_i)
proof1 = chisq.test(n1_i, p = p1_i)
gl1 = length(n1_i) - 1
pchisq(X1_2, df = gl1, lower.tail = FALSE)
qchisq(0.05, df = gl1, lower.tail = FALSE)

areas = c("LECTURA \n CRITICA", "MATEMATICAS", "CIENCIAS \n NATURALES",
          "SOCIALES \n CIUDADANAS", "INGLES")

labels7 = round(100 * E1_i/ sum(E1_i), 2)
pie(E1_i, labels = labels7, radius = 1.4, main = "DISTRIBUCION POR MEJOR MATERIA 2015-II", col = rainbow(length(E1_i)))
legend("topright", areas, cex = 0.7, fill = rainbow(length(E1_i)))

labels8 = round(100 * n1_i/ sum(n1_i), 2)
pie(n1_i, labels = labels8, radius = 1.4, main = "DISTRIBUCION POR MEJOR MATERIA 2019-II", col = rainbow(length(n1_i)))
legend("topright", areas, cex = 0.7, fill = rainbow(length(n1_i)))

