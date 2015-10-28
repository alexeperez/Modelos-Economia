#-----------------------------------------------------#
#               Modelos en Economia
# Alex E. Pérez 
# Escuela Politécnica Nacional
#-----------------------------------------------------#

# directorio de trabajo
setwd("C:/Users/Toshiba/Desktop/Clases EPN/modelos en economia/Modelos-Economia/Regresion_lineal")
list.files()

# paquete lectura de archivos desde excel
install.packages("readxl", dependencies = TRUE)
library(readxl)

# data_rls_uti.xlsx
datarls <- read_excel("data_rls_uti.xlsx", sheet = 1, col_names = TRUE, na = "")
View(datarls)
str(datarls)
colnames(datarls)
summary(datarls)

# grafico de dispersion
# Variables
utilidad <- datarls[,"Utilidad"]
ventas <- datarls[,"Ventas"]

plot(x = ventas,y = utilidad)
plot(x = ventas,y = utilidad,main = "Utilidad vs Ventas")
plot(x = ventas,y = utilidad,main = "Utilidad vs Ventas", pch=16)
plot(x = ventas,y = utilidad,main = "Utilidad vs Ventas",
     pch=16, col="blue")

# ggplot2
install.packages(ggplot2, dependencies = TRUE)
library(ggplot2)
g <- ggplot(data = NULL, aes(x=ventas, y=utilidad))
g + geom_point(size=4, color="black")

# Correlacion
cor(x = utilidad, y= ventas)

#-----------------------------------------------------#
#              Regresion lineal Simple

regs <- lm(utilidad ~ ventas)
summary(regs)

# ANOVA
anovas <- aov(regs)
summary(anovas)

# Objeto regs
str(regs)
names(regs)

# Residuos
u_t <- regs$residuals

# Hipotesis sobre los errores
mean(u_t)
hist(u_t)

# Pronosticos o predicciones
y_t <- regs$fitted.values

datafinal <- data.frame(datarls, y_t)
View(datafinal)

# Recta de regresion
library(ggplot2)
g <- ggplot(data = NULL, aes(x=ventas, y=utilidad))
g + geom_point(size=3, color="black")+
  geom_smooth(method="lm", color="red")


# DEBER
# Ajustar un modelo de regresion lineal simple entre
# ln(pib) e ln(inflacion) del archivo data_rls_pib.xlsx
# use la funcion log() para extraer el log natural de una variable


#-----------------------------------------------------#
#              Regresion lineal multiple

# data_rls_uti.xlsx
datarlm <- read_excel("data_rlm_pib.xlsx", sheet = 1, col_names = TRUE, na = "")
View(datarlm)
str(datarlm)
colnames(datarlm)
summary(datarlm)

# Variables
pib <- datarlm[,"pib"]
infl <- datarlm[,"inflacion"]
ee <- datarlm[,"ee"]

# grafico de dispersion
# variable inflacion
plot(x = infl,y = pib,main = "pib vs inflacion",
     pch=16, col="blue")

# variable ee
plot(x = ee,y = pib,main = "pib vs ee",
     pch=16, col="blue")

# Relacion no lineal correccion
# transformar variables
ln_pib <- log(pib)
ln_infl <- log(infl)
ln_ee <- log(ee)

plot(x = ln_infl, y = ln_pib,main = "ln_pib vs ln_infl",
     pch=16, col="blue")

plot(x = ln_ee,y = ln_pib, main = "pib vs ee",
     pch=16, col="blue")


regm <- lm(ln_pib ~ ln_infl+ ln_ee)
summary(regm)

# ANOVA
anovam <- aov(regm)
summary(anovam)

# Objeto regm
names(regm)

# Pronosticos o predicciones
y_t <- regm$fitted.values

datafinal <- data.frame(datarlm,ln_pib, y_t)
View(datafinal)


