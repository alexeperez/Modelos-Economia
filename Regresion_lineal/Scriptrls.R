#-----------------------------------------------------#
#               Modelos en Economia                   #
# Alex E. Pérez 
# MS-PLUS Consultores
# http://www.consultoresmatematicos.com
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

# Regresion lineal
reg1 <- lm(utilidad ~ ventas, data=datarls)
summary(reg1)

# ANOVA
anova1 <- aov(reg1)
summary(anova1)

# Objeto reg1
str(reg1)
names(reg1)

# Residuos
u_t <- reg1$residuals

# Hipotesis sobre los errores
mean(u_t)
hist(u_t)

# Pronosticos o predicciones
y_t <- reg1$fitted.values

datafinal <- data.frame(datarls, y_t)
View(datafinal)

# Recta de regresion
library(ggplot2)
g <- ggplot(data = NULL, aes(x=ventas, y=utilidad))
g + geom_point(size=3, color="black")+
  geom_smooth(method="lm", color="red")


# DEBER
# Ajustar un modelo de regresion lineal simple entre
# pib e inflacion del archivo data_rls_pib.xlsx
