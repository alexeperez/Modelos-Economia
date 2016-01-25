#*-------- Correccion Prueba ----------------#


setwd("C:/Users/Toshiba/Desktop/Clases EPN/modelos en economia/Modelos-Economia/Pruebas")
library(readxl)
d <- read_excel("dataDA.xlsx", 
                sheet = 1,
                col_names = TRUE,
                na = "")
str(d)
# Corregir Deuda
d$Deuda <- as.numeric(d$Deuda)


# media variables numericas
media <- numeric(ncol(d))
for(i in 1:ncol(d)){
  media[i] <- mean(d[,i], na.rm = TRUE)
}
df1 <- data.frame(colnames(d), media)
df2 <- df1[!is.na(df1[,2]),]
orden <- order(df2[,2], 
               decreasing = TRUE)
df2[orden, ]


# suma Deuda en estado civil
d$estadocivil <- factor(d$estadocivil)
nivel <- levels(d$estadocivil)

suma <- numeric(length(nivel))
for (i in 1:length(nivel)){
df<-subset(d,subset=estadocivil==nivel[i])
suma[i] <- sum(df$Deuda, na.rm = TRUE)
}
suma
df_sum <- data.frame(nivel, suma)
df_sum

# Funcion en R
tapply(X = d$Deuda, 
       INDEX = d$estadocivil, 
       FUN = sum, 
       na.rm=TRUE)

# Recodificar Ingreso con 345 dólares.
recod <- function(x, val){
  x[is.na(x)] <- val
  return (x)
}
d$Ingreso<-recod(x = d$Ingreso, val = 345)



# regresión lineal simple entre Ingreso y endeudprom
# en los niveles dados por la variable Region
Rregs <- function(df){
  reg <- lm(Ingreso ~ endeudprom, df)
  objsumry <- summary(reg)
  R <- objsumry$r.squared
  return(R)
}
Rregs(d)

x <- mtcars[,3]
y <- mtcars[,1]
reg <- lm(y ~ x)
sumry <- summary(reg)
str(sumry)
sumry$r.squared




d$Region <- factor(d$Region)
nivel <- levels(d$Region)

Rcuad <- numeric(length(nivel))
for (i in 1:length(nivel)){
  df <- subset(d, subset=Region==nivel[i])
  Rcuad[i] <- Rregs(df)
}
Rcuad
df_Rcuad <- data.frame(nivel, Rcuad)
df_Rcuad

# Funcion en R
split_d <- split(x = d, f = d$Region)
str(split_d)
sapply(X = split_d, FUN = Rregs)



str(lapply(X = d, FUN = typeof))
str(sapply(X = d, FUN = typeof))
# Estudiar Funciones apply



