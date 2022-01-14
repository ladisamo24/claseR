

#librerias 
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)

#llamar bases de datos 
Vendedores = read_csv("Data/Vendedores.csv")
Ventas = read_excel("Data/Ventas.xlsx")

#Estructura de las variables
str(Ventas) #indica el tipo de variables de las columnas
str(Vendedores) #indica el tipo de variables de las columnas

#Visualizar el dataset de ventas
View(Ventas)
View(Vendedores)

head(Ventas, 3) #para ver las primeras filas del dataset
var = head(Ventas, 3) #para crear una tabla

summary(Ventas) #resumen de todas las variables que hay en el dataset


Ventas$Mes #para ver la columna "Mes" y todas las filas 
mes = Ventas[,1]  #para ver la columna "Mes" y todas las filas
mes = Ventas[,"Mes"] #para ver la columna "Mes" y todas las filas

fila = Ventas[1,] #Para ver la primera fila y todas las columnas

#TRATAMIENTO DE DATOS

#convertir una variable a factor
Ventas$`Código vendedor`= as.factor(Ventas$`Código vendedor` )
Vendedores$Codigo = as.factor(Vendedores$Codigo)

Ventas$Tienda  #mostrar los datos de la columna "Tienda"
Ventas$Tienda[1:10]  #valores de la columna tienda de la fila 1 a 10
Ventas[1:10, "Tienda"] #valores de la columna tienda de la fila 1 a 10

ejercicio = Ventas[Ventas$Mes < 7 & Ventas$Producto == "Radio",  ]


#agregar columnas al dataset
Ventas$"Tipo de compra" = ifelse(Ventas$`Monto venta` >1000,1,0)
summary(Ventas$`Tipo de compra`)

Ventas$Utilidad = Ventas$`Monto venta`*0.85

#agregar una columna correspondiente al nombre del vendedor
Ventas = merge(Ventas,Vendedores,by.x = "Código vendedor",by.y = "Codigo", all.x = TRUE)


#Limpieza de datos

#verificar que las variables de edad y mes estén dentro de un rango lógico
summary(Ventas)

Ventas = Ventas[Ventas$Mes > 0 & Ventas$Edad > 0,]

#verificar que las variables categóricas no tengan datos de más
table(Ventas$Compra)
Ventas = Ventas[Ventas$Compra != 2 ,]

#verficar que no hay datos faltantes
Ventas = na.omit(Ventas)

#eliminar columnas(variables) que no aportan o que no deberían estar ahí
Ventas = Ventas[,-c(1,11)]
Ventas$`Código vendedor` = NULL
Ventas$`Punto de red` = NULL


#elimino datos atípicos(outliers) de la variable "Monto venta"
Ventas = Ventas[Ventas$`Monto venta`< 6000, ]


#Análisis exploratorio de los datos EDA


boxplot(Ventas$`Monto venta`,col = "blue",main = "Montos",horizontal = TRUE) 
#elimino datos atípicos(outline) de la variable "Monto venta"
boxplot(Ventas$`Monto venta`,outline = FALSE,col = "blue",main = "Montos Ventas",horizontal = TRUE) 

#Gráfico de caja para la variable "Edad"
boxplot(Ventas$Edad,col = "red",main = "Edades")

#histograma 
hist(Ventas$Utilidad,col = "grey",ylab = "Frecuencia de datos")
hist(Ventas$`Monto venta`,col = "grey",ylab = "Frecuencia de datos")

#correlación
#cor(Utilidad, Edad)
cor(Ventas$Edad, Ventas$`Monto venta`)

plot(Ventas$Edad,Ventas$`Monto venta`,col = "red") #para crear un gráfico

#nrow(Ventas) #número de filas del dataset Ventas
#ncol(Ventas) #número de columnas del dataset Ventas

# media, mediana y desvest de la variable "Monto venta"
mean(Ventas$`Monto venta`)
median(Ventas$`Monto venta`)
sd(Ventas$`Monto venta`)

# media, mediana y desvest de la variable "Edad"
mean(Ventas$Edad)
median(Ventas$Edad)
sd(Ventas$Edad)

# media, mediana y desvest de la variable "Utilidad"
mean(Utilidad)
median(Utilidad)
sd(Utilidad)

################################################################

#EDA por medio de visualización

#Gráficos en R con ggplot2
# install.packages("ggplot2")
# library(ggplot2)

ggplot(data = Ventas,aes(Compra)) + geom_bar(aes(fill = factor(Género)))

ggplot(data = Ventas,aes(Producto)) + geom_bar(aes(fill = factor(Frecuencia)))







