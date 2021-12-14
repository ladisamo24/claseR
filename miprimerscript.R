#library
library(tidyverse)

mi_primera_funcion = function(parametro1,parametro2){
  
  if(is.numeric(parametro1)&is.numeric(parametro2)){
    
    conteo = parametro1+parametro2
    conteo
    
  }
  else{
    
    mensaje = "no son variables numéricas"
    mensaje
    
  }
}

mi_primera_funcion(parametro1 = 10,parametro2 = 8) 

mi_primera_funcion(parametro1 = "hola",parametro2 = 8) 



#funcion de cadena de Fibonacci

serie_fibonacci = function(longitud)
{
  fibonacci = 1
  fibonacci[1] = 1
  
  if (longitud > 1)
  {
    fibonacci[2] = 1
    
    if (longitud > 2)
    {
      for (i in 3:longitud)
      {
        fibonacci[i] = fibonacci[i-2] + fibonacci[i-1]
      }
    }
  }
  fibonacci
}

# Se aplica la funcion serie_fibonacci para generar 6 elementos de la serie 

serie_fibonacci(6)









