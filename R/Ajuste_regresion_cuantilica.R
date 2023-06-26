#'Ajuste de regresión cuantílica
#'
#' Se encarga de hacer un ajuste en la funcion de régresion cuantílica, donde se podrá validar la interaccion de los cuantiles.
#'
#' @param peso: es un vector numerico con los pesos de los individuos de la variable independiente
#' @param edad: es un vector numerico con las edades de los individuos de la variable dependiente
#'
#' @return coeficientes de la regresion cuantil, errores estandar
#' @export
#Funcion
ajusta_rq <- function(peso, edad) {

  # Longitud de los cuantiles
  tau <- seq(0.1, 0.9, by = 0.11)
  veces <- length(tau)
  B <- matrix(NA, nrow = veces, ncol = 2)

  # Un "for" para calcular cada valor de cuantil y obtener los coeficientes
  library(quantreg)
  for (i in 1:veces) {
    rq_i <- rq(peso ~ edad, tau = tau[i], method = "br")
    B[i, ] <- coef(rq_i)
  }

  # Crear el dataframe con los coeficientes y los cuantiles
  resultados <- data.frame(B, tau)

  # Graficar la línea de regresión para cada cuantil
  plot(edad,peso,xlab="Edad", ylab="Peso", main="regresion cuantilica")
  for (i in 1:veces) {
    abline(a=B [i,1], b=B [i,2], col=i)
  print(plot)

  }

  # Devolver el resultado
  return(resultados)
}

