calcular_epsilon <- function() {
  epsilon <- 1.0
  while ((1.0 + epsilon / 2) > 1.0) {
    epsilon <- epsilon / 2
  }
  return(epsilon)
}

epsilon_calculado <- calcular_epsilon()

cat("Epsilon de la maquina (calculado iterativamente):",
    format(epsilon_calculado, scientific = TRUE, digits = 16), "\n")
cat("Valor de referencia (.Machine$double.eps):        ",
    format(.Machine$double.eps, scientific = TRUE, digits = 16), "\n")
cat("Diferencia entre ambos valores:                    ",
    abs(epsilon_calculado - .Machine$double.eps), "\n")


