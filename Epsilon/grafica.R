# =========================================================================
# Recta numerica de la maquina: representables desde la definicion formal
# y = m * beta^(e - t),  con beta^(t-1) <= m <= beta^t - 1  (normalizado)
# Basado en Higham (2002), ec. 2.1, y Chapra & Canale (2006), ejemplo 3.4
# =========================================================================

representables <- function(beta, t, emin, emax) {
  # Genera todos los numeros representables positivos del sistema F
  valores <- c(0.0)
  for (e in emin:emax) {
    for (m in beta^(t - 1):(beta^t - 1)) {
      y <- m * beta^(e - t)
      valores <- c(valores, y)
    }
  }
  sort(unique(valores))
}

# --- Sistema juguete (el mismo del ejemplo 3.4 de Chapra) ---
beta <- 2
t    <- 3
emin <- -1
emax <- 3

vals <- representables(beta, t, emin, emax)
cat("Numeros representables:\n")
print(vals)

# --- Epsilon de la maquina: distancia de 1.0 al siguiente representable ---
idx_uno   <- which(vals == 1.0)
eps_medido  <- vals[idx_uno + 1] - vals[idx_uno]
eps_teorico <- beta^(1 - t)

cat("\nEpsilon medido directamente de la lista:", eps_medido, "\n")
cat("Epsilon teorico (beta^(1-t)):            ", eps_teorico, "\n")

# =========================================================================
# Graficar la recta numerica discreta
# =========================================================================

plot(vals, rep(0, length(vals)),
     pch = 19, col = "steelblue",
     yaxt = "n", ylab = "",
     xlab = "Valor representable",
     main = paste0("Sistema de punto flotante (beta=", beta, ", t=", t, ")"),
     ylim = c(-0.3, 0.5))

abline(h = 0, col = "gray70")

# Resaltar el hueco epsilon entre 1.0 y el siguiente representable
x1 <- vals[idx_uno]
x2 <- vals[idx_uno + 1]
arrows(x1, 0.15, x2, 0.15, col = "red", code = 3, length = 0.08)
text((x1 + x2) / 2, 0.28, labels = "epsilon", col = "red")

points(x1, 0, col = "red", pch = 19, cex = 1.4)
points(x2, 0, col = "red", pch = 19, cex = 1.4)