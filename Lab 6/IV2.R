# Setarea nivelului de semnificație
alpha <- 0.05

# Numărul total de componente testate
n <- 150

# Numărul de componente defecte
x <- 20

# Procentul estimat al componentelor defecte sub ipoteza nulă
p_null <- 0.10

# Calcularea valorii critice pentru testul unilateralei
crit_value <- qbinom(1 - alpha, n, p_null)

# Calcularea valorii p pentru testul unilateralei
p_value <- pbinom(x, n, p_null, lower.tail = FALSE)

# Afișarea rezultatelor
if (p_value <= alpha) {
  cat("Rezultatul testului: Procentul componentelor defecte este mai mare decât 10%.")
} else {
  cat("Rezultatul testului: Nu există suficiente dovezi că procentul componentelor defecte este mai mare decât 10%.")
}

cat("\n")
cat("Valoarea critică:", crit_value, "\n")
cat("Valoarea p:", p_value, "\n")

#Rezultate obtinute:
#Rezultatul testului: Nu există suficiente dovezi că procentul componentelor defecte este mai mare decât 10%
#Valoarea critică: 21 
#Valoarea p: 0.07208801 
