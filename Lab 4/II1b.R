set.seed(42)

# (1) Definim functia f(x)
f <- function(x) exp(x)

# (2) Calculam valoarea exacta a integralei
exact_val <- exp(4) - exp(1)

# (3) Generam n valori aleatoare uniform distribuite in [1, 4]
n <- 10000
x <- runif(n, min = 1, max = 4)

# (4) Calculam valorile corespunzatoare ale functiei f(x_i)
fx <- f(x)

# (5) Estimam valoarea integralei
est_val <- (4 - 1) * (1/n) * sum(fx)

# (6) Calculam eroarea absoluta
abs_error <- abs(est_val - exact_val)

# (7) Calculam eroarea relativa
rel_error <- abs_error / exact_val

# Afisam rezultatele
cat("Valoarea estimata a integralei este: ", est_val, "\n")
cat("Valoarea exacta a integralei este: ", exact_val, "\n")
cat("Eroarea absoluta este: ", abs_error, "\n")
cat("Eroarea relativa este: ", rel_error, "\n")

#Valoarea exactă a integralei este 51.87987, iar valoarea estimată cu metoda Monte Carlo
#este 51.91341. Asadar, observăm că valoarea estimată este ușor mai mare decât valoarea exactă.
#Eroarea absolută este 0.03354198, iar eroarea relativă este 0.0006465317. 
#Estimarea este destul de precisă, având în vedere faptul că eroarea
#relativă este foarte mică.

