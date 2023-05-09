# (1) Definim functia f(x)
f <- function(x) 1 / (4 * x^2 - 1)

# (2) Calculam valoarea exacta a integralei
#M <- 1000000
#exact_val <- 2 * (atanh(sqrt(M^2 - 1) / (2 * M)) - atanh(1/2))
exact_val <- log(3/4)

# (3) Generam n valori aleatoare uniform distribuite in [1, M]
set.seed(42)
n <- 10000
x <- runif(n, min = 1, max = M)

# (4) Calculam valorile corespunzatoare ale functiei f(x_i)
fx <- f(x)

# (5) Estimam valoarea integralei
est_val <- 2 * (M - 1) * (1/n) * sum(fx)

# (6) Calculam eroarea absoluta
abs_error <- abs(est_val - exact_val)

# (7) Calculam eroarea relativa
rel_error <- abs_error / abs(exact_val)

# Afisam rezultatele
cat("Valoarea estimata a integralei este: ", est_val, "\n")
cat("Valoarea exacta a integralei este: ", exact_val, "\n")
cat("Eroarea absoluta este: ", abs_error, "\n")
cat("Eroarea relativa este: ", rel_error, "\n")

# (8) Comparam valoarea estimata cu valoarea exacta
cat("Comparatie cu valoarea exacta: \n")
if(abs(est_val - exact_val) < 0.0001){
  cat("Estimarea este apropiata de valoarea exacta. \n")
} else {
  cat("Estimarea difera semnificativ de valoarea exacta. \n")
}

#Valoarea exactă a integralei este -0.2876821, iar valoarea estimată
#este 0.003382541. Asadar, observăm că valoarea estimată este ușor mai mare decât valoarea exactă.
#Eroarea absolută este 0.2910646, iar eroarea relativă este 1.011758. 
#Asadar, estimarea difera semnificativ de valoarea exacta. 
