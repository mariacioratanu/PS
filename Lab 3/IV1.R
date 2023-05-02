binomial_probability <- function(n, p, k) {
  expectation <- n * p
  variance <- n * p * (1 - p)
  standard_deviation <- sqrt(variance)
  q <- (k - 0.5 - expectation) / standard_deviation
  return(pnorm(q))
}

# Setam valorile pentru n si p
n <- 10
p <- 0.5

# Initializam un vector pentru a stoca rezultatele
results <- numeric(n)

# Pentru fiecare valoare de k de la 1 la n, calculam P(X < k) si stocam rezultatul in vectorul 'results'
for (k in 1:n) {
  results[k] <- binomial_probability(n, p, k-1)
}

# Afisam rezultatele
cat("P(X < k) pentru k = 1:", results[1], "\n")
for (k in 2:n) {
  cat("P(X < k) pentru k =", k, ":", results[k], "\n")
}

#Explicatii:
#Folosim k - 0.5 ca să calculăm P(X < k), 
#iar pnorm calculează funcția de distribuție pentru valorile mai mici 
#decât argumentul dat. 
