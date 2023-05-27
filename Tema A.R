# Definiți valorile pentru parametrii
lambda <- 2  # parametrul λ pentru distribuția Poisson
p <- 0.3  # parametrul p pentru distribuția geometrică
n <- 10  # parametrul n pentru distribuția binomială
k <- 5  # valoarea inițială k

# Calculați funcțiile de masă de probabilitate pentru distribuțiile Poisson, geometrică și binomială
poisson_probs <- dpois(k:n, lambda)
geometric_probs <- dgeom(k:n, p)
binomial_probs <- dbinom(k:n, n, p)

# Creați un cadru de date cu valorile și probabilitățile calculate
data <- data.frame(k:n, poisson_probs, geometric_probs, binomial_probs)
names(data) <- c("x", "Poisson", "Geometric", "Binomial")

# Trasați graficul utilizând funcțiile de bază din R
plot(data$x, data$Poisson, type = "o", col = "pink", xlab = "x", ylab = "Probabilitate",
     main = "Funcțiile de masă de probabilitate")
lines(data$x, data$Geometric, type = "o", col = "purple")
lines(data$x, data$Binomial, type = "o", col = "blue")
legend("topright", legend = c("Poisson", "Geometric", "Binomial"),
       col = c("pink", "purple", "blue"), lty = 1, cex = 0.8)
