CLT_gamma <- function(alpha, lambda, n, N, z) {
  expectation <- alpha / lambda
  st_dev <- sqrt(alpha) / lambda
  upper_bound <- z * st_dev / sqrt(n) + expectation
  sum <- 0
  for (i in 1:N) {
    x_n <- mean(rgamma(n, alpha, lambda))
    if (x_n <= upper_bound) {
      sum <- sum + 1
    }
  }
  return(c(sum/N, pnorm(z)))
}

#Setam valorile pentru alpha si lambda
alpha <- 2
lambda <- 3

#Setam valorile pentru n, N si z
n <- 50
N <- c(5000, 10000, 20000)
z <- c(-1.5, 0, 1.5)

#Apelam functia pentru fiecare combinatie de N si z si afisam rezultatele
for (i in 1:length(N)) {
  for (j in 1:length(z)) {
    result <- CLT_gamma(alpha, lambda, n, N[i], z[j])
    cat("P(Xbar <=", result[2], ") pentru N =", N[i], "si z =", z[j], ":", result[1], "\n")
  }
}




