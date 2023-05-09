set.seed(123)  # setăm o valoare fixă pentru a putea reproduce rezultatele

N <- 100000  # numărul de valori generate
lambda_1 <- 4  # parametrul λ pentru primul mecanic
lambda_2 <- 12  # parametrul λ pentru al doilea mecanic
p <- 0.75  # probabilitatea de a fi servit de primul mecanic

# funcția pentru generarea timpului de servire pentru un mecanic
generate_service_time <- function(lambda) {
  rexp(1, lambda)
}

# generăm valori aleatoare pentru timpul de servire
service_times <- numeric(N)
for (i in 1:N) {
  if (runif(1) < p) {
    service_times[i] <- generate_service_time(lambda_1)
  } else {
    service_times[i] <- generate_service_time(lambda_2) / 3
  }
}

# estimăm media timpului de servire
mean_service_time <- mean(service_times)


# afisăm rezultatele
cat("Media lui X este:", mean_service_time)

#Media lui X este: 0.194486
