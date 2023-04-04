poisson_distribution <- function(lambda, n) {
  x <- 0:n
  prob <- dpois(x, lambda)
  barplot(prob, main=paste("Distributie Poisson cu lambda=",lambda), 
          xlab="x", ylab="Probabilitate", col="blue")
}