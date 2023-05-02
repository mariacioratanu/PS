verify_CLT_Gamma <- function(alpha, lambda, n, N, z) {
  
  # Generate n Gamma distributed random variables
  X <- rgamma(n, shape = alpha, rate = lambda)
  
  # Calculate sample mean and standard deviation
  mu_hat <- mean(X)
  sigma_hat <- sd(X)
  
  # Calculate expected mean and standard deviation
  mu <- alpha/lambda
  sigma <- sqrt(alpha/(lambda^2))
  
  # Calculate the standard error
  SE <- sigma/sqrt(n)
  
  # Calculate the confidence interval
  CI <- c(mu_hat - qnorm(1 - z/2)*SE, mu_hat + qnorm(1 - z/2)*SE)
  
  # Generate N samples and calculate the sample means
  sample_means <- replicate(N, mean(rgamma(n, shape = alpha, rate = lambda)))
  
  # Plot the sample means and the normal distribution
  hist(sample_means, breaks = 30, freq = FALSE, main = "Histogram of Sample Means", 
       xlab = "Sample Means", ylim = c(0, 1.2*dnorm(mu_hat, mu, SE)), col = "purple")
  curve(dnorm(x, mu, SE), add = TRUE, col = "pink", lwd = 2)
  
  # Add a legend
  legend("topright", legend = c("Normal Distribution", "Sample Means"), 
         col = c("pink", "purple"), lwd = c(2, 10))
  
  # Print the sample mean, expected mean, sample standard deviation, expected standard deviation
  cat("Sample Mean:", mu_hat, "\n")
  cat("Expected Mean:", mu, "\n")
  cat("Sample Standard Deviation:", sigma_hat, "\n")
  cat("Expected Standard Deviation:", sigma, "\n")
  cat("Confidence Interval:", CI, "\n")
  
}

#verify_CLT_Gamma(alpha = 2, lambda = 1, n = 50, N = 10000, z = 0)

# Setarea parametrilor
n <- 50
N_vals <- c(5000, 10000, 20000)
z_vals <- c(-1.5, 0, 1.5)

# Ruleaza functia pentru fiecare combinatie de N si z
for (N in N_vals) {
  for (z in z_vals) {
    verify_CLT_Gamma(alpha = 2, lambda = 1, n = n, N = N, z = z)
  }
}
