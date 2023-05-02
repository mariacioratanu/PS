plot_normal_density <- function(mean, sd) {
  x <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 1000)
  y <- dnorm(x, mean, sd)
  plot(x, y, type = "l", lwd = 2, xlab = "x", ylab = "Density")
}

plot_normal_density(0, 1)

