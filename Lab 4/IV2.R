set.seed(42)

# (a) Simularea infectarii tuturor calculatoarelor
simulate_one_day <- function(n, k, p) {
  infected <- rep(0, n)
  infected[1:k] <- 1
  while (sum(infected) < n) {
    for (i in 1:n) {
      if (infected[i] == 1) {
        infected[which(infected == 0)] <- rbinom(length(which(infected == 0)), 1, p)
      }
    }
  }
  return(all(infected == 1))
}

estimate_prob_a <- function(n, k, p, num_trials) {
  successes <- sum(replicate(num_trials, simulate_one_day(n, k, p)))
  return(successes / num_trials)
}

prob_a <- estimate_prob_a(n = 40, k = 1, p = 0.2, num_trials = 10000)
cat("Probabilitatea ca toate cele 40 de calculatoare sa fie infectate, pornind de la o singura infectare este: ", prob_a, "\n")

# (b) Simularea infectarii a cel putin 15 calculatoare
estimate_prob_b <- function(n, k, p, num_trials, num_infected) {
  successes <- sum(replicate(num_trials, sum(replicate(1, simulate_one_day(n, k, p))) >= num_infected))
  return(successes / num_trials)
}

prob_b <- estimate_prob_b(n = 40, k = 1, p = 0.2, num_trials = 10000, num_infected = 15)
cat("Probabilitatea ca cel putin 15 din cele 40 de calculatoare sa fie infectate, pornind de la o singura infectare este: ", prob_b, "\n")

# (c) Calcularea intervalului de incredere pentru a)
estimate_prob_c <- function(n, k, p, num_trials, num_infected, error, confidence) {
  estimate_prob <- estimate_prob_a(n, k, p, num_trials)
  se <- sqrt(estimate_prob * (1 - estimate_prob) / num_trials)
  z <- qnorm((1 + confidence) / 2)
  ci_lower <- max(0, estimate_prob - z * se - error)
  ci_upper <- min(1, estimate_prob + z * se + error)
  return(list(p_hat = estimate_prob, ci_lower = ci_lower, ci_upper = ci_upper))
}

result_c <- estimate_prob_c(n = 40, k = 1, p = 0.2, num_trials = 10000, num_infected = 40, error = 0.01, confidence = 0.95)
cat("Intervalul de incredere pentru probabilitatea ca toate cele 40 de calculatoare sa fie infectate, pornind de la o singura infectare este: [", result_c$ci_lower, ", ", result_c$ci_upper, "]\n")
