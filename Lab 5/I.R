set.seed(123) # setam un seed

simulate_finite_variable <- function(values, probabilities) {
  
  if (length(values) != length(probabilities)) { #verificăm dacă lungimile vectorilor sunt egale
    stop("Lungimea vectorilor trebuie să fie aceeași!") #arunca o eroare 
  }
  
  #verificăm dacă probabilitățile sunt valide
  if (sum(probabilities) != 1) {
    stop("Suma probabilitatilor trebuie sa fie 1.")
  }
  
  cum_probs <- cumsum(probabilities) #calculează probabilitățile cumulate utilizând funcția cumsum
  r <- runif(1) #generează un număr aleatoriu între 0 și 1 utilizând funcția runif
  return(values[which.min(cum_probs - r > 0)])
}

#which.min este utilizată pentru a identifica prima poziție a vectorului cum_probs - r care devine pozitivă și selectează valoarea corespunzătoare din vectorul values


#Testare:
#values - un vector care conține valorile variabilei aleatoare finită X
values <- c(1, 2, 3)
#probabilities - un vector care conține probabilitățile asociate fiecărei valori din X
probabilities <- c(0.2, 0.5, 0.3)

x <- simulate_finite_variable(values, probabilities)
cat("Valoarea aleatoare generata este:", x, "\n")



#O alta rezolvare posibila ar fi cu functia sample, astfel:

#simulate_finite_variable <- function(values, probabilities) {
#  sample(values, size = 1, prob = probabilities)
#}