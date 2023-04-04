grafic_geometric <- function(p, n) {
      probabilitati <- dgeom(1:n, prob = p) # calculează primele n probabilități
      barplot(probabilitati, names.arg = 1:n, xlab = "Termeni", ylab = "Probabilități", main = "Densitatea repartiției geometrice") # afișează graficul
}
#grafic_geometric(p = 0.3, n = 10)

