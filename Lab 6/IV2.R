test_proportion =function(alfa, n, succese, p0, tip_ipoteza) {
  
  pprim = succese/n
  zscore = (pprim - p0)/sqrt(p0*(1 - p0)/n)
  
  # În funcție de tipul de ipoteză
  if (tip_ipoteza == "r") {
    criticalz = qnorm(1 - alfa, 0, 1)
    if (zscore < criticalz)
      print("HO nu a crescut")
    else 
      print("HO a crescut")
  }
  if (tip_ipoteza == "l") {
    criticalz = qnorm(alfa, 0, 1)
    if (zscore > criticalz)
      print("HO nu a crescut")
    else 
      print("HO a crescut")
  }
  if (tip_ipoteza == "s") {
    criticalz = qnorm(alfa, 0, 1)
    if (abs(zscore) <= criticalz)
      print("HO nu a crescut")
    else 
      print("HO a crescut")
  }
  
  cat("zscore =", zscore, "\n")
  cat("criticalz =", criticalz, "\n")
}

#Rezultatul:
test_proportion(0.05, 150, 20, 0.1,"r")