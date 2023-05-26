                             #Tema B

#B1


#Rezolvare si apelare:


  #Definirea functiei pentru calculul LNM pentru un sir de variabile aleatoare Geometrice
  calculeaza_LNM <- function(n, p) {
  #Generarea sirului de variabile aleatoare Geometrice
  variabile_aleatoare <- rgeom(n, p)
  
  #Calcularea mediei pentru fiecare pas
  medii <- cumsum(variabile_aleatoare) / seq_along(variabile_aleatoare)
  
  # Calculul Sample Mean (Media eșantionului)
  sample_mean <- mean(variabile_aleatoare)
  
  #Calculul Expected Mean (Media așteptată)
  expected_mean <- 1 / p
  
  #Calculul Sample Standard Deviation (Deviația standard a eșantionului)
  sample_sd <- sd(variabile_aleatoare)
  
  #Calculul Expected Standard Deviation (Deviația standard așteptată)
  expected_sd <- sqrt((1 - p) / (p^2))
  
  #Returnarea vectorului cu mediile și celelalte mărimi calculate
  return(list(medii = medii, sample_mean = sample_mean, expected_mean = expected_mean, sample_sd = sample_sd, expected_sd = expected_sd))
}

  #Valori pentru n și p
  n_valori <- c(5000, 10000, 100000, 500000)
  p_valori <- c(0.2, 0.6, 0.6, 0.8)

  #Calculul și compararea rezultatelor
  for (i in 1:length(n_valori)) {
  n <- n_valori[i]
  p <- p_valori[i]
  
  #Calculul mediilor și celorlalte mărimi pentru sirul de variabile aleatoare Geometrice
  rezultate <- calculeaza_LNM(n, p)
  medii_calculate <- rezultate$medii
  sample_mean <- rezultate$sample_mean
  expected_mean <- rezultate$expected_mean
  sample_sd <- rezultate$sample_sd
  expected_sd <- rezultate$expected_sd
  
  #Calculul mediei exacte
  medie_exacta <- 1 / p
  
  #Compararea rezultatelor
  eroare <- abs(medii_calculate[n] - medie_exacta)
  
  #Afisarea rezultatelor intr-un format mai frumos
  cat("Pentru n =", n, "și p =", p, ":\n")
  cat("Media calculată:", medii_calculate[n], "\n")
  cat("Media eșantionului:", sample_mean, "\n")
  cat("Media așteptată:", expected_mean, "\n")
  cat("Deviația standard a eșantionului:", sample_sd, "\n")
  cat("Deviația standard așteptată:", expected_sd, "\n")
  cat("Media exactă:", medie_exacta, "\n")
  cat("Eroarea:", eroare, "\n")
  cat("\n")
}


#Rezultatele obtinute:

#Pentru n = 5000 și p = 0.2 :
#Media calculată: 4.0224 
#Media eșantionului: 4.0224 
#Media așteptată: 5 
#Deviația standard a eșantionului: 4.551356 
#Deviația standard așteptată: 4.472136 
#Media exactă: 5 
#Eroarea: 0.9776 

#Pentru n = 10000 și p = 0.6 :
#Media calculată: 0.672 
#Media eșantionului: 0.672 
#Media așteptată: 1.666667 
#Deviația standard a eșantionului: 1.058172 
#Deviația standard așteptată: 1.054093 
#Media exactă: 1.666667 
#Eroarea: 0.9946667 

#Pentru n = 1e+05 și p = 0.6 :
#Media calculată: 0.66568 
#Media eșantionului: 0.66568 
#Media așteptată: 1.666667 
#Deviația standard a eșantionului: 1.052787 
#Deviația standard așteptată: 1.054093 
#Media exactă: 1.666667 
#Eroarea: 1.000987 

#Pentru n = 5e+05 și p = 0.8 :
#Media calculată: 0.249912 
#Media eșantionului: 0.249912 
#Media așteptată: 1.25 
#Deviația standard a eșantionului: 0.5592 
#Deviația standard așteptată: 0.559017 
#Media exactă: 1.25 
#Eroarea: 1.000088 



#B2


#Rezolvare si apelare:
  
  
  #Definim funcția pentru a verifica TLC pentru variabilele Student
  verifica_TLC <- function(r, n, N, z) {
  #Generăm un eșantion de variabile aleatoare Student
  eșantion <- rt(N, r)
    
  #Calculăm media și deviația standard a eșantionului
  media_eșantion <- mean(eșantion)
  deviația_standard_eșantion <- sd(eșantion)
    
  #Calculăm valorile teoretice așteptate pentru media și deviația standard
  media_teorie <- 0
  deviația_standard_teorie <- sqrt(r / (r - 2))
    
  #Calculăm erorile absolute pentru media și deviația standard
  eroare_absolută_media <- abs(media_eșantion - media_teorie)
  eroare_absolută_deviație_standard <- abs(deviația_standard_eșantion - deviația_standard_teorie)
    
  #Verificăm dacă eșantionul respectă condițiile TLC
  if (abs(media_eșantion - media_teorie) < z * deviația_standard_teorie / sqrt(n) &&
      abs(deviația_standard_eșantion - deviația_standard_teorie) < z * deviația_standard_teorie / sqrt(2 * (n - 1))) {
      cat("TLC este valid pentru r =", r, ", n =", n, ", N =", N, "și z =", z, "\n")
    } else {
      cat("TLC nu este valid pentru r =", r, ", n =", n, ", N =", N, "și z =", z, "\n")
    }
    
  #Afișăm erorile absolute pentru media și deviația standard
  cat("Eroare absolută pentru media:", eroare_absolută_media, "\n")
  cat("Eroare absolută pentru deviația standard:", eroare_absolută_deviație_standard, "\n\n")
  }
  
  #Specificăm valorile pentru n, N și z
  n <- 50
  N <- c(5000, 10000, 20000)
  z <- c(-1.5, 0, 1.5)
  
  #Specificăm valoarea pentru parametrul r
  r <- 5
  
  #Calculăm erorile absolute pentru fiecare combinație de n, N și z
  for (i in 1:length(N)) {
    for (j in 1:length(z)) {
      verifica_TLC(r, n, N[i], z[j])
    }
  }
  
#Rezultatele obtinute:
  
#TLC nu este valid pentru r = 5 , n = 50 , N = 5000 și z = -1.5 
#Eroare absolută pentru media: 0.02155276 
#Eroare absolută pentru deviația standard: 0.001194215 
 
#TLC nu este valid pentru r = 5 , n = 50 , N = 5000 și z = 0 
#Eroare absolută pentru media: 0.0295291 
#Eroare absolută pentru deviația standard: 0.0159669 
 
#TLC este valid pentru r = 5 , n = 50 , N = 5000 și z = 1.5 
#Eroare absolută pentru media: 0.01772764 
#Eroare absolută pentru deviația standard: 0.01918784 

#TLC nu este valid pentru r = 5 , n = 50 , N = 10000 și z = -1.5 
#Eroare absolută pentru media: 0.009823902 
#Eroare absolută pentru deviația standard: 0.01055081 
  
#TLC nu este valid pentru r = 5 , n = 50 , N = 10000 și z = 0 
#Eroare absolută pentru media: 0.007482244 
#Eroare absolută pentru deviația standard: 0.003099599 
 
#TLC este valid pentru r = 5 , n = 50 , N = 10000 și z = 1.5 
#Eroare absolută pentru media: 0.01349201 
#Eroare absolută pentru deviația standard: 0.01270358 
 
#TLC nu este valid pentru r = 5 , n = 50 , N = 20000 și z = -1.5 
#Eroare absolută pentru media: 0.006151826 
#Eroare absolută pentru deviația standard: 0.007396225 
 
#TLC nu este valid pentru r = 5 , n = 50 , N = 20000 și z = 0 
#Eroare absolută pentru media: 0.01152797 
#Eroare absolută pentru deviația standard: 0.01001774 
 
#TLC este valid pentru r = 5 , n = 50 , N = 20000 și z = 1.5 
#Eroare absolută pentru media: 0.0009987884 
#Eroare absolută pentru deviația standard: 0.00253344 

  
  
#B3
  
  
#Rezolvare si apelare:

  moivre_laplace_approx <- function(n, p, h, k) {
    # Calculam media si deviatia standard a distributiei binomiale
    mean <- n * p
    sd <- sqrt(n * p * (1 - p))
    
    # Aproximam probabilitatea folosind teorema Moivre-Laplace
    prob <- pnorm(k, mean, sd) - pnorm(h, mean, sd)
    
    return(prob)
  }
  
  #Exemplu de utilizare
  n <- 100 #Numărul de încercări
  p <- 0.3 #Probabilitatea de succes
  h <- 20  #Valoarea minimă
  k <- 40  #Valoarea maximă
  
  moivre_laplace_approximation(n, p, h, k) #Rezultat: 0.9709037
  
  #Un exemplu de estimare, a probabilității P(30 <= X < 50) pentru o variabilă aleatoare X cu distribuție binomială B(100, 0.5).
  moivre_laplace_approx(n=100, p=0.5, h=30, k=50)
  
  #alte exemple de estimari
  moivre_laplace_approx(n=100, p=0.5, h=30, k=70)
  moivre_laplace_approx(n=500, p=0.3, h=100, k=150)
  moivre_laplace_approx(n=1000, p=0.2, h=150, k=300)
  
  
#Rezultate obtinute pentru exemplele alese, in ordine:
#0.4999683
#0.9999367
#0.4999995
#0.9999614
  