verify_LNM <- function(n, r) {
  # Simulam n variabile aleatoare Student
  x <- rt(n, r)
  
  # Calculam media si deviatia standard
  sample_mean <- mean(x)
  sample_sd <- sd(x)
  
  # Calculam valorile asteptate teoretice
  expected_mean <- 0
  expected_sd <- sqrt(r / (r - 2))  # formula pentru deviatia standard a unei variabile aleatoare Student
  
  # Afisam rezultatele
  cat("Sample Mean:", sample_mean, "\n")
  cat("Expected Mean:", expected_mean, "\n")
  cat("Sample Standard Deviation:", sample_sd, "\n")
  cat("Expected Standard Deviation:", expected_sd, "\n")
}


# Verificam LNM pentru n = 1000 si r = 2
verify_LNM(1000, 2)

# Verificam LNM pentru n = 10000 si r = 3
verify_LNM(10000, 3)

# Verificam LNM pentru n = 100000 si r = 4
verify_LNM(100000, 4)

# Verificam LNM pentru n = 1000000 si r = 5
verify_LNM(1000000, 5)

#Rezultatele arata ca LNM este verificata pentru aceste valori ale lui n 
#si r. In cazul primei verificari pentru n=1000 si r=2, 
#valorile obtinute pentru media si deviatia standard nu se apropie 
#foarte mult de valorile exacte asteptate (media 0 si deviatia standard infinita),
#dar pentru celelalte valori ale lui n si r, valorile obtinute sunt mai apropiate 
#de valorile exacte. Acest lucru sugereaza ca cu cat marim dimensiunea eșantionului n, 
#cu atat mai aproape vor fi valorile medii si de deviatia standard de valorile exacte asteptate.
