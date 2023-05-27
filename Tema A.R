                             #Tema A


#A1


#Rezolvare si apelare:

#a)

reprezinta_functii_probabilitate <- function(lambda, p, n, k) {
  x <- k:n  #Valorile pentru care se calculează funcțiile de probabilitate
  
  #Calcularea funcției de masă de probabilitate pentru distribuția Poisson(λ)
  prob_poisson <- dpois(x, lambda)
  
  #Calcularea funcției de masă de probabilitate pentru distribuția Geometric(p)
  prob_geometric <- dgeom(x, prob = p)
  
  #Calcularea funcției de masă de probabilitate pentru distribuția B(n, p)
  prob_binomial <- dbinom(x, size = n, prob = p)
  
  #Crearea graficului cu cele trei funcții de probabilitate
  barplot(rbind(prob_poisson, prob_geometric, prob_binomial), 
          beside = TRUE, col = c("pink", "purple", "grey"),
          names.arg = x, xlab = "Termeni", ylab = "Probabilități",
          main = "Funcțiile de masă de probabilitate",
          legend.text = c("Poisson", "Geometric", "Binomial"))
}

#Exemplu de utilizare pentru lambda = 2, p = 0.3, n = 10, k = 3
reprezinta_functii_probabilitate(lambda = 2, p = 0.3, n = 10, k = 3)


#b)

#Parametrii
p <- 0.5  #Exemplu: p = 0.5
k <- 2    #Exemplu: k = 2
n <- 10   #Exemplu: n = 10

#Calcul probabilități
prob_impar <- sum(dgeom(seq(1, 1000, 2), prob = p))
prob_mare_egal_4 <- sum(dgeom(seq(4, 1000), prob = p))
prob_mic_egal_20 <- sum(dgeom(seq(0, 20), prob = p))

#Afișare rezultate
print(paste("P(X = impar) =", prob_impar))
print(paste("P(X >= 4) =", prob_mare_egal_4))
print(paste("P(X <= 20) =", prob_mic_egal_20))


#Rezultatele obtinute:

#P(X = impar) = 0.333333333333333 (aproximativ 1/3)
#P(X >= 4) = 0.0625 (1/16))
#P(X <= 20) = 0.999999523162842  (aproape 1)


#c)

#Definim funcția pentru determinarea celei mai mici valori a lui k0
poisson_smallest_k0 <- function(lambda) {
  k0 <- 0  #Valoarea inițială a lui k0
  
  #Iterăm prin valorile lui k0 până când probabilitatea devine mai mică decât 10^(-7)
  while (ppois(k0, lambda, lower.tail = FALSE) >= 10^(-7)) {
    k0 <- k0 + 1
  }
  
  return(k0)  #Returnăm cea mai mică valoare a lui k0 găsită
}

#Exemplu de utilizare pentru lambda = 2
smallest_k0 <- poisson_smallest_k0(lambda = 2)

#Afișăm rezultatul
print(paste("Cea mai mica valoare a lui k0 pentru P(Y >= k0) < 10^(-7) este:", smallest_k0))

#Rezultatele obtinute:

#Cea mai mica valoare a lui k0 pentru P(Y >= k0) < 10^(-7) este: 13




#A2


#Rezolvare si apelare:

#a)

calculeaza_statistici <- function(nume_fisier) {
  #Deschide fisierul
  fisier <- file(nume_fisier, "r")
  
  #Citeste antetul cu numele esantioanelor (P, S)
  antet <- readLines(fisier, n = 1)
  nume_esantioane <- strsplit(antet, ",")[[1]]
  
  #Citeste datele din fisier (2 coloane, separate prin ',')
  date <- read.table(fisier, sep = ",", header = FALSE, skip = 1, col.names = nume_esantioane)
  
  #Inchide fisierul
  close(fisier)
  
  #Calculeaza statistici pentru fiecare esantion
  for (esantion in nume_esantioane) {
    cat("Statistici pentru esantionul:", esantion, "\n")
    cat("Mediana:", median(date[[esantion]]), "\n")
    cat("Media:", mean(date[[esantion]]), "\n")
    cat("Deviatia standard:", sd(date[[esantion]]), "\n")
    cat("Cvartile:", quantile(date[[esantion]], probs = c(0.25, 0.75)), "\n\n")
  }
}

#Exemplu de utilizare
calculeaza_statistici("note.txt")



#Rezultatele obtinute:

#Statistici pentru esantionul: P 
#Mediana: 6.25 
#Media: 6.667847 
#Deviatia standard: 1.608851 
#Cvartile: 5.5 7.75 

#Statistici pentru esantionul: S 
#Mediana: 5.5 
#Media: 5.690793 
#Deviatia standard: 1.812514 
#Cvartile: 4.5 6.75 


#A se mentiona ca fisierul "note.txt" are formatul:
#P,S
#10.00,10.00
#10.00,10.00
#7.75,10.00...etc. 



#b)

elimina_valori_aberante <- function(nume_fisier, nume_esantion) {
  # Citeste fisierul si incarca datele intr-un dataframe
  df <- read.table(nume_fisier, sep = ",", header = TRUE)
  
  # Extrage esantionul specificat din dataframe si il converteste la tipul numeric
  esantion <- as.numeric(df[[nume_esantion]])
  
  # Calculeaza media si deviatia standard a esantionului
  m <- mean(esantion)
  s <- sd(esantion)
  
  # Initializeaza un vector gol pentru a stoca valorile aberante
  outliers <- vector()
  
  # Parcurge fiecare element din esantion
  for (i in 1:length(esantion)) {
    # Verifica daca elementul este o valoare aberanta
    if (esantion[i] < m - 2 * s || esantion[i] > m + 2 * s) {
      # Adauga valoarea aberanta la vectorul outliers
      outliers <- c(outliers, esantion[i])
    }
  }
  
  # Creeaza un nou esantion care exclude valorile aberante
  esantion_fara_aberante <- esantion[!esantion %in% outliers]
  
  #Daca dorim ca valorile aberante sa fie sterse din fisierul "note.txt", vom folosi urmatoarele linii de cod:
  #Actualizeaza dataframe-ul eliminand valorile aberante
  #df[[nume_esantion]] <- esantion_fara_aberante
  #Rescrie fisierul cu dataframe-ul actualizat
  #write.table(df, file = nume_fisier, sep = ",", row.names = FALSE)
  #Aceste linii de cod sunt utile pentru punctul c) (doar daca dorim sa modificam fisierul cu esantioanele)
  
  
  #print("Esantion initial:") #Pentru testare
  #print(esantion)
  #print("Valori aberante:")
  #print(outliers)
  
  # Afiseaza esantionul fara valorile aberante in consola
  print(esantion_fara_aberante)
  
  # Returneaza esantionul fara valorile aberante
  return(esantion_fara_aberante)
}

# Exemplu de utilizare pentru ambele esantioane "P" si "S" din fisierul "note.txt"
print("Esantionul P fara valori aberante:")
esantion_P_fara_aberante <- elimina_valori_aberante("note.txt", "P")
print("Esantionul S fara valori aberante:")
esantion_S_fara_aberante <- elimina_valori_aberante("note.txt", "S")


#Rezultatele obtinute:

# Esantionul P fara valori aberante:
# [1] 7.75 8.75 8.75 8.00 9.00 9.50 9.25 9.00 8.50 5.00 9.75 9.75 9.25 9.25 7.75
# [16] 7.50 7.50 7.25 7.25 8.50 8.50 7.25 7.25 7.25 7.25 6.50 5.00 8.25 8.25 7.50
# [31] 8.75 8.00 7.75 7.50 7.50 7.50 6.50 6.25 5.75 9.75 9.50 8.75 9.75 9.25 8.75
# [46] 7.75 7.25 7.00 6.25 6.25 9.25 7.50 6.75 5.50 9.50 8.50 8.50 8.50 8.25 8.00
# [61] 7.75 7.25 7.25 7.00 6.50 6.00 5.75 5.75 5.00 8.75 8.25 8.25 8.00 7.25 7.25
# [76] 7.25 6.75 6.50 6.00 9.50 9.25 7.50 7.50 6.75 6.75 6.50 6.25 6.00 6.00 5.75
# [91] 5.75 5.75 5.25 5.00 9.50 8.25 7.50 7.25 7.25 7.00 6.50 6.00 5.75 5.50 5.25
# [106] 9.50 9.00 8.50 8.25 8.25 8.00 7.75 7.50 7.50 7.25 7.25 6.75 6.75 6.50 6.50
# [121] 6.50 6.25 6.00 5.75 5.75 5.50 5.50 5.25 5.25 5.00 5.00 5.00 5.00 5.00 7.25
# [136] 8.75 8.75 8.25 7.75 7.75 6.50 6.50 6.50 6.00 6.00 5.75 5.75 5.50 5.50 5.25
# [151] 5.25 5.00 5.00 9.00 8.75 8.25 7.75 7.75 7.25 6.50 5.75 5.00 5.00 4.50 4.50
# [166] 8.50 7.00 6.75 6.25 6.25 6.25 6.25 5.75 5.75 5.50 5.25 5.00 4.75 9.75 8.50
# [181] 8.00 7.75 7.75 6.75 6.75 6.50 6.50 6.25 6.25 6.00 6.00 6.00 6.00 6.00 5.75
# [196] 5.75 5.75 5.75 5.75 5.50 5.25 5.25 5.25 5.25 5.00 5.00 5.00 5.00 5.00 5.00
# [211] 5.00 5.00 4.50 4.50 4.25 4.25 6.75 6.75 6.75 6.75 6.25 6.00 6.00 5.75 5.50
# [226] 5.50 5.50 5.25 5.25 5.25 5.25 5.25 5.25 3.50 5.50 8.25 7.75 7.50 7.25 7.25
# [241] 7.25 7.25 7.00 7.00 7.00 6.75 6.50 6.25 6.25 6.25 6.00 6.00 6.00 6.00 5.75
# [256] 5.50 5.50 5.50 5.50 5.50 4.25 5.75 5.75 5.00 4.50 7.50 6.75 6.50 6.25 6.25
# [271] 6.25 6.25 6.00 6.00 5.50 5.25 7.00 7.00 6.75 6.25 5.75 5.00 5.00 4.25 8.00
# [286] 7.25 6.75 6.50 6.50 5.75 5.50 5.25 5.00 5.00 4.75 4.75 4.50 7.25 6.25 5.00
# [301] 4.75 4.50 4.50 5.75 5.00 5.00 4.75 4.75 4.50 4.25 4.25 3.75 5.75 5.50 5.25
# [316] 5.00 5.00 5.00 5.00 5.00 4.75 5.75 4.75 4.50 4.25 6.00 4.50 4.25 8.75 4.50
# [331] 4.25

# Valorile aberante din P erau:
# 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10  3
  
  
# Esantionul S fara valori aberante:
# [1] 9.25 9.25 9.25 9.25 9.25 9.00 9.00 9.00 9.00 9.00 9.00 9.00 9.00 9.00 9.00
# [16] 9.00 9.00 8.75 8.75 8.75 8.75 8.75 8.50 8.50 8.50 8.50 8.25 8.25 8.25 8.25
# [31] 8.25 8.00 8.00 8.00 8.00 8.00 8.00 8.00 8.00 8.00 8.00 8.00 7.75 7.75 7.75
# [46] 7.50 7.50 7.50 7.50 7.50 7.50 7.50 7.50 7.50 7.25 7.25 7.25 7.25 7.00 7.00
# [61] 7.00 7.00 7.00 7.00 7.00 7.00 7.00 7.00 7.00 7.00 7.00 7.00 7.00 7.00 6.75
# [76] 6.75 6.75 6.75 6.75 6.75 6.75 6.75 6.75 6.75 6.75 6.75 6.50 6.50 6.50 6.50
# [91] 6.50 6.50 6.50 6.50 6.50 6.50 6.50 6.50 6.50 6.50 6.50 6.50 6.25 6.25 6.25
# [106] 6.25 6.25 6.25 6.25 6.25 6.25 6.25 6.25 6.25 6.00 6.00 6.00 6.00 6.00 6.00
# [121] 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00
# [136] 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 6.00 5.80 5.75 5.75 5.75 5.75 5.75
# [151] 5.75 5.75 5.75 5.75 5.75 5.75 5.75 5.75 5.75 5.75 5.75 5.75 5.75 5.50 5.50
# [166] 5.50 5.50 5.50 5.50 5.50 5.50 5.50 5.50 5.50 5.50 5.25 5.25 5.25 5.25 5.25
# [181] 5.25 5.25 5.25 5.25 5.25 5.25 5.25 5.25 5.00 5.00 5.00 5.00 5.00 5.00 5.00
# [196] 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00
# [211] 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00 5.00
# [226] 5.00 4.75 4.75 4.75 4.75 4.75 4.75 4.75 4.75 4.75 4.75 4.75 4.75 4.75 4.75
# [241] 4.75 4.75 4.75 4.75 4.55 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50
# [256] 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50 4.50
# [271] 4.50 4.50 4.25 4.25 4.25 4.25 4.00 4.00 4.00 4.00 4.00 4.00 4.00 4.00 4.00
# [286] 4.00 4.00 3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.50 3.50 3.50 3.50 3.50
# [301] 3.50 3.50 3.50 3.50 3.50 3.50 3.50 3.50 3.25 3.25 3.25 3.25 3.25 3.25 3.00
# [316] 3.00 3.00 3.00 3.00 3.00 3.00 3.00 3.00 2.75 2.75 2.75 2.75 2.75 2.75 2.75
# [331] 2.75 2.75 2.50 2.50 2.50 2.50 2.50 2.25 2.25 2.25

# Valorile aberante din S erau:
# 10.00 10.00 10.00  9.75  9.75  9.75  9.75  9.75  9.75  9.75  9.50  2.00  2.00  2.00


#c)

reprezinta_distributie <- function(nume_fisier, nume_esantion) {
  #Citeste fisierul si incarca datele intr-un dataframe
  df <- read.table(nume_fisier, sep = ",", header = TRUE)
  
  #Extrage esantionul specificat din dataframe si converteste-l la tipul numeric
  esantion <- as.numeric(df[[nume_esantion]])
  
  #Calculeaza numarul de intervale
  numar_intervale <- 9
  
  #Calculeaza limitele intervalelor
  limite_intervale <- seq(1, 10, length.out = numar_intervale + 1)
  
  #Calculeaza frecventele pe intervale
  frecvente <- hist(esantion, breaks = limite_intervale, plot = FALSE)$counts
  
  #Reprezinta grafic distributia frecventelor
  barplot(frecvente, names.arg = paste(limite_intervale[-(numar_intervale + 1)], limite_intervale[-1], sep = ","), 
          xlab = "Intervale", ylab = "Frecventa", main = "Distributia frecventelor")
}

#Exemplu de utilizare pentru esantionul "P" din fisierul "note.txt"
reprezinta_distributie("note.txt", "P")

#Exemplu de utilizare pentru esantionul "S" din fisierul "note.txt"
reprezinta_distributie("note.txt", "S")
