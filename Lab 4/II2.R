# Definim functia
f <- function(u) exp(-2*u^2)

# Definim limitele de integrare
a <- 0
b <- Inf

# Definim distributia
lambda <- 3

# Generam N puncte utilizand distributia exponetiala
N <- 50000

# Generam N puncte utilizand distributia exponetiala
u <- rexp(N, lambda)

# Calculam valorile functiei pentru fiecare punct generat
y <- f(u) / dexp(u, lambda)

# Calculam valoarea estimata a integralei
int_approx <- mean(y)

# Calculam valoarea exacta a integralei
int_exact <- sqrt(pi) / 8

# Calculam eroarea absoluta
eroare_abs <- abs(int_approx - int_exact)

# Calculam eroarea relativa
eroare_rel <- eroare_abs / int_exact

# Afisam rezultatele
cat("Valoarea estimata a integralei:", int_approx, "\n")
cat("Valoarea exacta a integralei:", int_exact, "\n")
cat("Eroarea absoluta:", eroare_abs, "\n")
cat("Eroarea relativa:", eroare_rel, "\n\n")

# Definim un vector in care vom stoca estimarile
estimari <- vector(length = 30)

# Generam si estimam integralele de 30 de ori
for(i in 1:30) {
  
  # Generam N puncte utilizand distributia exponetiala
  u <- rexp(N, lambda)
  
  # Calculam valorile functiei pentru fiecare punct generat
  y <- f(u) / dexp(u, lambda)
  
  # Calculam valoarea estimata a integralei
  int_approx <- mean(y)
  
  # Calculam valoarea exacta a integralei
  int_exact <- sqrt(pi) / 8
  
  # Calculam eroarea absoluta
  eroare_abs <- abs(int_approx - int_exact)
  
  # Calculam eroarea relativa
  eroare_rel <- eroare_abs / int_exact
  
  # Afisam rezultatele pentru fiecare estimare in parte
  cat("Estimarea", i, "\n")
  cat("Valoarea estimata a integralei:", int_approx, "\n")
  cat("Valoarea exacta a integralei:", int_exact, "\n")
  cat("Eroarea absoluta:", eroare_abs, "\n")
  cat("Eroarea relativa:", eroare_rel, "\n\n")
  
  # Adaugam estimarea curenta in vectorul de estimari
  estimari[i] <- int_approx
  
}

# Calculam media si deviatia standard a estimarilor
media_estimari <- mean(estimari)
deviatie_std_estimari <- sd(estimari)

# Afisam media si deviatia standard a estimarilor
cat("Media estimarilor:", media_estimari, "\n")
cat("Deviatia standard a estimarilor:", deviatie_std_estimari, "\n")


#Valoarea estimata a integralei este 0.6278509, iar valoarea exacta a integralei este 0.2215567.
#Astfel, eroarea absoluta este 0.4062942 si eroarea relativa este 1.83381.
#În cazul acestei estimări, eroarea absolută este destul de mare, de aproximativ 0.4, ceea ce înseamnă că valoarea estimată 
#este destul de diferită de valoarea exactă a integralei. De asemenea, 
#eroarea relativă este mare, de aproximativ 1.83, ceea ce sugerează că valoarea estimată este de aproape două ori mai mare decât valoarea exactă a integralei.

#Dupa cele 30 de aproximari calculate, media estimarilor este 0.6265683 si deviatia 
#standard a estimarilor este 0.001085595 
