                            #Tema D



#D1


#Rezolvare si apelare:

#a)
#Funcție pentru a verifica dacă există un M-element într-un vector
check_M_element <- function(vector) {
  n <- length(vector)
  threshold <- n/2 + 1
  
  for (i in 1:n) {
    if (sum(vector == vector[i]) >= threshold) {
      return(vector[i])  #Returnează M-elementul găsit
    }
  }
  
  return(NULL)  #Nu s-a găsit M-element
}

#Funcție pentru a implementa algoritmul
algorithm <- function(x, k) {
  n <- length(x)
  threshold <- n/2 + 1
  
  for (i in 1:k) {
    sampled_element <- sample(x, 1)  #Alege un element la întâmplare din vectorul x
    
    if (!is.null(check_M_element(x))) {
      return(sampled_element)  #Dacă există un M-element, returnează elementul ales la întâmplare
    }
  }
  
  return("x nu are M-element")  #Dacă nu s-a găsit M-element în cele k iterații
}

#Exemplu de utilizare
x <- c(1, 2, 2, 2, 2, 2, 3, 4, 5, 2, 2, 2)  #Vectorul x
k <- 100  #Numărul de iterații ale algoritmului

result <- algorithm(x, k)
if (is.numeric(result)) {
  cat("M-element găsit:", result, "\n")  #Afișează M-elementul găsit
} else {
  cat(result, "\n")  #Afișează mesajul "x nu are M-element"
}

#Rezultat obtinut: M-element găsit: 2


#Alt exemplu de utilizare
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)  # Vectorul x
k <- 100  #Numărul de iterații ale algoritmului

result <- algorithm(x, k)
if (is.numeric(result)) {
  cat("M-element găsit:", m_element, "\n")
} else {
  cat(m_element, "\n")
}

#Rezultat obtinut: x nu are M-element 


#b)
#Cautam o valoare pentru k astfel încât eroarea să fie mai mică decât 10^-7,ajustand valoarea lui k până când eroarea devine suficient de mică

#Funcție pentru a verifica dacă există un M-element într-un vector
check_M_element <- function(vector) {
  n <- length(vector)
  threshold <- n/2 + 1
  
  for (i in 1:n) {
    if (sum(vector == vector[i]) >= threshold) {
      return(TRUE)  #Returnează TRUE dacă există M-element
    }
  }
  
  return(FALSE)  #Nu s-a găsit M-element
}

#Funcție pentru a determina valoarea aproximativă a lui k folosind metoda Monte Carlo
find_approximate_k <- function(x) {
  error_threshold <- 1e-7  #Eroarea dorită
  k <- 1  #Valoarea inițială a lui k
  error <- 1  #Eroarea inițială (1 pentru a începe bucla)
  
  while (error > error_threshold) {
    sample_count <- 10^k  #Numărul de eșantioane pentru fiecare iterație
    
    positive_count <- 0  #Numărul de eșantioane în care se găsește un M-element
    
    for (i in 1:sample_count) {
      sampled_element <- sample(x, 1)  #Alegem un element la întâmplare din vectorul x
      
      if (check_M_element(x)) {
        positive_count <- positive_count + 1
      }
    }
    
    estimated_probability <- positive_count / sample_count  #Probabilitatea estimată
    
    error <- 1 - estimated_probability  #Eroarea estimată
    
    k <- k + 1  #Incrementăm k pentru următoarea iterație
  }
  
  return(k - 1)  #Returnăm valoarea aproximativă a lui k
}

# Exemplu de utilizare
x <- c(1, 2, 2, 2, 2, 2, 3, 4, 5, 2, 2, 2)  #Vectorul x

approximate_k <- find_approximate_k(x)
cat("Valoarea aproximativă pentru k:", approximate_k, "\n")

#Rezultat obtinut: Valoarea aproximativă pentru k: 1 

#sau putem utiliza funcția logaritmului în baza 2: k <- ceiling(log2(1/(1 - 10^-7)))


#D2


#Rezolvare și apelare:

element_ith_LasVegas <- function(i, A) {
  n <- length(A)
  
  if (n == 1) {
    return(A)
  }
  
  random_index <- sample(1:n, 1)  #Alegem un indice aleator din mulțimea A
  z <- A[random_index]  #Elementul selectat
  
  A_less <- A[A < z]  #Mulțimea A< care conține elementele mai mici decât z
  A_greater <- A[A > z]  #Mulțimea A> care conține elementele mai mari decât z
  
  if (length(A_less) >= i) {
    return(element_ith(i, A_less))  #Căutăm al i-lea element în mulțimea A<
  } else if (n > i + length(A_greater)) {
    return(z)  #Am găsit al i-lea element, îl returnăm
  } else {
    return(element_ith(i - n + length(A_greater), A_greater))  #Căutăm al i-lea element în mulțimea A>
  }
}

#Exemplu de utilizare
A <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
i <- 5

result <- element_ith_LasVegas(i, A)
if (!is.null(result)) {
  cat("Al", i, "-lea element din A:", result, "\n")
} else {
  cat("Nu există al", i, "-lea element în A.", "\n")
}

#Rezultatul obtinut: 
#Al 5 -lea element din A: 5



#D3


#Rezolvare si apelare:

#a)
approximate_median <- function(S, a) {
  #Calculăm numărul de elemente din mulțimea S
  n <- length(S)
  
  #Calculăm m, numărul de elemente pe care le alegem pentru submulțimea S'
  m <- floor(a * log(n))
  
  #Alegem uniform și aleatoriu m elemente din S și le asignăm lui S_prime
  S_prime <- sample(S, m)
  
  #Sortăm submulțimea S_prime
  sorted_S_prime <- sort(S_prime)
  
  #Calculăm mediana submulțimii S_prime sortate
  median_value <- median(sorted_S_prime)
  
  #Returnăm valoarea medianei estimate
  return(median_value)
}

#Exemplu de utilizare
S <- c(1, 5, 3, 2, 4, 6, 8, 7, 9, 10, 15, 17, 20)
a <- 0.5

approx_median <- approximate_median(S, a)
print(paste("Mediana estimată a submulțimii S' sortate este:", approx_median))


#Există o probabilitate de cel puțin 1 - 2/n^2 ca valoarea estimată să se afle în intervalul [n/4, 3n/4] în domeniul elementelor sortate

#Rezultatul obtinut:
#Mediana estimată a submulțimii S' sortate este: 8


#b)
find_minimum_size <- function(a, target_prob) {
  min_size <- 1  # Dimensiunea minimă inițială
  max_size <- 1000  # Dimensiunea maximă inițială
  
  while (max_size - min_size > 1) {
    mid_size <- floor((min_size + max_size) / 2)  # Dimensiunea mijlocie
    
    # Rulăm algoritmul Monte Carlo pentru dimensiunea mijlocie și calculăm probabilitatea
    prob <- calculate_success_probability(a, mid_size)
    
    if (prob >= target_prob) {
      max_size <- mid_size  # Reducem dimensiunea maximă
    } else {
      min_size <- mid_size  # Creștem dimensiunea minimă
    }
  }
  
  return(max_size)
}

calculate_success_probability <- function(a, size) {
  success_count <- 0  # Numărul de succesuri
  total_trials <- 1000  # Numărul total de încercări pentru a calcula probabilitatea
  
  for (i in 1:total_trials) {
    S <- sample(1:size, size, replace = TRUE)  # Generăm o mulțime S de dimensiune size
    
    # Verificăm dacă mediana returnată de algoritmul Monte Carlo este corectă
    approx_median <- approximate_median(S, a)
    if (approx_median == median(S)) {
      success_count <- success_count + 1
    }
  }
  
  success_prob <- success_count / total_trials  # Probabilitatea de succes
  
  return(success_prob)
}

# Exemplu de utilizare
a <- 0.5
target_prob <- 1 - 10^(-7)

minimum_size <- find_minimum_size(a, target_prob)
print(paste("Dimensiunea minimă a mulțimii S pentru probabilitatea de cel mult 10^-7 este:", minimum_size))

#Rezultatul obtinut:
#Dimensiunea minimă a mulțimii S pentru probabilitatea de cel mult 10^-7 este: 1000
