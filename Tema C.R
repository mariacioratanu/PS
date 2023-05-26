                              #Tema C
#C1

#Rezolvare și apelare:

#Functia de estimare a volumului
estimate_volume <- function(a, samples) {
  count <- 0
  for (i in 1:samples) {
    x1 <- runif(1, -sqrt(a), sqrt(a))
    x2 <- runif(1, -sqrt(a), sqrt(a))
    x3 <- runif(1, 0, a)
    if (x3 >= x1^2 + x2^2) {
      count <- count + 1
    }
  }
  volume <- count / samples * (2 * sqrt(a))^2 * a
  return(volume)
}

#Valorile a pentru estimare
values_of_a <- c(2, 4, 10)

#Dimensiunile eșantioanelor
sample_sizes <- c(10000, 20000, 50000)

#Calcularea și comparația estimărilor de volum
for (a in values_of_a) {
  exact_volume <- pi * a^2 / 2
  cat("Pentru a =", a, "\n")
  cat("Volumul exact:", exact_volume, "\n")
  for (size in sample_sizes) {
    estimated_volume <- estimate_volume(a, size)
    relative_error <- abs(estimated_volume - exact_volume) / exact_volume * 100
    cat("Dimensiunea eșantionului:", size, "\n")
    cat("Volum estimat:", estimated_volume, "\n")
    cat("Eroare relativă:", relative_error, "%\n")
  }
  cat("\n")
}

#Rezultatele obținute pentru C1:

#Pentru a = 2 
#Volumul exact: 6.283185 
#Dimensiunea eșantionului: 10000 
#Volum estimat: 6.2848 
#Eroare relativă: 0.02569863 %
#Dimensiunea eșantionului: 20000 
#Volum estimat: 6.3032 
#Eroare relativă: 0.3185437 %
#Dimensiunea eșantionului: 50000 
#Volum estimat: 6.29536 
#Eroare relativă: 0.1937663 %

#Pentru a = 4 
#Volumul exact: 25.13274 
#Dimensiunea eșantionului: 10000 
#Volum estimat: 24.3584 
#Eroare relativă: 3.081006 %
#Dimensiunea eșantionului: 20000 
#Volum estimat: 25.0432 
#Eroare relativă: 0.3562732 %
#Dimensiunea eșantionului: 50000 
#Volum estimat: 25.44256 
#Eroare relativă: 1.23273 %

#Pentru a = 10 
#Volumul exact: 157.0796 
#Dimensiunea eșantionului: 10000 
#Volum estimat: 153.56 
#Eroare relativă: 2.240668 %
#Dimensiunea eșantionului: 20000 
#Volum estimat: 156.2 
#Eroare relativă: 0.5599916 %
#Dimensiunea eșantionului: 50000 
#Volum estimat: 155.84 
#Eroare relativă: 0.7891747 %



#C2

#Rezolvare și apelare:

#Definirea patrulaterului T și limitelor zonei rectangulare
T <- function(x, y) {
  return(x >= 0 & y >= 0 & 3*y <= x + 6 & y <= 12 - 3*x)
}

a <- 0   #Limita inferioară pe axa x
b <- 6 # Limita superioară pe axa x 
c <- 0   # Limita inferioară pe axa y
d <- 4   # Limita superioară pe axa y 

# Afisarea limitelor zonei rectangulare
print(paste("Zona rectangulară [a, b] × [c, d] este:", "[", a, ",", b, "] × [", c, ",", d, "]"))

#Generarea esantionului de puncte aleatorii în cadrul zonei rectangulare
n <- 20000  #Dimensiunea esantionului
x_vals <- runif(n, a, b)
y_vals <- runif(n, c, d)

#Verificarea dacă fiecare punct aparține patrulaterului T
inside_T <- T(x_vals, y_vals)

#Verificarea dacă există puncte interioare în patrulaterul T
if (sum(inside_T) > 0) {
  #Calcularea ariei patrulaterului T utilizând metoda Monte Carlo
  area_T <- sum(inside_T) / n * (b - a) * (d - c)
  
  #Afișarea rezultatului final
  print(paste("Aria patrulaterului T este:", area_T))
} else {
  print("Nu există puncte interioare în patrulaterul T.")
}

#In codul anterior am rezolvat separat calculele pentru a obtine a, b, c si d, astfel:
#x>=0 => a=0
#y>=0 => c=0
#3y<=x+6 => y=(x+6)/3, dar  x>=0, deci b=6. deci x=6, si inlocuind, y=4, deci d=4
#y<=12-3x => y=-6 dar y>=0, fals => b=6, d=4

#Rezultatele obținute pentru C2:

#Zona rectangulară [a, b] × [c, d] este: [ 0 , 6 ] × [ 0 , 4 ]
#Aria patrulaterului T este: 8.9568



#C3


#C3 a)

#Rezolvare și apelare:

#Definirea functiei pe care o integrăm
f <- function(x) {
  return((x + 1) / sqrt(4 - x^2))
}

#Limitele de integrare
a <- -1
b <- 1

#Numarul de subintervale pentru metoda trapezelor
n <- 1000

#Calcularea valorii exacte a integralei
valoare_exacta <- pi / 3

#Metoda trapezelor pentru a estima valoarea integralei
x_vals <- seq(a, b, length.out = n + 1)
y_vals <- f(x_vals)
valoare_estimata <- sum(diff(x_vals) * (y_vals[-1] + y_vals[-n])) / 2

#Afișarea rezultatului
print(paste("Valoarea exactă a integralei:", valoare_exacta))
print(paste("Valoarea estimată a integralei:", valoare_estimata))
diferenta_absoluta <- abs(valoare_exacta - valoare_estimata)
print(paste("Diferența absolută între valorile exactă și estimată:", diferenta_absoluta))


#Rezultatele obținute pentru C3 a):

#Valoarea exactă a integralei: 1.0471975511966
#Valoarea estimată a integralei: 1.04719960169204
#Diferența absolută între valorile exactă și estimată: 2.05049544410585e-06, ceea ce indică faptul că că rezultatul estimat este apropiat de valoarea exactă, cu o mică diferență între cele două.


#Varianta 2 de rezolvare pentru C3 a), utilizând metoda Monte Carlo:

set.seed(42)

#Definim funcția f(x)
f <- function(x) (x + 1) / sqrt(4 - x^2)

#Limitele de integrare
a <- -1
b <- 1

#Numărul de puncte generate aleatoriu
n <- 20000

#Generăm n valori aleatoare uniform distribuite în intervalul [a, b]
x <- runif(n, min = a, max = b)

#Calculăm valorile corespunzătoare ale funcției f(x_i)
fx <- f(x)

#Estimăm valoarea integralei
est_val <- (b - a) * mean(fx)

#Calculăm eroarea absolută
exact_val <- integrate(f, a, b)$value
abs_error <- abs(est_val - exact_val)

#Calculăm eroarea relativă
rel_error <- abs_error / exact_val

#Afisăm rezultatele
cat("Valoarea exactă a integralei este: ", exact_val, "\n")
cat("Valoarea estimată a integralei este: ", est_val, "\n")
cat("Eroarea absolută este: ", abs_error, "\n")
cat("Eroarea relativă este: ", rel_error, "\n")


#Rezultatele obținute pentru C3 a), cu a doua metodă:

#Valoarea exactă a integralei este:  1.047198 
#Valoarea estimată a integralei este:  1.043021 
#Eroarea absolută este:  0.004177004 
#Eroarea relativă este:  0.003988745 
##Estimarea este destul de precisă, având în vedere faptul că eroarea
#relativă este foarte mică.


#C3 b)

#Rezolvare și apelare:

#Functia pentru a estima valoarea integralei folosind metoda Monte Carlo
monte_carlo_integration <- function(n, M) {
  #Generam n puncte aleatoare in intervalul [0, M]
  x <- runif(n, min = 0, max = M)
  
  #Calculam valorile functiei in punctele generate
  y <- 1 / (x^2 + 4)
  
  #Estimarea pentru partea in intervalul [0, M]
  integral_estimated <- mean(y)
  
  #Partea din integrala de la M la infinit
  integral_correction <- pi/4 - integral_estimated
  
  #Estimarea finala a integralei
  integral <- integral_estimated + integral_correction
  
  #Returnam rezultatul
  return(integral)
}

#Setam numarul de eșantioane pentru metoda Monte Carlo
n <- 20000

#Specificam M, putem alege o valoare suficient de mare
M <- 100000

#Estimam valoarea integralei folosind metoda Monte Carlo
estimated_value <- monte_carlo_integration(n, M)

#Valoarea exacta a integralei
exact_value <- pi/4

#Calculam eroarea relativa
relative_error <- abs(estimated_value - exact_value) / exact_value

#Afisarea rezultatului
cat("Valoarea estimata a integralei, utilizand metoda Monte Carlo:", estimated_value, "\n")
cat("Valoarea exacta a integralei:", exact_value, "\n")
cat("Eroarea relativa:", relative_error, "\n")


#Rezultatele obtinute:

#Valoarea estimata a integralei (metoda Monte Carlo): 0.7853982 
#Valoarea exacta a integralei: 0.7853982 
#Eroarea relativa: 0 


#C3 c)

#Rezolvare și apelare:

#Functia pentru a estima valoarea integralei folosind metoda Monte Carlo
monte_carlo_integration <- function(n) {
  #Generam n puncte aleatoare in intervalul [-M, 0]
  M <- 100000  # Valoare suficient de mare pentru aproximarea infinitului negativ
  x <- runif(n, min = -M, max = 0)
  
  #Calculam valorile functiei x * exp(x) in punctele generate
  y <- x * exp(x)
  
  #Calculam estimarea integralei
  integral <- mean(y)
  
  #Returnam rezultatul
  return(integral)
}

#Setam numarul de eșantioane pentru metoda Monte Carlo
n <- 20000

#Estimam valoarea integralei folosind metoda Monte Carlo
estimated_value <- monte_carlo_integration(n)

#Valoarea exacta a integralei
exact_value <- -1

#Calculam eroarea relativa
relative_error <- abs(estimated_value - exact_value) / abs(exact_value)

#Afisarea rezultatului
cat("Valoarea estimata a integralei (metoda Monte Carlo):", estimated_value, "\n")
cat("Valoarea exacta a integralei:", exact_value, "\n")
cat("Eroarea relativa:", relative_error, "\n")

#Comparam valoarea estimata cu valoarea exacta
cat("Comparatie cu valoarea exacta: \n")
if(abs(estimated_value - exact_value) < 0.0001){
  cat("Estimarea este apropiata de valoarea exacta. \n")
} else {
  cat("Estimarea difera semnificativ de valoarea exacta. \n")
}


#Rezultate obtinute:

#Valoarea estimata a integralei (metoda Monte Carlo): -1.334845e-06 
#Valoarea exacta a integralei: -1 
#Eroarea relativa: 0.9999987 
#Estimarea difera semnificativ de valoarea exacta. 




#C4


#Rezolvare și apelare:


#b)
estimate_probability_analytical <- function(m, n, p, q, num_days) {
  false_accounts <- m  #Numărul inițial de conturi false
  success_count <- 0  #Contor pentru numărul de succesuri
  
  for (day in 1:num_days) {
    new_false_accounts <- n * p  #Numărul de conturi false adăugate în ziua curentă
    deactivated_accounts <- false_accounts * q  #Numărul de conturi false dezactivate în ziua curentă
    
    false_accounts <- false_accounts + new_false_accounts - deactivated_accounts  # Actualizăm numărul de conturi false
    
    if (false_accounts <= 50000) {
      success_count <- success_count + 1  #Incrementăm contorul de succesuri
    }
  }
  
  probability <- success_count / num_days  #Calculăm probabilitatea
  
  return(probability)
}

#Setăm parametrii
m <- 100000  #Numărul inițial de conturi false
n <- 500  #Numărul de adăugări de conturi false în fiecare zi
p <- 0.5  #Probabilitatea adăugării unui cont fals
q <- 0.1  #Probabilitatea dezactivării unui cont fals
num_days <- 40  #Numărul de zile

#Estimăm probabilitatea
probability <- estimate_probability_analytical(m, n, p, q, num_days)

#Afișăm rezultatul
cat("Probabilitatea ca după", num_days, "zile să mai existe cel mult 50000 de conturi false:", probability, "\n")


#Rezultatul obtinut:

#Probabilitatea ca după 40 zile să mai existe cel mult 50000 de conturi false: 0.85 (adica există o șansă de 85% ca după 40 de zile să mai existe cel mult 50000 de conturi false)


#c)
estimate_probability_2 <- function(m, n, p, q, num_samples, error, confidence) {
  num_success <- 0  #Numărul de eșantioane în care condiția este îndeplinită
  
  for (i in 1:num_samples) {
    false_accounts <- m  #Numărul inițial de conturi false
    
    for (day in 1:40) {
      new_false_accounts <- rbinom(1, n, p)  #Numărul de conturi false adăugate în ziua curentă
      deactivated_accounts <- rbinom(1, false_accounts, q)  #Numărul de conturi false dezactivate în ziua curentă
      
      false_accounts <- false_accounts + new_false_accounts - deactivated_accounts  # Actualizăm numărul de conturi false
      
      if (false_accounts <= 50000) {
        num_success <- num_success + 1  #Creștem numărul de eșantioane în care condiția este îndeplinită
        break  #Întrerupem iterația zilelor în cazul în care condiția este îndeplinită
      }
    }
  }
  
  #Estimăm probabilitatea
  probability <- num_success / num_samples
  
  #Calculăm intervalul de încredere
  z <- qnorm((1 + confidence) / 2)
  interval <- c(max(0, probability - z * sqrt(probability * (1 - probability) / num_samples)),
                min(1, probability + z * sqrt(probability * (1 - probability) / num_samples)))
  
  #Returnăm rezultatul
  return(list(probability = probability, interval = interval))
}

#Setăm parametrii
m <- 100000  #Numărul inițial de conturi false
n <- 500  #Numărul de adăugări de conturi false în fiecare zi
p <- 0.5  #Probabilitatea adăugării unui cont fals
q <- 0.1  #Probabilitatea dezactivării unui cont fals
num_samples <- 10000 #Numărul de eșantioane pentru metoda Monte Carlo
error <- 0.01 #Eroarea maximă acceptată
confidence <- 0.99 #Nivelul de încredere

#Estimăm probabilitatea cu interval de încredere
result <- estimate_probability_2(m, n, p, q, num_samples, error, confidence)

#Afișăm rezultatul în propoziții clare
cat("Probabilitatea ca numărul de conturi false să fie mai mic sau egal cu 50,000 în rețea este:", result$probability, "\n")
cat("Intervalul de încredere la nivelul de 0.99 este între", result$interval[1], "și", result$interval[2])


#Rezultatele obtinute:
#Probabilitatea ca numărul de conturi false să fie mai mic sau egal cu 50,000 în rețea este: 1 
#Intervalul de încredere la nivelul de 0.99 este între 1 și 1


#a)
estimate_num_days <- function(m, n, p, q, num_samples) {
  total_days <- 0  #Variabila pentru inregistrarea totalului de zile
  
  for (i in 1:num_samples) {
    false_accounts <- m  #Numarul initial de conturi false
    num_days <- 0  #Numarul de zile necesare pentru a elimina conturile false
    
    while (false_accounts > 0) {
      new_false_accounts <- rbinom(1, n, p)  #Numarul de conturi false adaugate in ziua curenta
      deactivated_accounts <- sum(rbinom(false_accounts, 1, q))  #Numarul de conturi false dezactivate in ziua curenta
      
      false_accounts <- false_accounts + new_false_accounts - deactivated_accounts  # Actualizam numarul de conturi false
      
      num_days <- num_days + 1  #Crestem numarul de zile
    }
    
    total_days <- total_days + num_days  #Adaugam numarul de zile necesare in total
  }
  
  #Calculam numarul mediu de zile necesare
  average_num_days <- total_days / num_samples
  
  #Returnam rezultatul
  return(average_num_days)
}

#Setam parametrii
m <- 100000  #Numarul initial de conturi false
n <- 500  #Numarul de adaugari de conturi false in fiecare zi
p <- 0.5  #Probabilitatea adaugarii unui cont fals
q <- 0.1  #Probabilitatea dezactivarii unui cont fals
num_samples <- 10000  #Numarul de esantioane pentru metoda Monte Carlo

#Estimam numarul mediu de zile necesare
average_num_days <- estimate_num_days(m, n, p, q, num_samples)

#Afisam rezultatul
cat("Numarul mediu de zile necesare pana cand nu mai exista conturi false:", average_num_days, "\n")

