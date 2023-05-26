                              #Tema E

#E1

#Rezolvare si apelare:


interval_de_incredere = function(alfa, n, sample_mean, sigma){
  critical_z = qnorm(1 - alfa/2)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return(interval)
}

#Specificați valorile cunoscute
sigma <- 11  #Deviația standard
media <- 138  #Media
n <- 10  #Numărul de măsurători

#Calculează intervalele de încredere
interval_90 <- interval_de_incredere(0.1, n, media, sigma)
interval_95 <- interval_de_incredere(0.05, n, media, sigma)
interval_99 <- interval_de_incredere(0.01, n, media, sigma)

# Afișează rezultatele
cat("Intervalul de încredere de 90%:", interval_90, "\n")
cat("Intervalul de încredere de 95%:", interval_95, "\n")
cat("Intervalul de încredere de 99%:", interval_99, "\n")


#Rezultatele obtinute:

#Intervalul de încredere de 90%: 132.2784 143.7216 
#Intervalul de încredere de 95%: 131.1823 144.8177 
#Intervalul de încredere de 99%: 129.04 146.96 


#Varianta a doua pentru E1, fara a utiliza functia interval_de_increde


#Specificați valorile cunoscute
sigma <- 11  #Deviația standard
media <- 138  #Media
n <- 10  #Numărul de măsurători

#Găsiți valorile critice pentru diferitele niveluri de încredere
conf_level_90 <- 0.9
z_critical_90 <- qnorm(1 - (1 - conf_level_90) / 2)

conf_level_95 <- 0.95
z_critical_95 <- qnorm(1 - (1 - conf_level_95) / 2)

conf_level_99 <- 0.99
z_critical_99 <- qnorm(1 - (1 - conf_level_99) / 2)

#Calculează intervalele de încredere
margin_error_90 <- z_critical_90 * sigma / sqrt(n)
interval_90 <- c(media - margin_error_90, media + margin_error_90)

margin_error_95 <- z_critical_95 * sigma / sqrt(n)
interval_95 <- c(media - margin_error_95, media + margin_error_95)

margin_error_99 <- z_critical_99 * sigma / sqrt(n)
interval_99 <- c(media - margin_error_99, media + margin_error_99)

#Afișează rezultatele
cat("Intervalul de încredere de 90%:", interval_90, "\n")
cat("Intervalul de încredere de 95%:", interval_95, "\n")
cat("Intervalul de încredere de 99%:", interval_99, "\n")


#Rezultate obtinute:

#Intervalul de încredere de 90%: 132.2784 143.7216 
#Intervalul de încredere de 95%: 131.1823 144.8177 
#Intervalul de încredere de 99%: 129.04 146.96 

#Observam ca rezultatele sunt aceleasi oricare dintre metode ar fi utilizata, rezultatele fiind cele asteptate. 



#E2


#Rezolvare si apelare:


interval_de_incredere = function(alfa, n, sample_mean, sigma){
  critical_z = qnorm(1 - alfa/2)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  return(interval)
}

#Specificați valorile cunoscute
n <- 256  #Numărul de indivizi din eșantion
sample_mean <- 18  #Media de selecție
sample_variance <- 1.44  #Dispersia eșantionului

#Calculează deviația standard a eșantionului
sample_standard_deviation <- sqrt(sample_variance)

#Calculează intervalul de încredere de 95%
interval <- interval_de_incredere(0.05, n, sample_mean, sample_standard_deviation)

#Afișează rezultatul
cat("Intervalul de încredere de 95%:", interval, "\n")


#Rezultatul obtinut este:

#Intervalul de încredere de 95%: 17.853 18.147 


#Varianta a doua de rezolvare, fara utilizarea functiei pentru calcularea intervalului de incredere

# Specificați valorile cunoscute
n <- 256  # Numărul de indivizi din eșantion
sample_mean <- 18  # Media de selecție
sample_variance <- 1.44  # Dispersia eșantionului

# Calculați deviația standard a eșantionului
sample_standard_deviation <- sqrt(sample_variance)

# Găsiți valorile critice pentru nivelul de încredere de 95%
conf_level <- 0.95
z_critical <- qnorm(1 - (1 - conf_level) / 2)

# Calculează marginile intervalului de încredere
margin_error <- z_critical * sample_standard_deviation / sqrt(n)
interval <- c(sample_mean - margin_error, sample_mean + margin_error)

# Afișează rezultatul
cat("Intervalul de încredere de 95%:", interval, "\n")


#Rezultatul obtinut este:
#Intervalul de încredere de 95%: 17.853 18.147 



#E3


#Rezolvare si apelare:


# Procentul nemulțumiților înainte de schimbare
p_before <- 0.12

# Mărimea eșantionului după schimbare
n <- 153

# Numărul de clienți nemulțumiți după schimbare
x <- 17

# Calculăm statisticile de test
p_hat <- x / n  # Procentul estimat de clienți nemulțumiți după schimbare
z <- (p_hat - p_before) / sqrt(p_before * (1 - p_before) / n)  # Statistica de test Z

# Calculăm valorile critice pentru nivelurile de semnificație de 1% și 5%
z_critical_1 <- qnorm(0.01 / 2)
z_critical_5 <- qnorm(0.05 / 2)

# Comparam statisticile de test cu valorile critice
if (abs(z) > z_critical_1) {
  conclusion_1 <- "Putem respinge ipoteza nulă la nivelul de semnificație de 1%."
} else {
  conclusion_1 <- "Nu putem respinge ipoteza nulă la nivelul de semnificație de 1%."
}

if (abs(z) > z_critical_5) {
  conclusion_5 <- "Putem respinge ipoteza nulă la nivelul de semnificație de 5%."
} else {
  conclusion_5 <- "Nu putem respinge ipoteza nulă la nivelul de semnificație de 5%."
}

# Afișăm rezultatele corectate
cat("Concluzie pentru nivelul de semnificație de 1%:", conclusion_1, "\n")
cat("Concluzie pentru nivelul de semnificație de 5%:", conclusion_5, "\n")



#O alta varianta posibila, asemanatoare, dar utilizand functia de la seminar, dar modificata

test_binomial <- function(alpha, n, successes, p0, hyp) {
  p_hat <- successes / n
  q_hat <- 1 - p_hat
  z_score <- (p_hat - p0) / sqrt(p0 * q_hat / n)
  
  if(hyp == "r") {
    critical_z_1 <- qnorm(1 - alpha, 0, 1)
    critical_z_5 <- qnorm(1 - 0.05, 0, 1)
    
    if(z_score > critical_z_1)
      conclusion <- "Putem respinge ipoteza nulă (H0) la nivelul de semnificație de 1%"
    else if(z_score > critical_z_5)
      conclusion <- "Putem respinge ipoteza nulă (H0) la nivelul de semnificație de 5%"
    else
      conclusion <- "Nu avem suficiente dovezi pentru a respinge ipoteza nulă (H0)"
  }
  
  if(hyp == "l") {
    critical_z_1 <- qnorm(alpha, 0, 1)
    critical_z_5 <- qnorm(0.05, 0, 1)
    
    if(z_score < critical_z_1)
      conclusion <- "Putem respinge ipoteza nulă (H0) la nivelul de semnificație de 1%"
    else if(z_score < critical_z_5)
      conclusion <- "Putem respinge ipoteza nulă (H0) la nivelul de semnificație de 5%"
    else
      conclusion <- "Nu avem suficiente dovezi pentru a respinge ipoteza nulă (H0)"
  }
  
  if(hyp == "s") {
    critical_z_1 <- qnorm(1 - alpha / 2, 0, 1)
    critical_z_5 <- qnorm(0.025, 0, 1)
    
    if(abs(z_score) > critical_z_1)
      conclusion <- "Putem respinge ipoteza nulă (H0) la nivelul de semnificație de 1%"
    else if(abs(z_score) > critical_z_5)
      conclusion <- "Putem respinge ipoteza nulă (H0) la nivelul de semnificație de 5%"
    else
      conclusion <- "Nu avem suficiente dovezi pentru a respinge ipoteza nulă (H0)"
  }
  
  return(conclusion)
}

# Apelarea funcției pentru problema ta
n <- 153
successes <- 17
p0 <- 0.12

conclusion_1 <- test_binomial(0.01, n, successes, p0, "s")
conclusion_5 <- test_binomial(0.05, n, successes, p0, "s")

cat("Concluzie pentru nivelul de semnificație de 1%:", conclusion_1, "\n")
cat("Concluzie pentru nivelul de semnificație de 5%:", conclusion_5, "\n")

