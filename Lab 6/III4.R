#exercitiu pe care l-am folosit in rezolvare:

#III.1
t_conf_interval = function(n, samplemean, s, alfa){
  se = s/sqrt(n)
  criticalt = qt(1 - alfa/2, n - 1)
  a = samplemean - criticalt*se  
  b = samplemean + criticalt*se
  interval = c(a, b)
  return(interval)
}

#tema:

#III.4:

t_conf_intervalfis <- function(filename, alfa) 
{
  x <- scan(filename)
  n <- length(x)  #Dimensiunea esantionului
  mediasel <- mean(x)  #Media de selectie
  s <- sd(x)  #Deviatia standard
  se <- s / sqrt(n) 
  critical_t <- qt(1 - alfa/2, n - 1)
  
  a <- mediasel - critical_t * se
  b <- mediasel + critical_t * se
  
  interval <- c(a, b)
  return(interval)
}

t_conf_intervalfis("history.txt",0.05)

#III.5
#am creat fisierul data.txt care contine valorile: 12 11 12 10 11 12 13 12 11 11 13 14 10

#Pentru nivelul de încredere de 90%:
interval90 <- t_conf_intervalfis("data.txt", alpha = 0.1)
#Pentru nivelul de încredere de 95%:
interval95 <- t_conf_intervalfis("data.txt", alpha = 0.05)
#Pentru nivelul de încredere de 99%:
interval99 <- t_conf_intervalfis("data.txt", alpha = 0.01)

#Rezultatele:
interval90
interval95
interval99
