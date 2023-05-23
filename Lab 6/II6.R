#ne vor ajuta in rezolvare urmatoarele:
#II.1
zconfidence_interval = function(alfa, n, samplemean,  sigma){
  criticalz = qnorm(1 - alfa/2, 0, 1) 
  a = samplemean - criticalz*sigma/sqrt(n)
  b = samplemean + criticalz*sigma/sqrt(n)
  interval = c(a, b)
  return(interval)
}
zconfidence_interval(0.1,100,20,3)

#II.2
zconfidence_interval(0.1,25,67.53,10)


#tema:
#II.6 
zconfidence_intervalfis = function(filename, alfa, sigma )
{
  x = scan(filename)
  n = length(x) #Dimensiunea esantionului
  mediaselectie = mean(x)  #Media de selectie
  
  z <- qnorm((1 - alfa) / 2, 0, 1)# Calculul valorii critice Z pentru un nivel de incredere de 95%
  a <- mediaselectie - z * (sigma / sqrt(n))
  b <- mediaselectie + z * (sigma / sqrt(n))
  
  interval <- c(a, b)
  print(interval)
}
zconfidence_intervalfis("history.txt",0.05, 10)
