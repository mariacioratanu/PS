#III.1
outliers_mean <- function(sample) {
  m <- mean(sample)
  s <- sd(sample)
  outliers <- vector()
  j <- 0
  
  for(i in 1:length(sample)) {
    if(sample[i] < m - 2*s || sample[i] > m + 2*s) {
      j <- j + 1
      outliers[j] <- sample[i]
    }
  }
  
  return(outliers)
}

#III.2

#outliers_iqr <- function(sample) {
#  q1 <- quantile(sample, 0.25)
#  q3 <- quantile(sample, 0.75)
#  iqr <- q3 - q1
#  outliers <- sample[sample < q1 - 1.5 * iqr | sample > q3 + 1.5 * iqr]
#  return(outliers)
#}

outliers_iqr=function(sample) {
  q1=as.vector(quantile(sample))[2]
  q3=as.vector(quantile(sample))[4]
  iqr=q3-q1
  outliers=vector()
  j=0
  for(i in 1: lenght(sample))
    if(sample[i] < q1 - 1.5 * iqr | sample > q3 + 1.5 * iqr){
      j=j+1
      outliers[j]=sample[i]
    }
  return (outliers)
}  

#III.3
sample = scan("sample2.txt")
summary(sample)
outliers_mean(sample)
outliers_iqr(sample)

# Răspuns:
#Rezultatele obținute sunt identice (numai că la summary avem rezultat real), 
#deoarece folosind același eșantion. 
#Cu toate acestea, funcția summary() afișeaza valorile minime și maxime, 
#valorile quartilelor și media, în timp ce funcțiile outliers_mean() și outliers_iqr() determină valorile aberante folosind criterii specifice.

#Rezultatele obținute sunt următoarele:
#summary(sample)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-487.00   58.00   75.00   72.57   84.00  567.00 
#outliers_iqr(sample)
#[1] -487  567
# outliers_meam(sample)
#Error in outliers_meam(sample) : could not find function "outliers_meam"
# outliers_mean(sample)
#[1] -487  567