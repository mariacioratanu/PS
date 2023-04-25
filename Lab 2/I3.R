tablou = read.csv("life_expect.csv", header = T)
country=tablou[['country']]
male=tablou[['male']]
female = tablou[['female']]
interval_female=seq(min(female), max(female), length.out=7+1)
interval_male=seq(min(male), max(male), length.out=7+1)
#hist(female, breaks = 7, freq=T, col='pink')
#hist(male, breaks = 7 ,freq=T,col='blue')
hist(female, breaks = interval_female, freq=T, col='pink')
hist(male, breaks = interval_male ,freq=T,col='blue')