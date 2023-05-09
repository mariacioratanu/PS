# Definim functia parabolei
y <- function(x) -2*x^2 + 5*x - 2

# Generam o grila de puncte de x intre 0 si 2 pentru a trasa graficul functiei parabolei
x_grila <- seq(0, 2, length.out = 1000)

# Calculam valorile corespunzatoare ale functiei parabolei pentru fiecare punct de pe grila de x
y_grila <- y(x_grila)

# Desenam graficul functiei parabolei
plot(x_grila, y_grila, type = "l", xlab = "x", ylab = "y", main = "Graficul functiei parabolei", col = "purple", lwd = 3)

# Definim numarul de valori uniforme
n <- 10000

# Generam n valori uniforme pentru x intre 0 si 2
x_vals <- runif(n, 0, 2)

# Calculam valorile corespunzatoare ale parabolei pentru fiecare valoare de x generata
y_vals <- y(x_vals)

# Determinam cate dintre aceste valori se afla deasupra axei Ox
y_above_zero <- y_vals > 0

# Estimam aria ca produs intre fractia de valori deasupra axei Ox si aria dreptunghiului acoperitor
area_estimate <- mean(y_above_zero) * 2 * 2

# Determinam aria exacta prin integrare
integrala_exacta <- integrate(y, 0, 2)$value

# Calculam eroarea relativa
eroare_relativa <- abs(area_estimate - integrala_exacta) / integrala_exacta

#Afisam rezultatele in consola
cat("Aria estimată este:", area_estimate, "\n")
cat("Aria exactă este:", integrala_exacta, "\n")
cat("Eroarea relativă este:", eroare_relativa)
