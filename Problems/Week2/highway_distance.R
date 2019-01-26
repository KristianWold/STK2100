#load data from web
auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)
summary(auto)
#import variables with correct name
attach(auto)
n <- nrow(auto)
d <- fuel == "gas"
e <- n.cylinders == 2

fuel1 <- factor(fuel, levels = c("gas", "diesel"))
twoCyl <- e*1

fit1 <- lm(log(highway.distance) ~ log(engine.size) + log(curb.weight) + fuel1 + twoCyl 
           + log(HP))
print(summary(fit1))

r2.2 <- 1 - sum((highway.distance - exp(fitted(fit1)))^2) / ((n - 1)*var(highway.distance))
cat("R squared =", round(r2.2, digits = 3),"\n")

plot(engine.size, highway.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)")
points(engine.size, city.distance, col = 4, pch = 1)
plot(curb.weight, highway.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)")
points(curb.weight, city.distance, col = 4, pch = 1)
plot(curb.weight, highway.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)")
points(curb.weight, city.distance, col = 4, pch = 1)
