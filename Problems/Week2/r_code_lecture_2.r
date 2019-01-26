# code based on the file "Code regarding section 2.1 (copyright 2003, 2004, 2012 A.Azzalini and B.Scarpa)"

auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)
summary(auto)
attach(auto)

# sample size

n <- nrow(auto)
# create a dummy variable for fuel: diesel = FALSE, gasoline = TRUE
d <- fuel == "gas"
e <- n.cylinders == 2

### figure 2.1 ###
pairs(auto[ , c("city.distance", "engine.size","n.cylinders","curb.weight")],     
      labels = c("City\ndistance", "Engine\nsize","Number of\ncylinders", "Curb\nweight"),
      col = ifelse(d, 'blue', 'red'), pch = ifelse(d, 1, 2), # 1 is a circle, 2 is a triangle
      cex = 10/sqrt(n))


### figure 2.2 ###
plot(engine.size, city.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)", xlim = c(1, 5.5))
points(engine.size[d], city.distance[d], col = 4, pch = 1)
points(engine.size[!d], city.distance[!d], col = 2, pch = 2)
legend('topright', pch = c(1, 2), col = c(4, 2),
       legend = c("Gasoline  ","Diesel"))


### additional figure ###
plot(engine.size, city.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)", xlim = c(1, 5.5))
points(engine.size[d], city.distance[d], col = 1, pch = 1)
points(engine.size[!d], city.distance[!d], col = 1, pch = 1)
fitO <- lm(city.distance ~ engine.size)
print(summary(fitO))
x <- (seq(min(engine.size), max(engine.size), length = 200))
x <- seq(1, 5.5, length = 200)
beta <- coef(fitO)
lines(x, beta[1] + beta[2]*x, col = 2, lty = 1, lwd = 2)


### additional figure 2  ###
plot(engine.size, city.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)", xlim = c(1, 5.5))
points(engine.size[d], city.distance[d], col = 1, pch = 1)
points(engine.size[!d], city.distance[!d], col = 1, pch = 1)
fitO2 <- lm(city.distance ~ engine.size + I(engine.size^2)  + I(engine.size^3))
print(summary(fitO2))
x <- (seq(min(engine.size), max(engine.size), length = 200))
x <- seq(1, 5.5, length = 200)
beta <- coef(fitO2)
lines(x, beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3, col = 2, lty = 1, lwd = 2)


### table 2.1 ###
fuel1 <- factor(fuel, levels = c("gas", "diesel"))
fit3 <- lm(city.distance ~ engine.size + I(engine.size^2) + I(engine.size^3) + fuel1)
print(summary(fit3))


### figure 2.3 ### 
plot(engine.size, city.distance, type = "n", ylab = "City distance",
     xlab="Engine size",  xlim = c(1, 7))
points(engine.size[d], (city.distance[d]), col = 4, pch = 1)
points(engine.size[!d], (city.distance[!d]), col = 2, pch = 2)
x <- (seq(min(engine.size), max(engine.size), length = 200))
#extrapoltate to 7
x <- seq(1, 7, length = 200)
beta <- coef(fit3)
lines(x, beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3, col = 4, lty = 2)
lines(x,  beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3 + beta[5], col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2), legend = c("Gasoline", "Diesel"))


### figure 2.4(a) ### 
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1) # not importnat, just graphic paraemters
plot(fit3, which = 1, sub.caption = "", add.smooth = FALSE )


### figure 2.4(b) ### 
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1) # again, just graphic paraemters
plot(fit3, which = 2, sub.caption = "")


### figure 2.5 ###
par(mar = c(3.5, 3.5, 1, 1) + 0.1)
#1 if two cylinders, zero otherwise
twoCyl = e*1
plot(engine.size, 1/(city.distance), type="n", ylab="Consumption", xlab="Engine size")
points(engine.size[d&!e], 1/(city.distance[d&!e]), col = 4, pch = 1)
points(engine.size[!d&!e], 1/(city.distance[!d&!e]), col = 2, pch = 2)
points(engine.size[e], 1/(city.distance[e]), col = 5, pch = 3)
#
fit2 <- lm(1/(city.distance) ~ engine.size + fuel1 + curb.weight + twoCyl)
print(summary(fit2))
beta <- coef(fit2)
abline(beta[1] + beta[3], beta[2], col = 2, lty = 2)
abline(beta[1], beta[2], col = 4, lty = 2)


### table 2.2 ###
print(summary(fit2))

### figure 2.6 ###
plot(engine.size, city.distance, type = "n", ylab = "City distance", xlab = "Engine size",
     xlim = c(1, 5.5))
points(engine.size[d], (city.distance[d]), col = 4, pch = 1)
points(engine.size[!d], (city.distance[!d]), col = 2, pch = 2)
#
x <- (seq(min(engine.size), max(engine.size), length = 200))
x <- seq(1,5.5, length = 200)
lines(x, 1/(beta[1] + beta[2]*x), col = 4, lty = 2)
lines(x, 1/(beta[1] + beta[3] + beta[2]*x), col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2), legend = c("Gasoline", "Diesel"))


### compute R^2 ###
r2.2 <- 1 - sum((city.distance - 1/(fitted(fit2)))^2) / ((n - 1)*var(city.distance))
cat("R squared =", round(r2.2, digits = 2),"\n")


### figure 2.7(a) ###
fit2a <- update(fit2, . ~ . + factor(n.cylinders == 2), data = auto)
fit2b <- update(fit2a, . ~ . + curb.weight, data = auto)
#
par(mar=c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit2, which = 1, sub.caption = "", add.smooth = FALSE)
par(mar=c(3.5, 3.5, 1, 1) + 0.1)

### figure 2.7(b) ###
par(mar=c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit2, which = 2, sub.caption = "")



### table 2.3 ###
fit1 <- lm(log(city.distance) ~ log(engine.size) + fuel1)
print(summary(fit1))

### figure 2.8(a) ###
par(mar = c(3.5, 3.5, 1, 1) + 0.1)
plot(log(engine.size), log(city.distance), type = "n",
     ylab = "log(city distance)", xlab="log(engine size)")
points(log(engine.size[d]), log(city.distance[d]), col = 4, pch = 1)
points(log(engine.size[!d]), log(city.distance[!d]), col = 2, pch = 2)
beta<- coef(fit1)
abline(beta[1:2], col = 4, lty = 2)
abline(beta[1] + beta[3], sum(beta[2]) , col = 2, lty = 2)



### figure 2.8(b) ###
plot(engine.size, city.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)", xlim = c(1, 5.5))
points(engine.size[d], city.distance[d], col = 4, pch = 1)
points(engine.size[!d], city.distance[!d], col = 2, pch = 2)
x <- log(seq(min(engine.size), max(engine.size), length = 200))
x <- log(seq(1, 5.5, length = 200))
beta <- coef(fit1)
lines(exp(x), exp(beta[1] + beta[2]*x), col = 4, lty = 2)
lines(exp(x), exp(beta[1] + beta[3] + beta[2]*x),  col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2),
       legend = c("Gasoline", "Diesel"))


### figure 2.9(a) ###
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit1, which = 1, sub.caption = "",  add.smooth = FALSE)


### figure 2.9(b) ###
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit1, which = 2, sub.caption = "")


### table 2.4 ###
r2.1 <- 1 - sum((city.distance - exp(fitted(fit1)))^2) /
  ((n - 1)*var(city.distance))
cat("R squared =", round(r2.1, 2), "\n")
I.D   <- factor(n.cylinders == 2, labels = c(">2", "=2"))
fit1a <- update(fit1, . ~. + I.D, data = auto)
fit1b <- update(fit1a, . ~. + log(curb.weight), data = auto)
fit1c <- update(fit1, . ~. + log(curb.weight), data = auto)

print(summary(fit1b))
r2.1b <-  1 - sum((city.distance - exp(fitted(fit1b)))^2)/
  (202*var(city.distance))
cat("R squared = ", round(r2.1b, 2), "\n")


### figure 2.10(a) ###
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit1b, 1, sub.caption = "", add.smooth = FALSE)


### figure 2.10(b) ###
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit1b, 2, sub.caption = "")


### figure 2.10(c) ###
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit1b, 3, sub.caption = "",  add.smooth = FALSE)


### figure 2.10(d) ###
par(mar = c(3.5, 3.5, 1.2, 1) + 0.1)
plot(fit1b, 4, sub.caption = "")

detach(auto)
