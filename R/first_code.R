source('./R/basicglm')
nitrates <- read.table('./Nitrates', header = T)
head(nitrates)
county <- kronecker(diag(3), rep(1,25))
head(county)
nit <- nitrates[,2]
head(nit)
plot(nit, pch = 17, col = nitrates[,1])

model <- basicglm(county, nit, 1, 5)


qnorm(.05/6)
4.19159 - 2.39398*sqrt(0.2378926)
4.19159 + 2.39398*sqrt(0.2378926)

4.577402 + 2.39398*sqrt(0.2837014)
4.577402 - 2.39398*sqrt(0.2837014)

4.242913 + 2.39398*sqrt(0.2437539)
4.242913 - 2.39398*sqrt(0.2437539)

r <- model$vals$stdevres
aggregate(r, by = list(nitrates[,1]), median) -> sum_stat
kronecker(as.matrix(sum_stat[,2]),rep(1,25)) -> jj
leven <- abs(r - jj)
summary(aov(leven ~ as.factor(nitrates[,1])))

plot(nit, pch = 17, col = nitrates[,1])
lines(1:75, model$vals$muhat, lty = 1)

x <- seq(.2, 12, length = 300)
y_1 <- dgamma(x, 4.2429, rate = .6962617)
y_2 <- dgamma(x, 4.577402, rate = .6453831)
y_3 <- dgamma(x, 4.19159, rate = .704787)

plot(x,y_1, type ="l", lty = 1, col = "black", lwd = 3)
lines(x, y_2, type = "l", lty = 2, col = 2, lwd = 3)
lines(x, y_3, type = "l", lty = 3, col = 3, lwd = 3)
legend(9, .15, legend = c("County 1", "County 2", "County 3"), col = 1:3, lty=1:3, cex = .5)
