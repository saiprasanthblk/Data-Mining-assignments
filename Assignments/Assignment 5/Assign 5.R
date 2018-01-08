# chapter 7 lab

library (ISLR)
attach (Wage)

fit=lm(wage???poly(age ,4) ,data=Wage)
coef(summary (fit))

fit2=lm(wage???poly(age ,4, raw =T),data=Wage)
coef(summary (fit2))

agelims =range(age)
age.grid=seq (from=agelims [1], to=agelims [2])
preds=predict(fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

preds2 =predict (fit2 ,newdata =list(age=age.grid),se=TRUE)
max(abs(preds$fit - preds2$fit ))

fit.1= lm(wage???age ,data=Wage)
fit.2= lm(wage???poly(age ,2) ,data=Wage)
fit.3= lm(wage???poly(age ,3) ,data=Wage)
fit.4= lm(wage???poly(age ,4) ,data=Wage)
fit.5= lm(wage???poly(age ,5) ,data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)


fit=glm(I(wage >250)???poly(age ,4) ,data=Wage ,family =binomial )

preds=predict (fit ,newdata =list(age=age.grid),se=T)

preds=predict (fit ,newdata =list(age=age.grid),type= "response", se=T)


pfit=exp(preds$fit )/(1+ exp(preds$fit))
se.bands.logit = cbind(preds$fit +2*preds$se.fit , preds$fit -2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+ exp(se.bands.logit))



plot(age ,I(wage >250) ,xlim=agelims ,type ="n",ylim=c(0 ,.2) )
points(jitter (age), I((wage >250) /5) ,cex =.5, pch ="|",col =" darkgrey ")
lines(age.grid ,pfit ,lwd =2, col =" blue")
matlines(age.grid ,se.bands ,lwd =1, col =" blue",lty =3)



table(cut (age ,4))


fit=lm(wage???cut (age ,4) ,data=Wage)
coef(summary (fit))


library(splines)
fit=lm(wage???bs(age ,knots =c(25 ,40 ,60) ),data=Wage)
pred=predict (fit ,newdata =list(age =age.grid),se=T)
plot(age ,wage ,col =" gray ")
lines(age.grid ,pred$fit ,lwd =2)
lines(age.grid ,pred$fit + 2*pred$se ,lty = "dashed")
lines(age.grid ,pred$fit - 2*pred$se ,lty = "dashed")


dim(bs(age ,knots=c(25 ,40 ,60)))
dim(bs(age ,df=6))
attr(bs(age ,df=6) ,"knots")

fit2=lm(wage???ns(age ,df =4) ,data=Wage)
pred2=predict (fit2 ,newdata =list(age=age.grid),se=T)
lines(age.grid , pred2$fit ,col ="red",lwd =2)

plot(age ,wage ,xlim=agelims ,cex =.5, col =" darkgrey ")
title("Smoothing Spline")
fit=smooth.spline (age ,wage ,df =16) # defining 16 degrees of freedom
fit2=smooth.spline (age ,wage ,cv=TRUE)
fit2$df
lines(fit ,col ="red ",lwd =2)

lines(fit2 ,col =" blue",lwd =2)
legend("topright",legend =c("16 DF " ,"6.8 DF"), col=c("red "," blue "), lty =1, lwd =2, cex =.8)


plot(age ,wage ,xlim=agelims ,cex =.5, col =" darkgrey ")
title (" Local Regression ")
fit=loess (wage???age ,span =.2, data=Wage)
fit2=loess(wage???age ,span =.5, data=Wage)
lines(age.grid ,predict (fit ,data.frame(age=age.grid)),col ="red ",lwd =2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),col =" blue",lwd =2)
legend("topright",legend =c("Span =0.2"," Span =0.5") ,col=c("red","blue"),lty =1, lwd =2, cex =.8)


gam1=lm(wage???ns(year ,4)+ns(age ,5) +education ,data=Wage)

install.packages("gam")
library(gam)

#gam.lo=gam(wage???s(year ,df=4)+lo(age ,span =0.7)+education , data=Wage)
plot.gam(gam.lo , se=TRUE , col ="green ")




# exercises

# problem 3

x = -2:2
y = 1 + x + -2 * (x-1)^2 * I(x>1)
plot(x, y)


# between 1 and 2, the curve is linear: y = 1 + x
# when x > 1, the curve is y = 1 + x -2(x-1)^2

# problem 6

# part a

library(ISLR)
library(boot)
set.seed(1)
deltas <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  deltas[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(1:10, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20) # this will help me identify the right polynomial

# we see that 4 is optimal degree

fit1 <- lm(wage ~ age, data = Wage)
fit2 <- lm(wage ~ poly(age, 2), data = Wage)
fit3 <- lm(wage ~ poly(age, 3), data = Wage)
fit4 <- lm(wage ~ poly(age, 4), data = Wage)
fit5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit1, fit2, fit3, fit4, fit5)

# we see that the 4th degree one (or even the 3rd degree plynomial is significantly different from the first two and you don't get much value by adding further degrees


plot(wage ~ age, data = Wage, col = "darkgrey")
agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
fit <- lm(wage ~ poly(age, 3), data = Wage)
preds <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)


# the step function part

cvs <- rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage ~ age.cut, data = Wage)
  cvs[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
d.min <- which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)

# 8 cuts seem to be the best. There is continous big drop until 8


plot(wage ~ age, data = Wage, col = "darkgrey")
agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, data.frame(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

# problem 7

set.seed(1)
summary(Wage$maritl)

summary(Wage$jobclass)

par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

# married people earn more. Industry jobs less than information jobs

fit1 = gam(wage~lo(year, span=0.7) +s(age,5) + education, data=Wage)
fit2 = gam(wage~lo(year, span=0.7) +s(age,5) + education + jobclass, data=Wage)
fit3 = gam(wage~lo(year, span=0.7) +s(age,5) + education + maritl, data=Wage)
fit4 = gam(wage~lo(year, span=0.7) +s(age,5) + education + jobclass + maritl, data=Wage)
anova(fit1, fit2, fit3, fit4)

par(mfrow = c(3, 3))
plot(fit3, se = T, col = "blue")

# problem 9

# part a

library(MASS)
set.seed(1)
fit <- lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)

dislims <- range(Boston$dis)
dis.grid <- seq(from = dislims[1], to = dislims[2], by = 0.1)
preds <- predict(fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds, col = "red", lwd = 2)

# seems like a good fit

# part b

rss <- rep(NA, 10)
for (i in 1:10) {
  fit <- lm(nox ~ poly(dis, i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}
plot(1:10, rss, xlab = "Degree", ylab = "RSS", type = "l")

# it keeps on continously decreasing. but overfitting?

# part c

deltas <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(nox ~ poly(dis, i), data = Boston)
  deltas[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")

# degree of 4 minimizes the test MSE

# part d

plot(Boston$dis, Boston$nox)

# I think it kind of starts getting linear around 4. There are sizable points between 7 and 11 with different slope and very few points beyond 11

fit <- lm(nox ~ bs(dis, knots = c(4, 7, 11)), data = Boston)
summary(fit)

pred <- predict(fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds, col = "red", lwd = 2)


# part e

rss <- rep(NA, 16)
for (i in 3:16) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}
plot(3:16, rss[-c(1, 2)], xlab = "Degrees of freedom", ylab = "RSS", type = "l")


# RSS goes up slightly towards the end

# part f

cv <- rep(NA, 16)
for (i in 3:16) {
  fit <- glm(nox ~ bs(dis, df = i), data = Boston)
  cv[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}

plot(3:16, cv[-c(1, 2)], xlab = "Degrees of freedom", ylab = "Test MSE", type = "l")

# 10 degrees of freedom is the least










