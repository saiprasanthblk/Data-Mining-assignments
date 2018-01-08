install.packages('ISLR')

library (ISLR)
set.seed (1)
train=sample (392 ,196)

lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train )

attach (Auto)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)

lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
# 19.82
lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)
# 19.78



set.seed (2)
train=sample (392 ,196)
lm.fit =lm(mpg~horsepower ,subset =train)

mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
# 23.30
lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
# 18.90
lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)
# 19.26



glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)
# (Intercept ) horsepower
# 39.936        -0.158


lm.fit =lm(mpg~horsepower ,data=Auto)
coef(lm.fit)
# (Intercept )   horsepower
# 39.936           -0.158


library (boot)
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err =cv.glm(Auto ,glm.fit)
cv.err$delta
#  1        1
# 24.23  24.23




cv.error=rep (0,5)
for (i in 1:5) {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm (Auto,glm.fit)$delta[1]
}
cv.error
# 24.23 19.25 19.33 19.42 19.03



set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm (Auto,glm.fit,K=10)$delta[1]
  }
cv.error.10

# 24.21 19.19 19.31 19.34 18.88 19.02 18.90 19.71 18.95 19.50





alpha.fn=function (data ,index){
  X=data$X[index]
  Y=data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}


alpha.fn(Portfolio ,1:100)

# 0.576


set.seed (1)
alpha.fn(Portfolio ,sample (100 ,100 , replace =T))
# 0.596


boot(Portfolio ,alpha.fn,R=1000)

# ORDINARY NONPARAMETRIC BOOTSTRAP
#Call:
boot(data = Portfolio , statistic = alpha.fn, R = 1000)

# Bootstrap Statistics :
#    original bias std . error
#t1* 0.5758 -7.315e -05 0.0886


boot.fn=function (data ,index )
  return (coef(lm(mpg~horsepower ,data=data ,subset=index)))
boot.fn(Auto ,1:392)
# (Intercept ) horsepower
#     39.936   -0.158


set.seed(1)
boot.fn(Auto ,sample (392 ,392 , replace =T))
#(Intercept ) horsepower
#38.739        -0.148
boot.fn(Auto ,sample (392 ,392 , replace =T))
# (Intercept ) horsepower
# 40.038        -0.160


boot(Auto ,boot.fn ,1000)
# ORDINARY NONPARAMETRIC BOOTSTRAP
#Call:
#  boot(data = Auto , statistic = boot.fn, R = 1000)
# Bootstrap Statistics :
#  original bias std. error
#t1* 39.936 0.0297 0.8600
#t2* -0.158 -0.0003 0.0074
  
summary (lm(mpg~horsepower ,data=Auto))$coef

# Estimate Std. Error t value Pr(>|t|)
# (Intercept ) 39.936 0.71750 55.7 1.22e-187
# horsepower -0.158 0.00645 -24.5 7.03e-81

boot.fn=function (data ,index )
  coefficients(lm(mpg~horsepower +I( horsepower ^2) ,data=data ,
                    subset =index))

set.seed (1)
boot(Auto ,boot.fn ,1000)

#Bootstrap Statistics :
#  original        bias     std. error
#t1* 56.900099702  6.098115e-03 2.0944855842
#t2* -0.466189630 -1.777108e-04 0.0334123802
#t3*  0.001230536  1.324315e-06 0.0001208339


summary (lm(mpg~horsepower +I(horsepower ^2) ,data=Auto))$coef
#Estimate Std. Error t value Pr(>|t|)
#(Intercept ) 56.9001 1.80043 32 1.7e-109
#horsepower -0.4662 0.03112 -15 2.3e-40
#I(horsepower ^2) 0.0012 0.00012 10 2.2e-21



