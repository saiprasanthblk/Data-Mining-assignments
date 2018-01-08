set.seed (1)
x=matrix (rnorm (20*2) , ncol =2)
y=c(rep (-1,10) , rep (1 ,10) )
x[y==1 ,]= x[y==1,] + 1
plot(x, col =(3-y))

install.packages("e1071")

dat=data.frame(x=x, y=as.factor(y))

library (e1071)

svmfit =svm(y???., data=dat , kernel ="linear", cost =10,
              scale =FALSE )

plot(svmfit , dat)

svmfit$index

summary(svmfit)


svmfit =svm(y???., data=dat , kernel ="linear", cost =0.1,
            scale =FALSE )
plot(svmfit , dat)
svmfit$index


set.seed(1)
tune.out=tune(svm ,y???.,data=dat ,kernel ="linear",
                ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))

summary(tune.out)

bestmod =tune.out$best.model
summary(bestmod)


xtest=matrix (rnorm (20*2) , ncol =2)
ytest=sample (c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1
testdat =data.frame(x=xtest , y=as.factor(ytest))


ypred=predict(bestmod ,testdat )
table(predict =ypred, truth= testdat$y )

svmfit =svm(y???., data=dat , kernel ="linear", cost =.01,
            scale =FALSE )
ypred=predict (svmfit ,testdat )
table(predict =ypred , truth= testdat$y )

x[y==1 ,]= x[y==1 ,]+0.5
plot(x, col =(y+5) /2, pch =19)


dat=data.frame(x=x,y=as.factor(y))
svmfit =svm(y???., data=dat , kernel ="linear", cost =1e5)
summary(svmfit)


plot(svmfit , dat)


svmfit =svm(y???., data=dat , kernel ="linear", cost =1)
summary (svmfit)
plot(svmfit ,dat)


set.seed(1)
x=matrix (rnorm (200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2
x[101:150 ,]= x[101:150 ,] -2
y=c(rep(1 ,150) ,rep(2 ,50) )
dat=data.frame(x=x,y=as.factor(y))


train=sample(200 ,100)
svmfit =svm(y???., data=dat [train ,], kernel ="radial", gamma =1,
              cost =1)
plot(svmfit , dat[train ,])

summary(svmfit)


svmfit =svm(y???., data=dat [train ,], kernel ="radial",gamma =1,
            cost=1e5)
plot(svmfit ,dat [train ,])


set.seed (1)
tune.out=tune(svm , y???., data=dat[train ,], kernel ="radial",
                ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                             gamma=c(0.5,1,2,3,4) ))
summary(tune.out)


table(true=dat[-train ,"y"], pred=predict(tune.out$best.model,
                                             newdata =dat[-train,]))

install.packages("ROCR")

library(ROCR)

rocplot =function (pred , truth , ...){
  + predob = prediction (pred , truth )
  + perf = performance (predob , "tpr ", "fpr")
  + plot(perf ,...)}


svmfit.opt = svm(y???., data=dat[train,], kernel = "radial",
                gamma =2, cost=1, decision.values =T)

fitted =attributes(predict(svmfit.opt ,dat[train ,], decision.values =TRUE))$decision.values

par(mfrow =c(1,2))
rocplot(fitted, dat[train,"y"], main = "Training Data ")

require(ROCR)

svmfit.flex=svm(y???., data=dat[train ,], kernel ="radial",
                  gamma =50, cost=1, decision.values =T)
fitted =attributes(predict(svmfit.flex ,dat[train ,], decision.values =T))$decision.values
rocplot(fitted, dat[train,"y"], add =T, col ="red")


fitted =attributes (predict (svmfit .opt ,dat[-train ,], decision.values =T))$decision.values
rocplot(fitted ,dat [-train ,"y"], main ="Test Data")
fitted =attributes (predict (svmfit.flex ,dat[-train ,], decision.values =T))$decision.values
rocplot(fitted ,dat [-train ,"y"], add=T,col ="red")


set.seed(1)
x=rbind(x, matrix(rnorm (50*2) , ncol =2))
y=c(y, rep (0 ,50) )
x[y==0 ,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor (y))
par(mfrow =c(1,1))
plot(x,col =(y+1))


svmfit =svm(y???., data=dat, kernel ="radial", cost=10, gamma =1)
plot(svmfit , dat)


library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)

table(Khan$ytest)


dat=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain))
out=svm(y???., data=dat , kernel ="linear",cost =10)
summary(out)

dat.te=data.frame(x=Khan$xtest , y=as.factor (Khan$ytest ))
pred.te=predict (out , newdata =dat.te)
table(pred.te , dat.te$y)



# exercises

#1

x1 <- -10:10
x2 <- 1 + 3 * x1
plot(x1, x2, type = "l", col = "blue")
text(c(0), c(-20), "Greater than 0", col = "blue")
text(c(0), c(20), "Less than 0", col = "blue")
lines(x1, 1 - x1/2, col = "red")
text(c(0), c(-15), "Less than 0", col = "red")
text(c(0), c(15), "Greater than 0", col = "red")

# 2a

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

#b

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

#c

plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)


# d
# We could expand the equation and think of each term including the polynomial ones as linear

#3

#a

x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
colors = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))

#b

plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)

#c

# if X1 - X2 - 0.5 < 0, then red, otherwise blue

#d

plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

#e

#(2,1), (2,2), (4,3), (4,4) are the support vectors

#f

#(4,1) is not a support vector. Nothing will change

#g

plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.3, 1)

#h

plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
points(c(3), c(1), col = c("red"))

#4

library(e1071)
set.seed(1)
x <- rnorm(100)
y <- 4 * x^2 + 1 + rnorm(100)
class <- sample(100, 50)
y[class] <- y[class] + 3
y[-class] <- y[-class] - 3
plot(x[class], y[class], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30))
points(x[-class], y[-class], col = "blue")

z <- rep(-1, 100)
z[class] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))
train <- sample(100, 50)
data.train <- data[train, ]
data.test <- data[-train, ]
svm.linear <- svm(z ~ ., data = data.train, kernel = "linear", cost = 10)
plot(svm.linear, data.train)


table(predict = predict(svm.linear, data.train), truth = data.train$z)

svm.poly <- svm(z ~ ., data = data.train, kernel = "polynomial", cost = 10)
plot(svm.poly, data.train)

table(predict = predict(svm.poly, data.train), truth = data.train$z)

svm.radial <- svm(z ~ ., data = data.train, kernel = "radial", gamma = 1, cost = 10)
plot(svm.radial, data.train)

table(predict = predict(svm.radial, data.train), truth = data.train$z)

plot(svm.linear, data.test)

table(predict = predict(svm.linear, data.test), truth = data.test$z)

plot(svm.poly, data.test)

table(predict = predict(svm.poly, data.test), truth = data.test$z)

plot(svm.radial, data.test)

table(predict = predict(svm.radial, data.test), truth = data.test$z)


#5

#a

set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

#b

plot(x1, x2, xlab = "X1", ylab = "X2", col = (4 - y), pch = (3 - y))

#c

logit.fit <- glm(y ~ x1 + x2, family = "binomial")
summary(logit.fit)

#d

data <- data.frame(x1 = x1, x2 = x2, y = y)
probs <- predict(logit.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

#e

logitnl.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")

summary(logitnl.fit)

#f

probs <- predict(logitnl.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

#g

data$y <- as.factor(data$y)
svm.fit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds <- predict(svm.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

#h

data$y <- as.factor(data$y)
svmnl.fit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
preds <- predict(svmnl.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))


#i

# performance of non linear and linear appear to be same
# also, SVM with logistic regression and linear kernel does very badly without interaction term

#6

#a

set.seed(1)
x.one <- runif(500, 0, 90)
y.one <- runif(500, x.one + 10, 100)
x.one.noise <- runif(50, 20, 80)
y.one.noise <- 5/4 * (x.one.noise - 10) + 0.1

x.zero <- runif(500, 10, 100)
y.zero <- runif(500, 0, x.zero - 10)
x.zero.noise <- runif(50, 20, 80)
y.zero.noise <- 5/4 * (x.zero.noise - 10) - 0.1

class.one <- seq(1, 550)
x <- c(x.one, x.one.noise, x.zero, x.zero.noise)
y <- c(y.one, y.one.noise, y.zero, y.zero.noise)

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)


#b

set.seed(2)
z <- rep(0, 1100)
z[class.one] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))
tune.out <- tune(svm, z ~ ., data = data, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)

data.frame(cost = tune.out$performance$cost, misclass = tune.out$performance$error * 1100)

#c

x.test <- runif(1000, 0, 100)
class.one <- sample(1000, 500)
y.test <- rep(NA, 1000)
# Set y > x for class.one
for (i in class.one) {
  y.test[i] <- runif(1, x.test[i], 100)
}
# set y < x for class.zero
for (i in setdiff(1:1000, class.one)) {
  y.test[i] <- runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+")
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)



set.seed(3)
z.test <- rep(0, 1000)
z.test[class.one] <- 1
data.test <- data.frame(x = x.test, y = y.test, z = as.factor(z.test))
costs <- c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.err <- rep(NA, length(costs))
for (i in 1:length(costs)) {
  svm.fit <- svm(z ~ ., data = data, kernel = "linear", cost = costs[i])
  pred <- predict(svm.fit, data.test)
  test.err[i] <- sum(pred != data.test$z)
}
data.frame(cost = costs, misclass = test.err)


#d

#linear kernel tends to overfit. Large cost obviously overfits the data. 

#7

#a

library(ISLR)
var <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpglevel <- as.factor(var)

#b

set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

#c

set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(tune.out)

set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

#d

svm.linear <- svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly <- svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 100, degree = 2)
svm.radial <- svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 100, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm.linear)
































