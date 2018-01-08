#lab

install.packages("tree")

library(tree)

library (ISLR)
attach (Carseats )
High=ifelse (Sales <=8," No"," Yes ")

Carseats =data.frame(Carseats ,High)

tree.carseats =tree(High???. -Sales ,Carseats)

summary (tree.carseats )

tree.carseats

set.seed(2)
train=sample (1: nrow(Carseats ), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train ]
tree.carseats =tree(High???.-Sales ,Carseats ,subset =train )
tree.pred=predict (tree.carseats ,Carseats.test ,type ="class")
table(tree.pred ,High.test)

# 71.5% 

set.seed(3)
cv.carseats = cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats)

cv.carseats

par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")


prune.carseats =prune.misclass (tree.carseats ,best =9)
plot(prune.carseats )
text(prune.carseats ,pretty =0)


tree.pred=predict (prune.carseats , Carseats .test ,type=" class ")
table(tree.pred ,High.test)

#0.77



library (MASS)
set.seed (1)
train = sample (1: nrow(Boston ), nrow(Boston )/2)
tree.boston =tree(medv???.,Boston ,subset =train)
summary (tree.boston)

plot(tree.boston )
text(tree.boston ,pretty =0)

cv.boston =cv.tree(tree.boston )
plot(cv.boston$size ,cv.boston$dev)


prune.boston =prune.tree(tree.boston ,best =5)
plot(prune.boston)
text(prune.boston,pretty =0)

yhat=predict (tree.boston ,newdata =Boston [-train ,])
boston.test=Boston[-train,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)



install.packages("randomForest")
library (randomForest)
set.seed (1)
bag.boston =randomForest(medv???.,data=Boston ,subset =train ,
                           mtry=13, importance =TRUE)
bag.boston


yhat.bag = predict (bag.boston ,newdata =Boston [-train ,])
plot(yhat.bag , boston.test)
abline (0,1)
mean((yhat.bag -boston.test)^2)



bag.boston =randomForest(medv???.,data=Boston ,subset =train ,
                           mtry=13, ntree =25)
yhat.bag = predict (bag.boston ,newdata =Boston[-train ,])
mean(( yhat.bag -boston.test)^2)


set.seed (1)
rf.boston =randomForest(medv???.,data=Boston ,subset =train ,
                          mtry=6, importance =TRUE)
yhat.rf = predict (rf.boston ,newdata =Boston[-train ,])
mean((yhat.rf -boston.test)^2)


importance (rf.boston)

varImpPlot(rf.boston)

install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston =gbm(medv???.,data=Boston[train,], distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4)


summary(boost.boston)

par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i=" lstat ")

yhat.boost=predict(boost.boston ,newdata =Boston[-train ,],
                      n.trees =5000)
mean(( yhat.boost -boston.test)^2)



boost.boston =gbm(medv???.,data=Boston [train,], distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,
                    verbose =F)
yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],
                      n.trees =5000)
mean((yhat.boost -boston.test)^2)







#Problems---------------------


#problem 1


# problem 3

p <- seq(0, 1, 0.01)
gini.index <- 2 * p * (1 - p)
class.error <- 1 - pmax(p, 1 - p)
cross.entropy <- - (p * log(p) + (1 - p) * log(1 - p))
matplot(p, cbind(gini.index, class.error, cross.entropy), col = c("red", "green", "blue"))

# problem 4

par(xpd = NA)
plot(NA, NA, type = "n", xlim = c(-2, 2), ylim = c(-3, 3), xlab = "X1", ylab = "X2")
# X2 < 1
lines(x = c(-2, 2), y = c(1, 1))
# X1 < 1 with X2 < 1
lines(x = c(1, 1), y = c(-3, 1))
text(x = (-2 + 1)/2, y = -1, labels = c(-1.8))
text(x = 1.5, y = -1, labels = c(0.63))
# X2 < 2 with X2 >= 1
lines(x = c(-2, 2), y = c(2, 2))
text(x = 0, y = 2.5, labels = c(2.49))
# X1 < 0 with X2<2 and X2>=1
lines(x = c(0, 0), y = c(1, 2))
text(x = -1, y = 1.5, labels = c(-1.06))
text(x = 1, y = 1.5, labels = c(0.21))



# problem 5
# majority vote will tell us x is red since 6 values are more than 0.5
# probability will tell us X is green since p avg is 0.45


# problem 6

# we find the split that minizes RSS to the largest extent
# the same is applied multiple times until there are few observations in each bucket
# we have alpha which measures cost complexity
# optimal alpha is established in a way that minimizes the (y - yi)^2 + alpha*T
# higher alpha - lesser tree length
# we perform cv to find the optimal T


#problem 7

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
Boston.train <- Boston[train, -14]
Boston.test <- Boston[-train, -14]
Y.train <- Boston[train, 14]
Y.test <- Boston[-train, 14]
rf.boston1 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = ncol(Boston) - 1, ntree = 500)
rf.boston2 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = (ncol(Boston) - 1) / 2, ntree = 500)
rf.boston3 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = sqrt(ncol(Boston) - 1), ntree = 500)
plot(1:500, rf.boston1$test$mse, col = "green", type = "l", xlab = "Number of Trees", ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston3$test$mse, col = "blue", type = "l")
legend("topright", c("m = p", "m = p/2", "m = sqrt(p)"), col = c("green", "red", "blue"), cex = 1, lty = 1)

#problem 8

# part a

library(ISLR)
set.seed(1)
train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
Carseats.train <- Carseats[train, ]
Carseats.test <- Carseats[-train, ]

# part b

tree.carseats <- tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

pred <- predict(tree.carseats, newdata = Carseats.test)
mean((pred - Carseats.test$Sales)^2)

#part c

cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min <- which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)

prune.carseats <- prune.tree(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

yhat <- predict(prune.carseats, newdata = Carseats.test)
mean((yhat - Carseats.test$Sales)^2)

# part d

bag.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
yhat.bag <- predict(bag.carseats, newdata = Carseats.test)
mean((yhat.bag - Carseats.test$Sales)^2)

importance(bag.carseats)

# part e

rf.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 3, ntree = 500, importance = TRUE)
yhat.rf <- predict(rf.carseats, newdata = Carseats.test)
mean((yhat.rf - Carseats.test$Sales)^2)


importance(rf.carseats)

# question 9

# part a

set.seed(1)
train <- sample(1:nrow(OJ), 800)
OJ.train <- OJ[train, ]
OJ.test <- OJ[-train, ]


# part b

tree.oj <- tree(Purchase ~ ., data = OJ.train)
summary(tree.oj)

# part c

tree.oj

# part d

plot(tree.oj)
text(tree.oj, pretty = 0)

# part e

tree.pred <- predict(tree.oj, OJ.test, type = "class")
table(tree.pred, OJ.test$Purchase)

# part f

cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
cv.oj

# part g

plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree size", ylab = "Deviance")

# part h

# the two node tree

# part i

prune.oj <- prune.misclass(tree.oj, best = 2)
plot(prune.oj)
text(prune.oj, pretty = 0)

# part j

summary(tree.oj)

summary(prune.oj)

# pruned tree doesn't do better

# part k

prune.pred <- predict(prune.oj, OJ.test, type = "class")
table(prune.pred, OJ.test$Purchase)



#problem 10

# part a

Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)

# part b

train <- 1:200
Hitters.train <- Hitters[train, ]
Hitters.test <- Hitters[-train, ]

# part c

library(gbm)
set.seed(1)
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
train.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train <- predict(boost.hitters, Hitters.train, n.trees = 1000)
  train.err[i] <- mean((pred.train - Hitters.train$Salary)^2)
}
plot(lambdas, train.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")

# part d

set.seed(1)
test.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  yhat <- predict(boost.hitters, Hitters.test, n.trees = 1000)
  test.err[i] <- mean((yhat - Hitters.test$Salary)^2)
}
plot(lambdas, test.err, type = "b", xlab = "Shrinkage values", ylab = "Test MSE")

min(test.err)

lambdas[which.min(test.err)]

# part e

library(glmnet)

fit1 <- lm(Salary ~ ., data = Hitters.train)
pred1 <- predict(fit1, Hitters.test)
mean((pred1 - Hitters.test$Salary)^2)

fit1 <- lm(Salary ~ ., data = Hitters.train)
pred1 <- predict(fit1, Hitters.test)
mean((pred1 - Hitters.test$Salary)^2)

x <- model.matrix(Salary ~ ., data = Hitters.train)
x.test <- model.matrix(Salary ~ ., data = Hitters.test)
y <- Hitters.train$Salary
fit2 <- glmnet(x, y, alpha = 0)
pred2 <- predict(fit2, s = 0.01, newx = x.test)
mean((pred2 - Hitters.test$Salary)^2)


# part f

library(gbm)

boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[which.min(test.err)])
summary(boost.hitters)

# part g

set.seed(1)
bag.hitters <- randomForest(Salary ~ ., data = Hitters.train, mtry = 19, ntree = 500)
yhat.bag <- predict(bag.hitters, newdata = Hitters.test)
mean((yhat.bag - Hitters.test$Salary)^2)




# problem 12

####################  Trees  ####################

# what percentage of target column is 1?

nrow(subset(train, train$target == 1))*100/nrow(train)

# just 3.64%. We have a class imbalance problem in this data set. Need to think about that

# decision trees

library(tree)

train2 = train

train2[c("ps_car_11_cat")] <- list(NULL) # I am removing the columns that it is a categorical variable but has 104 unique values

train2$target = as.factor(train2$target)

# this is a classification problem

tree.porto =tree(target ~. ,train2)

summary(tree.porto)

tree.porto

# residual mean deviance = 0.0313

# importantly, misclassification = 0.03645

test = train2[sample(nrow(train2), 10000), ]

tree.pred =predict(tree.porto,test,type ="class")
table(tree.pred,test$target)

#tree.pred   0    1
#0         9638  362
#1           0    0

library(rpart)
prob <- predict(tree.porto, test, type = "prob")


# lets do cross validation to find optimal value

cv.porto <- cv.tree(tree.porto, FUN=prune.misclass)
plot(cv.porto$size, cv.porto$dev, type = "b")
tree.min <- which.min(cv.porto$dev)
points(tree.min, cv.porto$dev[tree.min], col = "red", cex = 2, pch = 20)

# what does it mean it cannot prune further. Let's see the tree

plot(tree.porto)
text(tree.porto, pretty = 0)





# lets explore some more trees based approaches. we will see what turns up





# bagging approach

library(randomForest)

train3 <- train2[sample(nrow(train2), 10000), ]

test = train2[sample(nrow(train2), 10000), ]

summary(train3$target)

# 3.66% are 1's. This is pretty close to real value

bag.porto <- randomForest(target ~ ., data = train3, mtry = 55, ntree = 5, importance = TRUE)
yhat.bag <- predict(bag.porto, newdata = test, type = "class")

summary(yhat.bag)
table(yhat.bag,test$target)

prob <- predict(bag.porto, test, type = "prob")

# there's a key learning here. Classification trees run much faster than regression trees. Tried that

#yhat.bag   0       1
#   0       9608    353
#   1       39      8 


# random forest

train4 <- train2[sample(nrow(train2), 10000), ]
test = train2[sample(nrow(train2), 10000), ]

rf.porto <- randomForest(target ~ ., data = train4, mtry = 8, ntree = 5, importance = TRUE)
yhat.bag <- predict(rf.porto, newdata = test, type = "class")

# mtry is square root of total predictors

summary(yhat.bag)
table(yhat.bag,test$target)

#yhat.bag   0     1
#0         9607   360
#1          29    4

prob <- predict(rf.porto, test, type = "prob")

importance(rf.porto)


#                         0           1 MeanDecreaseAccuracy MeanDecreaseGini
# id             -0.04025452  1.19480792           0.22436613     38.435765253
# ps_ind_01       7.26756030  0.66030796           7.37068398     16.014471436
# ps_ind_02_cat   6.31924829 -2.76233898           5.73173411      7.723166354
# ps_ind_03       9.63014257 -0.80416356           9.39749901     22.728275983
# ps_ind_04_cat   3.02688606 -1.15456769           2.76923899      2.717580042
# ps_ind_05_cat   3.40735190  5.59960499           4.80798570     12.870403906
# ps_ind_06_bin   6.32610310 -4.30309718           5.68893905      4.070708102
# ps_ind_07_bin   9.10699732  0.43839186           9.01106816      4.616474510
# ps_ind_08_bin  -0.16343486 -0.47308637          -0.24338395      3.729193202
# ps_ind_09_bin   2.23581646  0.22723215           2.27478513      3.424232808
# ps_ind_10_bin   0.00000000  0.00000000           0.00000000      0.006555556
# ps_ind_11_bin  -0.63149883  0.50181888          -0.53881494      0.900562965
# ps_ind_12_bin  -1.29387359 -0.28742447          -1.35540318      0.675569327
# ps_ind_13_bin   0.00000000  0.00000000           0.00000000      0.011000000
# ps_ind_14      -0.46055574 -0.93651001          -0.64025888      1.151615790
# ps_ind_15       4.14353762 -2.98249073           3.56687577     20.458815383
# ps_ind_16_bin   3.32474858  1.04347526           3.37351544      4.683771938
# ps_ind_17_bin   3.32909503 -1.47988561           3.10815190      4.115044208
# ps_ind_18_bin   4.87978072 -1.94385470           4.55183159      3.765969895
# ps_reg_01       9.10131802 -2.13150926           8.68911012     14.916797865
# ps_reg_02      15.76111410 -4.58177899          15.28321769     21.291418343
# ps_reg_03      17.64536016 -4.24819971          17.21763925     35.080668913
# ps_car_01_cat  12.15826592 -1.09687902          11.82951055     21.893125697
# ps_car_02_cat   5.17487659 -0.84526378           5.05511422      1.823042205
# ps_car_04_cat  10.31848484 -4.44213631          10.37836365      7.594751139
# ps_car_06_cat  15.19963665 -3.16994795          14.75500687     34.450838803
# ps_car_07_cat   1.93152475  0.97869192           2.14254174      2.415850881
# ps_car_08_cat   5.36645517 -1.63867233           5.04314170      2.461433323
# ps_car_09_cat   9.55069867 -2.15774461           9.18570057      9.569265712
# ps_car_10_cat   0.86159299 -1.13184780           0.60131984      1.008233945
# ps_car_11       8.07042022 -1.54194628           7.98228545      7.825892097
# ps_car_12      15.69841377 -5.80065546          15.60270581     16.655136838
# ps_car_13      23.24586819 -6.19574103          23.08973305     36.670207999
# ps_car_14      17.23885918 -4.07272447          16.97557335     31.099228675
# ps_car_15      11.96761504 -2.70281046          11.70712394     17.871010417
# ps_calc_01      0.29694591  0.85880967           0.47588421     18.769930229
# ps_calc_02     -0.15760363  1.16557736           0.08235381     19.186725772
# ps_calc_03     -0.58138077 -0.74451936          -0.72507750     19.991917859
# ps_calc_04      1.48638950  0.65619288           1.58804445     15.620913856
# ps_calc_05     -0.68430568  0.31019346          -0.60348025     13.714182263
# ps_calc_06     -1.67403848  0.04657021          -1.61485861     16.520092623
# ps_calc_07      1.90385655  1.62223998           2.21127337     17.594644864
# ps_calc_08     -1.92376163  0.07354145          -1.87086006     19.185903123
# ps_calc_09      0.96613520 -0.32422003           0.88134313     16.891908493
# ps_calc_10      0.77287775  0.16943014           0.74451541     23.171962667
# ps_calc_11     -0.60163415  0.15572323          -0.54568160     22.328087578
# ps_calc_12      0.82113456  0.16485055           0.84045166     14.335544525
# ps_calc_13     -1.32710452  1.12377149          -1.06006525     20.847960793
# ps_calc_14     -0.97513747  0.10661703          -0.97331766     22.352432738
# ps_calc_15_bin -0.56437663  1.21231494          -0.35806428      4.597289599
# ps_calc_16_bin -1.30474076 -0.44964802          -1.33012676      5.060269362
# ps_calc_17_bin  0.18868541  1.93110915           0.65823878      5.496522677
# ps_calc_18_bin  2.29677307 -0.33518759           2.13992097      5.323885313
# ps_calc_19_bin -0.22784601 -0.05488170          -0.22386072      5.717105071
# ps_calc_20_bin  0.30370860  0.50430792           0.39440685      3.522452208


# Boosting

train5 <- train2[sample(nrow(train2), 10000), ]


library(gbm)
boost.porto =gbm(target~.,data=train5, distribution = "bernoulli", n.trees =500, interaction.depth = 10, shrinkage = 0.2, verbose = F)

# actually, in my boosting model, there are no predictors that had non zero influence!

#"A gradient boosted model with bernoulli loss function.
#500 iterations were performed.
#There were 55 predictors of which 0 had non-zero influence."

summary(boost.porto)

yhat.bag <- predict(boost.porto, newdata = test, n.trees = 50)

yhat.bag

prob <- predict(boost.porto, test, type = "prob")


summary(yhat.bag)
table(yhat.bag,test$target)

# Based on what our group has observed so far, logistic regression is better than tree based methods. With that being said, this data set may not be the ideal data set to test the effectiveness of tree based methods due to extremely high class imbalance.



















