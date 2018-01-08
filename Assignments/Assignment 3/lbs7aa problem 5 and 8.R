# Applied

# Problem 5

# part A
library(ISLR)
attach(Default)
set.seed(1)
model1.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(model1.glm)

#part B
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
model2.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(model2.glm)

probability <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probability))
pred.glm[probability > 0.5] <- "Yes"

mean(pred.glm != Default[-train, ]$default)

# 2.36%

# part C

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

# 0.028

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

# 0.0268

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

# 0.252

# This goes back to the high variability aspect. The MSE depends on which test set we are actually considering

# part D

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
model3.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
pred.glm <- rep("No", length(probability))
probability <- predict(model3.glm, newdata = Default[-train, ], type = "response")
pred.glm[probability > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

# 0.0246

# The student variable doesn't have significant impact on the MSE


# Problem 8

# part A

set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

# n is 100 and p is 2

# part B

plot(x, y)

# parabolic relationship

# Part c

library(boot)
set.seed(1)
Data <- data.frame(x, y)
model1.glm <- glm(y ~ x)
cv.glm(Data, model1.glm)$delta[1]

# 5.89

model2.glm <- glm(y ~ poly(x, 2))
cv.glm(Data, model2.glm)$delta[1]

# 1.08

model3.glm <- glm(y ~ poly(x, 3))
cv.glm(Data, model3.glm)$delta[1]

# 1.1

model4.glm <- glm(y ~ poly(x, 4))
cv.glm(Data, model4.glm)$delta[1]

# 1.1

# As the graphs indicate, the relaionship is more quadratic

# Part D

set.seed(10)
model1.glm <- glm(y ~ x)
cv.glm(Data, model1.glm)$delta[1]

# 5.89

model2.glm <- glm(y ~ poly(x, 2))
cv.glm(Data, model2.glm)$delta[1]

# 1.08

model3.glm <- glm(y ~ poly(x, 3))
cv.glm(Data, model3.glm)$delta[1]

# 1.1

model4.glm <- glm(y ~ poly(x, 4))
cv.glm(Data, model4.glm)$delta[1]

# 1.11

# LOOCV is a special case of K fold afterall. The answer are very similar in both cases

# part E

# the quadratic one. It has the least MSE K fold CV simulations as well

# part F

summary(model2.glm) # Lienar and quadratic are significant
summary(model3.glm) # the third exponential is not significant
summary(model4.glm) # This model too indicates the 3rd and 4th exponential are not significant

# The AIC is actually going up for the 4th model