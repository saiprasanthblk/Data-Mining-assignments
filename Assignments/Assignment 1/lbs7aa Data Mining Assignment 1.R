# Lab

x <- c(1,3,2,5)

x # 1 3 2 5

# we ca use = instea of <-

x = c(1,6,2)
x
# 1 6 2
y = c(1,4,3)

x + y

# 2 10 5

 ls()
# "x" "y"
rm(x,y)
ls()
# character (0)

x=matrix (data=c(1,2,3,4) , nrow=2, ncol =2)

x

#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

x=matrix (c(1,2,3,4) ,2,2)

matrix (c(1,2,3,4) ,2,2,byrow =TRUE)

#[,1] [,2]
#[1,] 1 2
#[2,] 3 4

x=rnorm (50)
y=x+rnorm (50, mean=50, sd=.1)
cor(x,y)

# I got 0.9962. Random numbers!

set.seed (3)
y=rnorm (100)
mean(y)

var(y)

sd(y)

x=rnorm (100)
y=rnorm (100)
plot(x,y)

plot(x,y,xlab=" this is the x-axis",ylab=" this is the y-axis", main=" Plot of X vs Y")

pdf (" Figure .pdf ")
plot(x,y,col =" green ")
dev.off ()

x=seq (1 ,10)

x=seq(-pi ,pi ,length =50)

x

# I got a distribution of 50 equally distributed numbers between -3.14 and 3.14

y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)

A=matrix (1:16 ,4 ,4)

A[2,3]

A[c(1,3) ,c(2,4)]

A[1,]

# [1] 1 5 9 13

A[-c(1,3) ,]

dim(A)

# [1] 4 4

#yeah. I got the dimension of the matrix

Auto=read.csv (" Auto.csv", header =T,na.strings ="?")
fix(Auto)
dim(Auto)

# [1] 397 9

Auto=na.omit(Auto)
dim(Auto)

# 5 rows with NA's have been removed

names(Auto)


plot(Auto$cylinders , Auto$mpg )
attach (Auto)
plot(cylinders , mpg)

# Why can't we just use gg plot?

cylinders =as.factor (cylinders )

# Customizing our base R plots

plot(cylinders , mpg)
plot(cylinders , mpg , col ="red ")
plot(cylinders , mpg , col ="red", varwidth =T)
plot(cylinders , mpg , col ="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ", ylab ="MPG ")

# Histogram plots

hist(mpg)
hist(mpg ,col =2)
hist(mpg ,col =2, breaks =15)

pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)

#pairs function pairs the variables and generates distribution of each of the


plot(horsepower ,mpg)
identify (horsepower ,mpg ,name)

# But why would I use identify? I can just open the data frame or write out files. We are plotting the same points anyway

summary (Auto)

summary (mpg)

# I got the quartiles


