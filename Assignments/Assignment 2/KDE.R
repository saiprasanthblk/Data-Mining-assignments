library(ggplot2)
library(grid)

# univariate case

# I am using gaussian distribution of K

K = function(x) {
  K = (1/sqrt(2*22/7))*exp(-0.5*x^2)
  return(K)
}

# Defining KDE function with certain sample and other conditions

sample = sample(1:13, 50, replace = TRUE)
xi = (1:13)
bandwidth = 1
x =5

# defining the summation function

summation = function(x) {
  M = data.frame(matrix(NA))
  for (p in 1:13) { 
    M[, p] = K((x-xi[p])/bandwidth)
  } 
  return(apply(M, 1, sum))
}


# Defining the KDE function

est.kde.univariate = function(x, sample, bandwidth) {
  est.kde.univariate =  (1/length(sample)*bandwidth)*summation(x)
  return(est.kde.univariate)
}


est.kde.univariate(6, sample(1:13, 50, replace = TRUE), 1)
# Yes. The functions are working as expected
    

# Generating plots and simulations

rt = function(bandwidth) {
  M = data.frame(matrix(NA))
  for (p in 1:13) {
    for (i in 1:13) { 
      M[i, p] = K((i-xi[p])/bandwidth)*(1/length(sample)*bandwidth)
    } 
  }      
    return(as.data.frame(cbind(1:13,apply(M, 1, sum), bandwidth)))
}

bandwidth = c(1,0.8,0.6,0.4,0.2)

summation_matrix = data.frame(NA)

merge = rbind(rt(bandwidth[1]), rt(bandwidth[2]), rt(bandwidth[3]), rt(bandwidth[4]), rt(bandwidth[5]))

plot1 <- hist(sample) # The histogram of our sample set

plot2 <- qplot(V1, V2, data = merge, colour = bandwidth) # the actual KDE plots





# Bivariate

# I will code it directly in short form

sample1 = sample(1:13, 50, replace = TRUE)
sample2 = sample(1:13, 50, replace = TRUE)

x1 = 3
x2 = 4

bandwidth = 1

corr = cov(sample1,sample2)/(sd(sample1)*sd(sample2))  

z = ((x1 - mean(sample1))^2/sd(sample1)^2) + ((x2 - mean(sample2))^2/sd(sample2)^2) - ((2*corr*(x1 - mean(sample1))*(x2 - mean(sample2)))/(sd(sample1)*sd(sample2)))

est.kde.bivariate = function(x1, x2, sample1, sample2, bandwidth) {
  est.kde.bivariate = (1/(bandwidth*2*3.14*sd(sample1)*sd(sample2)*sqrt(1 - (corr)^2)))*(exp(-z/(bandwidth*2*(1 - (corr)^2))))
  return(est.kde.bivariate)
  }

est.kde.bivariate(3,4, sample(1:13, 50, replace = TRUE), sample(1:13, 50, replace = TRUE), 1)

# The function works as expected

M = data.frame(NA)

for (i in 1:13) {
  for (j in 1:13) {
    M[i,j] = est.kde.bivariate(i,j,sample(1:13, 50, replace = TRUE), sample(1:13, 50, replace = TRUE),bandwidth)
  }
}
}