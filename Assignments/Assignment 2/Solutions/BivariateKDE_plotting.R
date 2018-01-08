library(MASS)
library(RColorBrewer)

# get color palette for heat map
palette.function = colorRampPalette(rev(brewer.pal(11,'Spectral')))
heat.colors = palette.function(32)

# generate first bivariate sample
cov1 = matrix(c(10,5,5,10),2,2)
data1 = mvrnorm(n=1000, c(0,0), cov1)
plot(data1, xlab = "X", ylab = "Y")

# generate second bivariate sample
cov2 = matrix(c(10,-5,-5,10),2,2)
data2 = mvrnorm(n=1000, c(0,20), cov2)
plot(data2, xlab = "X", ylab = "Y")

# combine samples
combined.data = rbind(data1, data2)
plot(combined.data, xlab = "X", ylab = "Y")

# get and display bivariate KDE of combined data with different bandwidths
est = kde2d(combined.data[,1], combined.data[,2], h=0.1, n=c(100,100))  # h=0.1
image(est, col = heat.colors, useRaster=TRUE, asp=1)

est = kde2d(combined.data[,1], combined.data[,2], h=1, n=c(100,100))  # h=1
image(est, col = heat.colors, useRaster=TRUE, asp=1)

est = kde2d(combined.data[,1], combined.data[,2], h=10, n=c(100,100))  # h=10
image(est, col = heat.colors, useRaster=TRUE, asp=1)

est = kde2d(combined.data[,1], combined.data[,2], h=100, n=c(100,100))  # h=100
image(est, col = heat.colors, useRaster=TRUE, asp=1)

# what happens if the first sample is larger than the second?
data2 = mvrnorm(n=500, c(0,20), cov2)  # resample second dataset with n=500 rather than n=1000
combined.data = rbind(data1, data2)    # recombine with first dataset
est = kde2d(combined.data[,1], combined.data[,2], n=c(100,100))
image(est, col = heat.colors, useRaster=TRUE, asp=1)
