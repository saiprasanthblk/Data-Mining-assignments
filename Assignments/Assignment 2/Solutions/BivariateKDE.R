library(rgl) # Import rgl package for 3D graphing

est.kde.bivariate = function(x, sample, bandwidth) {
  h <-  bandwidth # Assign bandwidth to h
  n <- dim(sample)[1] # Calculate sample size
  kde <- 1/(n*h)*sum(apply((-sweep(sample,2,x))/h,1,FUN = function(x) (1/(2*pi)*exp(-(x[1]^2+x[2]^2)/2)))) # Generate KDE estimate
  kde # Return KDE
}

# Generate sample data
bi_x <- c(rnorm(10,-1,1),rnorm(10,2,1))
bi_y <- c(rnorm(10,-1,1),rnorm(10,2,1))
sample <- as.matrix(expand.grid(bi_x,bi_y))

# Generate evenly spaced x* and y* to calculate KDE
x <- seq(-5,5,by=0.1) 
y <- seq(-5,5,by=0.1)
xy <- as.matrix(expand.grid(x,y))

#Run KDE function
z.01 <- apply(xy,1, FUN = est.kde.bivariate,sample,.01)  # bandwidth .01
z.1 <- apply(xy,1, FUN = est.kde.bivariate,sample,.1) # bandwidth .1
z1 <- apply(xy,1, FUN = est.kde.bivariate,sample,1) # bandwidth 1
z5 <- apply(xy,1, FUN = est.kde.bivariate,sample,5) # bandwidth 5

# Construct dataframe with coordinates for each KDE 
df.01 <- as.data.frame(cbind(xy,z.01))
df.1 <- as.data.frame(cbind(xy,z.1))
df1 <- as.data.frame(cbind(xy,z1))
df5 <- as.data.frame(cbind(xy,z5))

#Rename df columns
names(df.01) <- c('x','y','z') 
names(df.1) <- c('x','y','z') 
names(df1) <- c('x','y','z') 
names(df5) <- c('x','y','z') 

# Plot coordinates
plot3d(df.01, col='blue', size=2)
plot3d(df.1, col='red', size=2, add = TRUE, alpha = .5)
plot3d(df1, col='yellow', size=2, add = TRUE, alpha = .5)
plot3d(df5, col='green', size=2, add = TRUE, alpha = .25)


